use crate::parse::Parser;
use crate::token::{CommentToken, LexicalToken, Symbol, Token, TokenPosition, TokenRegion};
use crate::tokenize::{self, Tokenizer};
use erl_tokenize::PositionRange as _;
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("cannot set the position {position:?} as it's invalid")]
    InvalidPosition { position: TokenPosition },

    #[error(transparent)]
    TokenizeError(#[from] tokenize::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer,
    tokens: Vec<LexicalToken>,
    current: usize,
    comments: BTreeMap<TokenPosition, CommentToken>,
    macro_calls: BTreeMap<TokenRegion, ()>, // TODO
}

impl Lexer {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            current: 0,
            comments: BTreeMap::new(),
            macro_calls: BTreeMap::new(),
        }
    }

    pub fn finish(self) -> LexedText {
        LexedText {
            original_text: self.tokenizer.finish(),
            tokens: self.tokens,
            comments: self.comments,
            macro_calls: self.macro_calls,
        }
    }

    pub fn read_token(&mut self) -> Result<Option<LexicalToken>> {
        if let Some(token) = self.tokens.get(self.current).cloned() {
            self.current += 1;
            return Ok(Some(token));
        }

        while let Some(token) = self.tokenizer.next().transpose()? {
            let token: LexicalToken = match token {
                Token::Whitespace(_) => {
                    continue;
                }
                Token::Comment(x) => {
                    self.comments
                        .insert(TokenPosition::new(None, x.start_position()), x);
                    continue;
                }
                Token::Symbol(x) => x.into(),
                Token::Atom(x) => x.into(),
                Token::Char(x) => x.into(),
                Token::Float(x) => x.into(),
                Token::Integer(x) => x.into(),
                Token::Keyword(x) => x.into(),
                Token::String(x) => x.into(),
                Token::Variable(x) => x.into(),
            };

            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
                    todo!();
                }
                _ => {}
            }

            self.tokens.push(token.clone());
            self.current += 1;

            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Hyphen => {
                    let index = self.current;
                    self.try_handle_directives()?;
                    self.current = index;
                }
                _ => {}
            }

            return Ok(Some(token));
        }

        Ok(None)
    }

    fn try_handle_directives(&mut self) -> Result<()> {
        let is_target = match self.read_token()? {
            Some(LexicalToken::Atom(x))
                if matches!(x.value(), "define" | "include" | "include_lib") =>
            {
                true
            }
            _ => false,
        };
        if !is_target {
            return Ok(());
        }

        self.current -= 2;
        let _parser = Parser::new(self);
        todo!();
    }

    pub fn current_position(&self) -> TokenPosition {
        let token_index = self.current;
        let text_position = self
            .tokens
            .get(self.current)
            .map(|x| x.start_position())
            .unwrap_or_else(|| self.tokenizer.next_position());
        TokenPosition::new(Some(token_index), text_position)
    }

    pub fn set_position(&mut self, position: TokenPosition) -> Result<()> {
        if let Some(index) = position.token_index() {
            if index <= self.tokens.len() {
                self.current = index;
                return Ok(());
            }
        }
        Err(Error::InvalidPosition { position })
    }
}

#[derive(Debug)]
pub struct LexedText {
    pub original_text: String,
    pub tokens: Vec<LexicalToken>,
    pub comments: BTreeMap<TokenPosition, CommentToken>,
    pub macro_calls: BTreeMap<TokenRegion, ()>, // TODO
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Context;

    #[test]
    fn lexer_works() -> anyhow::Result<()> {
        let testnames = ["nomacro", "macro"];
        for testname in testnames {
            let (before_path, before, after_path, after_expected) =
                crate::tests::load_testdata(&format!("lex/{}", testname))
                    .with_context(|| format!("[{}] cannot load testdata", testname))?;

            let tokenizer = Tokenizer::new(before);
            let mut lexer = Lexer::new(tokenizer);
            let mut tokens = Vec::new();
            while let Some(token) = lexer
                .read_token()
                .with_context(|| format!("[{}] cannot read token", testname))?
            {
                tokens.push(token);
            }

            let after_actual = tokens
                .iter()
                .map(|x| x.text())
                .collect::<Vec<_>>()
                .join(" ");
            anyhow::ensure!(
                after_actual == after_expected.trim(),
                "unexpected result.\n[ACTUAL] {}\n{}\n\n[EXPECTED] {}\n{}",
                before_path,
                after_actual,
                after_path,
                after_expected
            );
        }
        Ok(())
    }
}
