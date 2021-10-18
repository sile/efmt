use crate::Result;
use erl_tokenize::tokens::{AtomToken, CommentToken, SymbolToken};
use erl_tokenize::values::Symbol;
use erl_tokenize::{LexicalToken, PositionRange, Token, Tokenizer};
use std::collections::BTreeMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Position(usize);

impl Position {
    pub const fn new(n: usize) -> Self {
        Self(n)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Region {
    pub start: Position,
    pub end: Position,
}

impl Region {
    pub const fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer<String>,
    comments: BTreeMap<Position, CommentToken>,
    current_position: Position,
}

impl Lexer {
    pub fn new(text: impl AsRef<str>) -> Self {
        Self {
            tokenizer: Tokenizer::new(text.as_ref().to_owned()),
            comments: BTreeMap::new(),
            current_position: Position::new(0),
        }
    }

    pub fn current_position(&self) -> Position {
        self.current_position
    }

    pub fn next_token(&mut self) -> Result<Option<LexicalToken>> {
        while let Some(token) = self.tokenizer.next().transpose()? {
            self.current_position = Position::new(token.end_position().offset());
            match token {
                Token::Comment(t) => {
                    self.comments
                        .insert(Position::new(t.start_position().offset()), t);
                }
                Token::Whitespace(_) => {}
                Token::Atom(t) => return Ok(Some(t.into())),
                Token::Char(t) => return Ok(Some(t.into())),
                Token::Float(t) => return Ok(Some(t.into())),
                Token::Integer(t) => return Ok(Some(t.into())),
                Token::Keyword(t) => return Ok(Some(t.into())),
                Token::String(t) => return Ok(Some(t.into())),
                Token::Symbol(t) => return Ok(Some(t.into())),
                Token::Variable(t) => return Ok(Some(t.into())),
            }
        }
        Ok(None)
    }

    pub fn expect_symbol_value(&mut self, expected: Symbol) -> Result<SymbolToken> {
        match self.next_token()? {
            Some(LexicalToken::Symbol(token)) => {
                anyhow::ensure!(
                    token.value() == expected,
                    "expected {:?}, but got {:?}",
                    expected,
                    token
                );
                Ok(token)
            }
            Some(token) => {
                anyhow::bail!("expected a symbol token, but got {:?}", token);
            }
            None => {
                anyhow::bail!("unexpected EOF");
            }
        }
    }

    pub fn expect_atom(&mut self) -> Result<AtomToken> {
        match self.next_token()? {
            Some(LexicalToken::Atom(token)) => Ok(token),
            Some(token) => {
                anyhow::bail!("expected a atom token, but got {:?}", token);
            }
            None => {
                anyhow::bail!("unexpected EOF");
            }
        }
    }

    pub fn expect_atom_value(&mut self, expected: &str) -> Result<AtomToken> {
        let token = self.expect_atom()?;
        anyhow::ensure!(
            token.value() == expected,
            "expected {:?}, but got {:?}",
            expected,
            token
        );
        Ok(token)
    }
}
