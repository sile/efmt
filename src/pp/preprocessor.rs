use crate::cst::attributes::{Attr, DefineAttr};
use crate::parse::{Parse, TokenReader};
use crate::pp::{MacroCall, Result};
use crate::token::{CommentToken, LexicalToken, Symbol, Token, TokenIndex, TokenRegion};
use erl_tokenize::{Position, PositionRange, Tokenizer};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug)]
pub struct Preprocessor {
    tokenizer: Tokenizer<String>,
    macro_defines: HashMap<String, DefineAttr>,
    preprocessed: PreprocessedText,
}

impl Preprocessor {
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        let preprocessed = PreprocessedText {
            text: tokenizer.text().to_owned(),
            tokens: Vec::new(),
            comments: BTreeMap::new(),
            macro_calls: BTreeMap::new(),
        };
        Self {
            tokenizer,
            macro_defines: HashMap::new(),
            preprocessed,
        }
    }

    pub fn preprocess(mut self) -> Result<PreprocessedText> {
        while let Some(token) = self.next_lexical_token()? {
            if let LexicalToken::Symbol(x) = &token {
                if x.value() == Symbol::Question {
                    let (tokens, macro_call) =
                        self.expand_macro(TokenIndex::new(self.preprocessed.tokens.len()))?;
                    let token_range = TokenRegion::new(
                        TokenIndex::new(self.preprocessed.tokens.len()),
                        TokenIndex::new(self.preprocessed.tokens.len() + tokens.len()),
                    )
                    .expect("unreachable");
                    self.preprocessed.tokens.extend(tokens);
                    self.preprocessed
                        .macro_calls
                        .insert(token_range.start(), macro_call);
                    continue;
                } else if x.value() == Symbol::Hyphen {
                    self.try_handle_directives(x.clone().into())?;
                    self.tokenizer.set_position(x.end_position());
                }
            }
            self.preprocessed.tokens.push(token);
        }
        Ok(self.preprocessed)
    }

    fn try_handle_directives(&mut self, hyphen: LexicalToken) -> Result<()> {
        let mut tokens = vec![hyphen];
        if let Some(LexicalToken::Atom(token)) = self.next_lexical_token()? {
            if matches!(token.value(), "define" | "include" | "include_lib") {
                tokens.push(token.into());
            } else {
                return Ok(());
            }
        }

        while let Some(token) = self.next_lexical_token()? {
            tokens.push(token.clone());
            if token
                .as_symbol_token()
                .map_or(false, |x| x.value() == Symbol::Dot)
            {
                let mut tokens = TokenReader::new(tokens);
                let attr = Attr::parse(&mut tokens)?;
                match attr {
                    Attr::Define(x) => {
                        self.macro_defines.insert(x.macro_name().to_owned(), x);
                    }
                }
                break;
            }
        }

        Ok(())
    }

    fn next_lexical_token(&mut self) -> Result<Option<LexicalToken>> {
        while let Some(token) = self.tokenizer.next().transpose()? {
            match token {
                Token::Whitespace(_) => {}
                Token::Comment(x) => {
                    // TODO: handle include
                    //if self.preprocessed.file.as_ref() == x.start_position().filepath() {
                    self.preprocessed.comments.insert(x.start_position(), x);
                    //}
                }
                Token::Symbol(x) => return Ok(Some(x.into())),
                Token::Atom(x) => return Ok(Some(x.into())),
                Token::Char(x) => return Ok(Some(x.into())),
                Token::Float(x) => return Ok(Some(x.into())),
                Token::Integer(x) => return Ok(Some(x.into())),
                Token::Keyword(x) => return Ok(Some(x.into())),
                Token::String(x) => return Ok(Some(x.into())),
                Token::Variable(x) => return Ok(Some(x.into())),
            }
        }
        Ok(None)
    }

    fn expand_macro(&mut self, _start: TokenIndex) -> Result<(Vec<LexicalToken>, MacroCall)> {
        // let position = self.tokenizer.next_position();
        // let name = match self.read_expect(Or(ExpectAtom, ExpectVariable))? {
        //     Either::A(x) => x.value().to_owned(),
        //     Either::B(x) => x.value().to_owned(),
        // };
        // let define =
        //     self.macro_defines
        //         .get(&name)
        //         .cloned()
        //         .ok_or_else(|| Error::UndefinedMacro {
        //             name: name.clone(),
        //             position: position.clone(),
        //         })?;

        // if let Some(params) = &define.params {
        //     let _ = self.read_expect(Symbol::OpenParen)?;
        //     let mut args = HashMap::new();
        //     let mut args_vec = Vec::new();
        //     for (i, param) in params.iter().enumerate() {
        //         let arg = self.parse_macro_arg()?;
        //         args_vec.push(arg.clone());
        //         args.insert(param.value(), arg);
        //         if i + 1 < params.len() {
        //             self.read_expect(Symbol::Comma)?;
        //         }
        //     }
        //     let macro_call = MacroCall {
        //         name,
        //         args: Some(args_vec),
        //     };
        //     Ok((define.replace_variables(&args), macro_call))
        // } else {
        //     let macro_call = MacroCall { name, args: None };
        //     Ok((define.replacement.clone(), macro_call))
        // }
        todo!()
    }

    // fn parse_macro_arg(&mut self) -> Result<Vec<LexicalToken>> {
    //     #[derive(Debug, Default)]
    //     struct Level {
    //         paren: usize,
    //         brace: usize,
    //         square: usize,
    //         block: usize,
    //         // TODO: '<<''>>'
    //     }

    //     impl Level {
    //         fn is_top_level(&self) -> bool {
    //             (self.paren + self.brace + self.square + self.block) == 0
    //         }
    //     }

    //     let mut level = Level::default();
    //     let mut tokens = Vec::new();
    //     while let Some(token) = self.next_lexical_token()? {
    //         let position = token.start_position();
    //         match &token {
    //             LexicalToken::Symbol(x) if x.value() == Symbol::Comma && level.is_top_level() => {
    //                 self.tokenizer.set_position(token.start_position());
    //                 break;
    //             }
    //             LexicalToken::Symbol(x)
    //                 if x.value() == Symbol::CloseParen && level.is_top_level() =>
    //             {
    //                 self.tokenizer.set_position(token.start_position());
    //                 break;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::Dot => {
    //                 // TODO: fix ('.' can be appeared in record access such as #foo.bar
    //                 return Err(Error::MalformedMacroArg { position });
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
    //                 level.paren += 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
    //                 if level.paren == 0 {
    //                     return Err(Error::MalformedMacroArg { position });
    //                 }
    //                 level.paren -= 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::OpenBrace => {
    //                 level.brace += 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::CloseBrace => {
    //                 if level.brace == 0 {
    //                     return Err(Error::MalformedMacroArg { position });
    //                 }
    //                 level.brace -= 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::OpenSquare => {
    //                 level.square += 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::CloseSquare => {
    //                 if level.square == 0 {
    //                     return Err(Error::MalformedMacroArg { position });
    //                 }
    //                 level.square -= 1;
    //             }
    //             LexicalToken::Keyword(x)
    //                 if x.value() == Keyword::Begin
    //                     && x.value() == Keyword::Try
    //                     && x.value() == Keyword::Fun
    //                     && x.value() == Keyword::Case
    //                     && x.value() == Keyword::If =>
    //             {
    //                 level.block += 1;
    //             }
    //             LexicalToken::Keyword(x) if x.value() == Keyword::End => {
    //                 if level.block == 0 {
    //                     return Err(Error::MalformedMacroArg { position });
    //                 }
    //                 level.block -= 1;
    //             }
    //             _ => {}
    //         }
    //         tokens.push(token);
    //     }

    //     Ok(tokens)
    // }
}

#[derive(Debug, Clone)]
pub struct PreprocessedText {
    pub text: String,
    pub tokens: Vec<LexicalToken>,
    pub comments: BTreeMap<Position, CommentToken>, // s/Position/Offset/
    pub macro_calls: BTreeMap<TokenIndex, MacroCall>,
}

impl PreprocessedText {
    // TODO: rename
    pub fn actual_position(&self, i: TokenIndex) -> Position {
        for (_, m) in self
            .macro_calls
            .range(..i)
            .rev()
            .take(1)
            .chain(self.macro_calls.range(i..).take(1))
        {
            if m.range.start() <= i && i < m.range.end() {
                return m.start_position.clone();
            }
        }
        self.tokens[i.get()].start_position()
    }
}

impl std::fmt::Display for PreprocessedText {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut last_line = 1;
        let mut first_column = true;
        for (i, token) in self.tokens.iter().enumerate() {
            let position = self.actual_position(TokenIndex::new(i));
            if last_line != position.line() {
                writeln!(f)?;
                if last_line + 1 < position.line() {
                    writeln!(f)?;
                }
                last_line = position.line();
                first_column = true;
                for _ in 1..position.column() {
                    write!(f, " ")?;
                }
            }
            if !first_column {
                write!(f, " ")?;
            }
            write!(f, "{}", token.text())?;
            first_column = false;
        }
        writeln!(f)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use anyhow::Context;
    use erl_tokenize::Tokenizer;

    #[test]
    fn preprocess_works() -> anyhow::Result<()> {
        let testnames = ["nomacro"];
        for testname in testnames {
            let (before, after_expected) = crate::tests::load_testdata(&format!("pp/{}", testname))
                .with_context(|| format!("[{}] cannot load testdata", testname))?;
            let pp = Preprocessor::new(Tokenizer::new(before));
            let preprocessed = pp
                .preprocess()
                .with_context(|| format!("[{}] cannot preprocess", testname))?;
            let after_actual = preprocessed.to_string();
            anyhow::ensure!(
                after_actual == after_expected,
                "unexpected preprocessed result.\n[ACTUAL]\n{}\n[EXPECTED]\n{}",
                after_actual,
                after_expected
            );
        }
        Ok(())
    }
}
