use crate::pp::{MacroCall, MacroDefine, Result};
use crate::token::{CommentToken, LexicalToken, Symbol, Token, TokenIndex, TokenRange};
use erl_tokenize::{Position, PositionRange, Tokenizer};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug)]
pub struct Preprocessor {
    tokenizer: Tokenizer<String>,
    macro_defines: HashMap<String, MacroDefine>,
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
            macro_defines: MacroDefine::predefined(),
            preprocessed,
        }
    }

    pub fn preprocess(mut self) -> Result<PreprocessedText> {
        while let Some(token) = self.next_lexical_token()? {
            if let LexicalToken::Symbol(x) = &token {
                if x.value() == Symbol::Question {
                    let (tokens, macro_call) =
                        self.expand_macro(TokenIndex::new(self.preprocessed.tokens.len()))?;
                    let token_range = TokenRange::new(
                        TokenIndex::new(self.preprocessed.tokens.len()),
                        TokenIndex::new(self.preprocessed.tokens.len() + tokens.len()),
                    );
                    self.preprocessed.tokens.extend(tokens);
                    self.preprocessed
                        .macro_calls
                        .insert(token_range.start(), macro_call);
                    continue;
                } else if x.value() == Symbol::Hyphen {
                    self.try_handle_directives()?;
                    self.tokenizer.set_position(x.end_position());
                }
            }
            self.preprocessed.tokens.push(token);
        }
        Ok(self.preprocessed)
    }

    fn try_handle_directives(&mut self) -> Result<()> {
        if let Some(LexicalToken::Atom(token)) = self.next_lexical_token()? {
            match token.value() {
                "define" => {
                    todo!();
                    // let define = self.parse_define()?;
                    // self.macro_defines.insert(define.name().to_owned(), define);
                }
                "include" => todo!(),
                "include_lib" => todo!(),
                _ => {}
            }
        }
        Ok(())
    }

    // fn parse_define(&mut self) -> Result<MacroDefine> {
    //     let _ = self.read_expect(Symbol::OpenParen)?;
    //     let name = self.read_expect(Or(ExpectAtom, ExpectVariable))?;
    //     let params = if self
    //         .read_expect(Or(Symbol::Comma, Symbol::OpenParen))?
    //         .is_a()
    //     {
    //         None
    //     } else {
    //         let mut params = Vec::new();
    //         while let Some(param) = self.try_read_expect(ExpectVariable) {
    //             params.push(param);
    //             if self.try_read_expect(Symbol::Comma).is_none() {
    //                 break;
    //             }
    //         }
    //         let _ = self.read_expect(Symbol::CloseParen)?;
    //         let _ = self.read_expect(Symbol::Comma)?;
    //         Some(params)
    //     };
    //     let mut replacement = Vec::new();
    //     let mut level = 0;
    //     while let Some(token) = self.next_lexical_token()? {
    //         match &token {
    //             LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
    //                 let (tokens, _) = self.expand_macro(x.start_position())?;
    //                 replacement.extend(tokens);
    //                 continue;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
    //                 level += 1;
    //             }
    //             LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
    //                 if level == 0 {
    //                     break;
    //                 }
    //                 level -= 1;
    //             }
    //             _ => {}
    //         }
    //         replacement.push(token);
    //     }
    //     let _ = self.read_expect(Symbol::Dot)?;
    //     Ok(MacroDefine {
    //         name,
    //         params,
    //         replacement,
    //     })
    // }

    // fn try_read_expect<T: Expect>(&mut self, expected: T) -> Option<T::Token> {
    //     let position = self.tokenizer.next_position();
    //     if let Ok(token) = self.read_expect(expected) {
    //         Some(token)
    //     } else {
    //         self.tokenizer.set_position(position);
    //         None
    //     }
    // }

    // fn read_expect<T: Expect>(&mut self, expected: T) -> Result<T::Token> {
    //     if let Some(token) = self.next_lexical_token()? {
    //         expected.expect(token).map_err(|token| {
    //             anyhow::anyhow!("expected {:?}, but got {:?}", expected, token).into()
    //         })
    //     } else {
    //         Err(Error::UnexpectedEof)
    //     }
    // }

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
    use erl_tokenize::Tokenizer;

    #[test]
    fn preprocess_works() {
        let mut count = 0;
        for entry in glob::glob("testdata/pp/*_before.erl").unwrap() {
            let path = entry.unwrap();
            println!("[INPUT FILE] {:?}", path);

            let (before, after) = crate::tests::load_before_and_after_files(path);
            let pp = Preprocessor::new(Tokenizer::new(before));
            let preprocessed = pp.preprocess().unwrap();
            assert_eq!(preprocessed.to_string(), after);
            count += 1;
        }
        assert_ne!(count, 0);
    }
}
