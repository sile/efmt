use crate::expect::{Either, Expect, ExpectAtom, ExpectVariable, Or};
use erl_tokenize::tokens::{AtomToken, CommentToken, VariableToken};
use erl_tokenize::values::{Keyword, Symbol};
use erl_tokenize::{LexicalToken, Position, PositionRange, Result, Token, Tokenizer};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("macro {name:?} is not defined ({position:?})")]
    UndefinedMacro { name: String, position: Position },

    #[error("badly formed argument ({position:?})")]
    MalformedMacroArg { position: Position },

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

#[derive(Debug)]
pub struct Preprocessor2 {
    tokenizer: Tokenizer<String>,
    macro_defines: HashMap<String, MacroDefine>,
    preprocessed: Preprocessed,
}

impl Preprocessor2 {
    // TODO: user defined predefined macros
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        let preprocessed = Preprocessed {
            file: tokenizer.next_position().filepath().cloned(),
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

    fn try_handle_directives(&mut self) -> std::result::Result<(), Error> {
        if let Some(LexicalToken::Atom(token)) = self.next_lexical_token()? {
            match token.value() {
                "define" => {
                    let define = self.parse_define()?;
                    self.macro_defines.insert(define.name().to_owned(), define);
                }
                "include" => todo!(),
                "include_lib" => todo!(),
                _ => {}
            }
        }
        Ok(())
    }

    fn parse_define(&mut self) -> std::result::Result<MacroDefine, Error> {
        let _ = self.read_expect(Symbol::OpenParen)?;
        let name = self.read_expect(Or(ExpectAtom, ExpectVariable))?;
        let params = if self
            .read_expect(Or(Symbol::Comma, Symbol::OpenParen))?
            .is_a()
        {
            None
        } else {
            let mut params = Vec::new();
            while let Some(param) = self.try_read_expect(ExpectVariable) {
                params.push(param);
                if self.try_read_expect(Symbol::Comma).is_none() {
                    break;
                }
            }
            let _ = self.read_expect(Symbol::CloseParen)?;
            let _ = self.read_expect(Symbol::Comma)?;
            Some(params)
        };
        let mut replacement = Vec::new();
        let mut level = 0;
        while let Some(token) = self.next_lexical_token()? {
            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
                    level += 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                    if level == 0 {
                        break;
                    }
                    level -= 1;
                }
                _ => {}
            }
            replacement.push(token);
        }
        let _ = self.read_expect(Symbol::Dot)?;
        Ok(MacroDefine {
            name,
            params,
            replacement,
        })
    }

    fn try_read_expect<T: Expect>(&mut self, expected: T) -> Option<T::Token> {
        let position = self.tokenizer.next_position();
        if let Some(token) = self.read_expect(expected).ok() {
            Some(token)
        } else {
            self.tokenizer.set_position(position);
            None
        }
    }

    fn read_expect<T: Expect>(&mut self, expected: T) -> std::result::Result<T::Token, Error> {
        if let Some(token) = self.next_lexical_token()? {
            expected.expect(token).map_err(|token| {
                anyhow::anyhow!("expected {:?}, but got {:?}", expected, token).into()
            })
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    fn next_lexical_token(&mut self) -> std::result::Result<Option<LexicalToken>, Error> {
        while let Some(token) = self.tokenizer.next().transpose()? {
            match token {
                Token::Whitespace(_) => {}
                Token::Comment(x) => {
                    if self.preprocessed.file.as_ref() == x.start_position().filepath() {
                        self.preprocessed.comments.insert(x.start_position(), x);
                    }
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

    pub fn preprocess(mut self) -> std::result::Result<Preprocessed, Error> {
        while let Some(token) = self.next_lexical_token()? {
            if let LexicalToken::Symbol(x) = &token {
                if x.value() == Symbol::Question {
                    self.expand_macro()?;
                } else if x.value() == Symbol::Hyphen {
                    self.try_handle_directives()?;
                    self.tokenizer.set_position(x.end_position());
                }
            }
            self.preprocessed.tokens.push(token);
        }
        Ok(self.preprocessed)
    }

    fn expand_macro(&mut self) -> std::result::Result<(), Error> {
        let position = self.tokenizer.next_position();
        let name = match self.read_expect(Or(ExpectAtom, ExpectVariable))? {
            Either::A(x) => x.value().to_owned(),
            Either::B(x) => x.value().to_owned(),
        };
        let define =
            self.macro_defines
                .get(&name)
                .cloned()
                .ok_or_else(|| Error::UndefinedMacro {
                    name: name.clone(),
                    position: position.clone(),
                })?;

        if let Some(params) = &define.params {
            let _ = self.read_expect(Symbol::OpenParen)?;
            let mut args = HashMap::new();
            for (i, param) in params.iter().enumerate() {
                let arg = self.parse_macro_arg()?;
                args.insert(param.value(), arg);
                if i + 1 < params.len() {
                    self.read_expect(Symbol::Comma)?;
                }
            }
            let _ = self.read_expect(Symbol::CloseParen)?;
            self.preprocessed
                .tokens
                .extend(define.replace_variables(&args));
        } else {
            self.preprocessed
                .tokens
                .extend(define.replacement.iter().cloned());
        }
        Ok(())
    }

    fn parse_macro_arg(&mut self) -> std::result::Result<Vec<LexicalToken>, Error> {
        #[derive(Debug, Default)]
        struct Level {
            paren: usize,
            brace: usize,
            square: usize,
            block: usize,
        }

        impl Level {
            fn is_top_level(&self) -> bool {
                (self.paren + self.brace + self.square + self.block) == 0
            }
        }

        let mut level = Level::default();
        let mut tokens = Vec::new();
        while let Some(token) = self.next_lexical_token()? {
            let position = token.start_position();
            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Comma && level.is_top_level() => {
                    self.tokenizer.set_position(token.start_position());
                    break;
                }
                LexicalToken::Symbol(x)
                    if x.value() == Symbol::CloseParen && level.is_top_level() =>
                {
                    self.tokenizer.set_position(token.start_position());
                    break;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::Dot => {
                    return Err(Error::MalformedMacroArg { position });
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
                    level.paren += 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                    if level.paren == 0 {
                        return Err(Error::MalformedMacroArg { position });
                    }
                    level.paren -= 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::OpenBrace => {
                    level.brace += 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseBrace => {
                    if level.brace == 0 {
                        return Err(Error::MalformedMacroArg { position });
                    }
                    level.brace -= 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::OpenSquare => {
                    level.square += 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseSquare => {
                    if level.square == 0 {
                        return Err(Error::MalformedMacroArg { position });
                    }
                    level.square -= 1;
                }
                LexicalToken::Keyword(x)
                    if x.value() == Keyword::Begin
                        && x.value() == Keyword::Try
                        && x.value() == Keyword::Fun
                        && x.value() == Keyword::Case
                        && x.value() == Keyword::If =>
                {
                    level.block += 1;
                }
                LexicalToken::Keyword(x) if x.value() == Keyword::End => {
                    if level.block == 0 {
                        return Err(Error::MalformedMacroArg { position });
                    }
                    level.block -= 1;
                }
                _ => {}
            }
            tokens.push(token);
        }

        Ok(tokens)
    }
}

#[derive(Debug)]
pub struct Preprocessed {
    pub file: Option<PathBuf>,
    pub text: String,
    pub tokens: Vec<LexicalToken>,
    pub comments: BTreeMap<Position, CommentToken>,
    pub macro_calls: BTreeMap<Position, MacroCall>,
}

#[derive(Debug)]
pub struct Preprocessor {
    tokenizer: Tokenizer<String>,
    visited: HashSet<Position>,
    defines: HashMap<String, MacroDefine>,
    expanded_tokens: Vec<Token>,
}

impl Preprocessor {
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        Self {
            tokenizer,
            visited: HashSet::new(),
            defines: HashMap::new(),
            expanded_tokens: Vec::new(),
        }
    }

    pub fn next_position(&self) -> Position {
        self.tokenizer.next_position()
    }

    pub fn set_position(&mut self, position: Position) {
        self.tokenizer.set_position(position);
        self.expanded_tokens.clear();
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
        if let Some(token) = self.expanded_tokens.pop() {
            return Ok(Some(token));
        }

        let position = self.tokenizer.next_position();
        let token = self.tokenizer.next().transpose()?;
        if !self.visited.contains(&position) {
            self.visited.insert(position.clone());
            if let Some(token) = &token {
                self.try_process_define(token);
            }
        }
        if let Some(token) = &token {
            self.try_process_macro_expansion(token);
            if let Some(token) = self.expanded_tokens.pop() {
                return Ok(Some(token));
            }
        }
        Ok(token)
    }

    fn try_process_macro_expansion(&mut self, token: &Token) {
        match token {
            Token::Symbol(x) if x.value() == Symbol::Question => {}
            _ => return,
        }

        let name = match self.try_next_lexical_token() {
            Some(LexicalToken::Atom(x)) => x.value().to_owned(),
            Some(LexicalToken::Variable(x)) => x.value().to_owned(),
            token => todo!("invalid macro name: {:?}", token),
        };
        if let Some(define) = self.defines.get(&name) {
            if define.params.is_none() {
                todo!();
            } else {
                todo!();
            }
        } else {
            todo!("undefined macro: {:?}", name);
        }
    }

    fn try_next_lexical_token(&mut self) -> Option<LexicalToken> {
        while let Some(token) = self.tokenizer.next().transpose().ok().flatten() {
            match token {
                Token::Comment(_) | Token::Whitespace(_) => {}
                Token::Atom(t) => return Some(t.into()),
                Token::Char(t) => return Some(t.into()),
                Token::Float(t) => return Some(t.into()),
                Token::Integer(t) => return Some(t.into()),
                Token::Keyword(t) => return Some(t.into()),
                Token::String(t) => return Some(t.into()),
                Token::Symbol(t) => return Some(t.into()),
                Token::Variable(t) => return Some(t.into()),
            }
        }
        None
    }

    fn try_process_define(&mut self, token: &Token) {
        if token
            .as_symbol_token()
            .map_or(false, |x| x.value() == Symbol::Hyphen)
        {
            let position = self.tokenizer.next_position();
            if self
                .try_next_lexical_token()
                .filter(|x| x.as_atom_token().map_or(false, |x| x.value() == "define"))
                .is_some()
            {
                if let Some(_macro_define) = self.try_parse_define() {
                    // self.defines.insert(macro_define.name.clone(), macro_define);
                    todo!()
                }
            }
            self.tokenizer.set_position(position);
        }
    }

    fn try_parse_define(&mut self) -> Option<MacroDefine> {
        todo!()
        // if self
        //     .try_next_lexical_token()
        //     .filter(|x| {
        //         x.as_symbol_token()
        //             .map_or(false, |x| x.value() == Symbol::OpenParen)
        //     })
        //     .is_none()
        // {
        //     return None;
        // }

        // let name = match self.try_next_lexical_token() {
        //     Some(LexicalToken::Atom(x)) => x.value().to_owned(),
        //     Some(LexicalToken::Variable(x)) => x.value().to_owned(),
        //     _ => return None,
        // };

        // let args = match self.try_next_lexical_token() {
        //     Some(LexicalToken::Symbol(x)) if x.value() == Symbol::Comma => None,
        //     Some(LexicalToken::Symbol(x)) if x.value() == Symbol::OpenParen => {
        //         let mut args = Vec::new();
        //         loop {
        //             match self.try_next_lexical_token() {
        //                 Some(LexicalToken::Variable(arg)) => {
        //                     args.push(arg);
        //                     match self.try_next_lexical_token() {
        //                         Some(LexicalToken::Symbol(x))
        //                             if x.value() == Symbol::CloseParen =>
        //                         {
        //                             break
        //                         }
        //                         Some(LexicalToken::Symbol(x)) if x.value() == Symbol::Comma => {}
        //                         _ => return None,
        //                     }
        //                 }
        //                 _ => return None,
        //             }
        //         }
        //         Some(args)
        //     }
        //     _ => return None,
        // };

        // let mut replacement = Vec::new();
        // let mut level = 0;
        // loop {
        //     match self.try_next_lexical_token() {
        //         Some(token) => {
        //             match &token {
        //                 LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
        //                     level += 1;
        //                 }
        //                 LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
        //                     if level == 0 {
        //                         break;
        //                     } else {
        //                         level -= 1;
        //                     }
        //                 }
        //                 _ => {}
        //             }
        //             replacement.push(token.into());
        //         }
        //         None => return None,
        //     }
        // }
        // if self
        //     .try_next_lexical_token()
        //     .filter(|x| {
        //         x.as_symbol_token()
        //             .map_or(false, |x| x.value() == Symbol::Dot)
        //     })
        //     .is_none()
        // {
        //     return None;
        // }

        // Some(MacroDefine {
        //     name,
        //     args,
        //     replacement,
        // })
    }
}

impl Iterator for Preprocessor {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

#[derive(Debug, Clone)]
pub struct MacroDefine {
    name: Either<AtomToken, VariableToken>,
    params: Option<Vec<VariableToken>>,
    replacement: Vec<LexicalToken>,
}

impl MacroDefine {
    pub fn predefined() -> HashMap<String, Self> {
        [
            "MODULE",
            "MODULE_STRING",
            "FILE",
            "LINE",
            "MACHINE",
            "FUNCTION_NAME",
            "FUNCTION_ARITY",
            "OTP_RELEASE",
        ]
        .into_iter()
        .map(|name| {
            (
                name.to_owned(),
                MacroDefine {
                    name: Either::B(VariableToken::from_value(name, Position::new()).expect("bug")),
                    params: None,
                    replacement: vec![AtomToken::from_value("dummy", Position::new()).into()],
                },
            )
        })
        .collect()
    }

    pub fn name(&self) -> &str {
        match &self.name {
            Either::A(x) => x.value(),
            Either::B(x) => x.value(),
        }
    }

    pub fn replace_variables(&self, args: &HashMap<&str, Vec<LexicalToken>>) -> Vec<LexicalToken> {
        let mut tokens = Vec::new();
        for token in self.replacement.iter().cloned() {
            match &token {
                LexicalToken::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].iter().cloned());
                    continue;
                }
                _ => {}
            }
            tokens.push(token);
        }
        tokens
    }
}

#[derive(Debug)]
pub struct MacroCall {}
