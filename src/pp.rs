use erl_tokenize::tokens::VariableToken;
use erl_tokenize::values::Symbol;
use erl_tokenize::{LexicalToken, Position, Result, Token, Tokenizer};
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct Preprocessor {
    tokenizer: Tokenizer<String>,
    visited: HashSet<Position>,
    defines: HashMap<String, MacroDefine>,
}

impl Preprocessor {
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        Self {
            tokenizer,
            visited: HashSet::new(),
            defines: HashMap::new(),
        }
    }

    pub fn next_position(&self) -> Position {
        self.tokenizer.next_position()
    }

    pub fn set_position(&mut self, position: Position) {
        self.tokenizer.set_position(position);
    }

    fn next_token(&mut self) -> Result<Option<Token>> {
        let position = self.tokenizer.next_position();
        let token = self.tokenizer.next().transpose()?;
        if !self.visited.contains(&position) {
            self.visited.insert(position.clone());
            if let Some(token) = &token {
                self.try_process_define(token);
            }
        }
        Ok(token)
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
                if let Some(macro_define) = self.try_parse_define() {
                    self.defines.insert(macro_define.name.clone(), macro_define);
                }
            }
            self.tokenizer.set_position(position);
        }
    }

    fn try_parse_define(&mut self) -> Option<MacroDefine> {
        if self
            .try_next_lexical_token()
            .filter(|x| {
                x.as_symbol_token()
                    .map_or(false, |x| x.value() == Symbol::OpenParen)
            })
            .is_none()
        {
            return None;
        }

        let name = match self.try_next_lexical_token() {
            Some(LexicalToken::Atom(x)) => x.value().to_owned(),
            Some(LexicalToken::Variable(x)) => x.value().to_owned(),
            _ => return None,
        };

        let mut args = Vec::new();
        match self.try_next_lexical_token() {
            Some(LexicalToken::Symbol(x)) if x.value() == Symbol::Comma => {}
            Some(LexicalToken::Symbol(x)) if x.value() == Symbol::OpenParen => loop {
                match self.try_next_lexical_token() {
                    Some(LexicalToken::Variable(arg)) => {
                        args.push(arg);
                        match self.try_next_lexical_token() {
                            Some(LexicalToken::Symbol(x)) if x.value() == Symbol::CloseParen => {
                                break
                            }
                            Some(LexicalToken::Symbol(x)) if x.value() == Symbol::Comma => {}
                            _ => return None,
                        }
                    }
                    _ => return None,
                }
            },
            _ => return None,
        }

        let mut replacement = Vec::new();
        let mut level = 0;
        loop {
            match self.try_next_lexical_token() {
                Some(token) => {
                    match &token {
                        LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
                            level += 1;
                        }
                        LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                            if level == 0 {
                                break;
                            } else {
                                level -= 1;
                            }
                        }
                        _ => {}
                    }
                    replacement.push(token.into());
                }
                None => return None,
            }
        }
        if self
            .try_next_lexical_token()
            .filter(|x| {
                x.as_symbol_token()
                    .map_or(false, |x| x.value() == Symbol::Dot)
            })
            .is_none()
        {
            return None;
        }

        Some(MacroDefine {
            name,
            args,
            replacement,
        })
    }
}

impl Iterator for Preprocessor {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token().transpose()
    }
}

#[derive(Debug)]
pub struct MacroDefine {
    pub name: String,
    pub args: Vec<VariableToken>,
    pub replacement: Vec<Token>,
}
