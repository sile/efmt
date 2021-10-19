use crate::{Error, Expect, Result};
use erl_tokenize::tokens::CommentToken;
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
    token_stack: Vec<LexicalToken>,
}

impl Lexer {
    pub fn new(text: impl AsRef<str>) -> Self {
        Self {
            tokenizer: Tokenizer::new(text.as_ref().to_owned()),
            comments: BTreeMap::new(),
            current_position: Position::new(0),
            token_stack: Vec::new(),
        }
    }

    pub fn current_position(&self) -> Position {
        self.current_position
    }

    pub fn eof(&mut self) -> Result<bool> {
        match self.peek_token() {
            Err(Error::UnexpectedEof) => Ok(true),
            Err(e) => Err(e),
            Ok(_) => Ok(false),
        }
    }

    pub fn peek_token(&mut self) -> Result<LexicalToken> {
        let token = self.read_token()?;
        self.unread_token(token.clone());
        Ok(token)
    }

    pub fn peek_tokens(&mut self, n: usize) -> Result<Vec<LexicalToken>> {
        let tokens = (0..n)
            .map(|_| self.read_token())
            .collect::<Result<Vec<_>>>()?;
        for token in tokens.iter().rev().cloned() {
            self.unread_token(token);
        }
        Ok(tokens)
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
        let token = if let Some(token) = self.token_stack.pop() {
            token
        } else {
            self.next_token()?
        };
        self.current_position = Position::new(token.end_position().offset());
        Ok(token)
    }

    pub fn unread_token(&mut self, token: LexicalToken) {
        self.current_position = Position::new(token.start_position().offset());
        self.token_stack.push(token);
    }

    fn next_token(&mut self) -> Result<LexicalToken> {
        while let Some(token) = self.tokenizer.next().transpose()? {
            match token {
                Token::Comment(t) => {
                    self.comments
                        .insert(Position::new(t.start_position().offset()), t);
                }
                Token::Whitespace(_) => {}
                Token::Atom(t) => return Ok(t.into()),
                Token::Char(t) => return Ok(t.into()),
                Token::Float(t) => return Ok(t.into()),
                Token::Integer(t) => return Ok(t.into()),
                Token::Keyword(t) => return Ok(t.into()),
                Token::String(t) => return Ok(t.into()),
                Token::Symbol(t) => return Ok(t.into()),
                Token::Variable(t) => return Ok(t.into()),
            }
        }
        Err(Error::UnexpectedEof)
    }

    pub fn read_expect<T: Expect>(&mut self, expected: T) -> Result<T::Token> {
        let token = self.read_token()?;
        expected
            .expect(token)
            .map_err(|token| anyhow::anyhow!("expected {:?}, but got {:?}", expected, token).into())
    }
}
