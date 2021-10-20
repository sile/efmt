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
}

impl Lexer {
    pub fn new(text: impl AsRef<str>) -> Self {
        Self {
            tokenizer: Tokenizer::new(text.as_ref().to_owned()),
            comments: BTreeMap::new(),
        }
    }

    pub fn current_position(&self) -> Position {
        Position::new(self.tokenizer.next_position().offset())
    }

    pub fn region(&self, start: Position) -> Region {
        Region::new(start, self.current_position())
    }

    // TODO: try_error_logs() -> Vec<Error>;

    pub fn eof(&mut self) -> Result<bool> {
        match self.peek_token() {
            Err(Error::UnexpectedEof) => Ok(true),
            Err(e) => Err(e),
            Ok(_) => Ok(false),
        }
    }

    pub fn with_transaction<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let position = self.tokenizer.next_position();
        match f(self) {
            Err(e) => {
                self.tokenizer.set_position(position);
                Err(e)
            }
            Ok(x) => Ok(x),
        }
    }

    pub fn with_peek<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        let position = self.tokenizer.next_position();
        let result = f(self);
        self.tokenizer.set_position(position);
        result
    }

    pub fn expect_2tokens(&mut self, expected0: impl Expect, expected1: impl Expect) -> bool {
        self.with_peek(|lexer| {
            lexer.read_expect(expected0).is_ok() && lexer.read_expect(expected1).is_ok()
        })
    }

    pub fn peek_token(&mut self) -> Result<LexicalToken> {
        self.with_peek(|lexer| lexer.read_token())
    }

    pub fn peek_tokens(&mut self, n: usize) -> Result<Vec<LexicalToken>> {
        self.with_peek(|lexer| {
            (0..n)
                .map(|_| lexer.read_token())
                .collect::<Result<Vec<_>>>()
        })
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
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

    pub fn try_read_expect<T: Expect>(&mut self, expected: T) -> Option<T::Token> {
        self.with_transaction(|lexer| lexer.read_expect(expected))
            .ok()
    }
}
