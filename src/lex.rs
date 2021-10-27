use crate::token::{CommentToken, LexicalToken, Token, TokenIndex, TokenRegion, TokenTextOffset};
use erl_tokenize::{PositionRange, Tokenizer};
use std::collections::BTreeMap;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedTokenValue {
        index: TokenIndex,
        token: LexicalToken,
        expected: String,
    },

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedToken {
        index: TokenIndex,
        token: LexicalToken,
        expected: &'static str,
    },

    #[error("invalid token region: start={start:?}, end={end:?}")]
    InvaildRegion { start: TokenIndex, end: TokenIndex },

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

impl Error {
    fn is_high_priority_than(&self, other: &Self) -> bool {
        fn token_index(e: &Error) -> TokenIndex {
            match e {
                Error::UnexpectedTokenValue { index, .. } => *index,
                Error::UnexpectedToken { index, .. } => *index,
                _ => unreachable!(),
            }
        }

        match (self, other) {
            (Self::TokenizeError { .. }, _) => true,
            (Self::InvaildRegion { .. }, _) => true, // This is a bug.
            (Self::UnexpectedEof, _) => true,
            (a, b) => token_index(a) >= token_index(b),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer<String>,
    tokens: Vec<LexicalToken>,
    current: TokenIndex,
    comments: BTreeMap<TokenTextOffset, CommentToken>,
    last_error: Option<Error>,
}

impl Lexer {
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            current: TokenIndex::new(0),
            comments: BTreeMap::new(),
            last_error: None,
        }
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
        if let Some(token) = self.tokens.get(self.current.get()).cloned() {
            self.current = TokenIndex::new(self.current.get() + 1);
            return Ok(token);
        }

        while let Some(token) = self.tokenizer.next().transpose()? {
            let token: LexicalToken = match token {
                Token::Whitespace(_) => {
                    continue;
                }
                Token::Comment(x) => {
                    self.comments
                        .insert(TokenTextOffset::new(x.start_position().offset()), x);
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
            // TODO: preprocess
            self.tokens.push(token.clone());
            self.current = TokenIndex::new(self.current.get() + 1);
            return Ok(token);
        }

        Err(Error::UnexpectedEof)
    }

    pub fn with_transaction<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let index = self.current;
        match f(self) {
            Err(e) => {
                self.current = index;
                if self
                    .last_error
                    .as_ref()
                    .map_or(true, |last| e.is_high_priority_than(last))
                {
                    self.last_error = Some(e);
                }
                None
            }
            Ok(v) => Some(v),
        }
    }

    pub fn take_last_error(&mut self) -> Option<Error> {
        self.last_error.take()
    }

    pub fn current_index(&self) -> TokenIndex {
        self.current
    }

    pub fn region(&self, start: TokenIndex) -> Result<TokenRegion> {
        let end = self.current;
        TokenRegion::new(start, end).ok_or(Error::InvaildRegion { start, end })
    }
}
