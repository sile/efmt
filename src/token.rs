pub use erl_tokenize::tokens::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, StringToken,
    SymbolToken, VariableToken, WhitespaceToken,
};
pub use erl_tokenize::values::{Keyword, Symbol};
pub use erl_tokenize::{LexicalToken, Token};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("invalid token range: start={start:?}, end={end:?}")]
    InvaildRange { start: TokenIndex, end: TokenIndex },
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenTextOffset(usize);

impl TokenTextOffset {
    pub const fn new(offset: usize) -> Self {
        Self(offset)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenIndex(usize);

impl TokenIndex {
    pub const fn new(i: usize) -> Self {
        Self(i)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenRegion {
    start: TokenIndex,
    end: TokenIndex,
}

impl TokenRegion {
    pub fn new(start: TokenIndex, end: TokenIndex) -> Result<Self> {
        if start <= end {
            Ok(Self { start, end })
        } else {
            Err(Error::InvaildRange { start, end })
        }
    }

    pub const fn start(self) -> TokenIndex {
        self.start
    }

    pub const fn end(self) -> TokenIndex {
        self.end
    }
}

pub trait Region {
    fn region(&self) -> TokenRegion;
}

#[derive(Debug)]
pub struct TokenReader {
    tokens: Vec<LexicalToken>,
    current: TokenIndex,
}

impl TokenReader {
    pub fn new(tokens: Vec<LexicalToken>) -> Self {
        Self {
            tokens,
            current: TokenIndex::new(0),
        }
    }

    pub fn with_transaction<F, T, E>(&mut self, f: F) -> std::result::Result<T, E>
    where
        F: FnOnce(&mut Self) -> std::result::Result<T, E>,
        E: From<Error>,
    {
        let index = self.current;
        let result = f(self);
        if result.is_err() {
            self.current = index;
        }
        result
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
        if let Some(token) = self.tokens.get(self.current.0).cloned() {
            self.current = TokenIndex(self.current.get() + 1);
            Ok(token)
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    pub fn current_index(&self) -> TokenIndex {
        self.current
    }

    pub fn region(&self, start: TokenIndex) -> Result<TokenRegion> {
        TokenRegion::new(start, self.current)
    }
}
