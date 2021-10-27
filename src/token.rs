pub use erl_tokenize::tokens::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, StringToken,
    SymbolToken, VariableToken, WhitespaceToken,
};
pub use erl_tokenize::values::{Keyword, Symbol};
pub use erl_tokenize::Position as TokenPosition;
pub use erl_tokenize::{LexicalToken, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd)]
pub struct TokenRegion {
    start: TokenPosition,
    end: TokenPosition,
}

impl TokenRegion {
    pub fn new(start: TokenPosition, end: TokenPosition) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> &TokenPosition {
        &self.start
    }

    pub fn end(&self) -> &TokenPosition {
        &self.end
    }
}

pub trait Region {
    fn region(&self) -> &TokenRegion;
}

impl Region for TokenRegion {
    fn region(&self) -> &TokenRegion {
        self
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
