pub use erl_tokenize::tokens::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, StringToken,
    SymbolToken, VariableToken, WhitespaceToken,
};
pub use erl_tokenize::values::{Keyword, Symbol};
pub use erl_tokenize::{LexicalToken, Token};

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
    pub fn new(start: TokenIndex, end: TokenIndex) -> Option<Self> {
        if start <= end {
            Some(Self { start, end })
        } else {
            None
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
