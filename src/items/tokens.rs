//! Erlang tokens.
use crate::format2::{Format2, Formatter2};
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Span, Format2)]
pub enum VisibleToken {
    Atom(AtomToken),
    Char(CharToken),
    Comment(CommentToken),
    Float(FloatToken),
    Integer(IntegerToken),
    Keyword(KeywordToken),
    String(StringToken),
    Symbol(SymbolToken),
    Variable(VariableToken),
}

impl VisibleToken {
    pub fn needs_space(&self, other: &Self) -> bool {
        use erl_tokenize::values::Symbol::*;

        if let (Self::Symbol(a), Self::Symbol(b)) = (self, other) {
            if matches!((a.value(), b.value()), (Hyphen, Hyphen) | (Plus, Plus)) {
                return true;
            }
        }
        if let (Self::Integer(_), Self::Symbol(b)) = (self, other) {
            if b.value() == Sharp {
                return true;
            }
        }
        if !matches!((self, other), (Self::Symbol(_), _) | (_, Self::Symbol(_))) {
            return true;
        }
        false
    }

    pub fn value(&self) -> Option<&str> {
        match self {
            Self::Atom(x) => Some(x.value()),
            Self::Variable(x) => Some(x.value()),
            Self::Symbol(x) => Some(x.value().as_str()),
            Self::Keyword(x) => Some(x.value().as_str()),
            _ => None,
        }
    }
}

// TODO(?): s/Token/LexicalToken/
// Note that the `Parse` trait for `Token` is implemented in the `parse` module.
#[derive(
    Debug, Clone, PartialEq, Eq, Hash, Span, Format2, serde::Serialize, serde::Deserialize,
)]
pub enum Token {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    Keyword(KeywordToken),
    String(StringToken),
    Symbol(SymbolToken),
    Variable(VariableToken),
}

impl Token {
    pub fn set_span(&mut self, span: &impl Span) {
        let (start, end) = match self {
            Self::Atom(x) => (&mut x.start, &mut x.end),
            Self::Char(x) => (&mut x.start, &mut x.end),
            Self::Float(x) => (&mut x.start, &mut x.end),
            Self::Integer(x) => (&mut x.start, &mut x.end),
            Self::Keyword(x) => (&mut x.start, &mut x.end),
            Self::String(x) => (&mut x.start, &mut x.end),
            Self::Symbol(x) => (&mut x.start, &mut x.end),
            Self::Variable(x) => (&mut x.start, &mut x.end),
        };
        *start = span.start_position();
        *end = span.end_position();
    }
}

impl Parse for Token {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        if let Some(token) = ts.next().transpose()? {
            Ok(token)
        } else {
            let position = ts.current_position();
            Err(parse::Error::UnexpectedEof { position })
        }
    }
}

macro_rules! impl_traits {
    ($name:ident, $variant:ident, $should_be_packed:expr) => {
        impl Span for $name {
            fn start_position(&self) -> Position {
                self.start
            }

            fn end_position(&self) -> Position {
                self.end
            }
        }

        impl Parse for $name {
            fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
                match ts.parse()? {
                    Token::$variant(token) => Ok(token),
                    token => Err(parse::Error::unexpected_token(ts, token)),
                }
            }
        }

        impl Format2 for $name {
            fn format2(&self, fmt: &mut Formatter2) {
                fmt.add_token(self.clone().into());
            }
        }

        impl From<$name> for Token {
            fn from(x: $name) -> Self {
                Self::$variant(x)
            }
        }

        impl From<$name> for VisibleToken {
            fn from(x: $name) -> Self {
                Self::$variant(x)
            }
        }
    };
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct AtomToken {
    value: String,
    start: Position,
    end: Position,
}

impl AtomToken {
    pub fn new(value: &str, start: Position, end: Position) -> Self {
        Self {
            value: value.to_owned(),
            start,
            end,
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl_traits!(AtomToken, Atom, true);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct CharToken {
    start: Position,
    end: Position,
}

impl CharToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl_traits!(CharToken, Char, true);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct FloatToken {
    start: Position,
    end: Position,
}

impl FloatToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl_traits!(FloatToken, Float, true);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct IntegerToken {
    start: Position,
    end: Position,
}

impl IntegerToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl_traits!(IntegerToken, Integer, true);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct KeywordToken {
    value: Keyword,
    start: Position,
    end: Position,
}

impl KeywordToken {
    pub fn new(value: Keyword, start: Position, end: Position) -> Self {
        Self { value, start, end }
    }

    pub fn value(&self) -> Keyword {
        self.value
    }
}

impl_traits!(KeywordToken, Keyword, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct StringToken {
    value: String,
    start: Position,
    end: Position,
}

impl StringToken {
    pub fn new(value: &str, start: Position, end: Position) -> Self {
        Self {
            value: value.to_owned(),
            start,
            end,
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl_traits!(StringToken, String, true);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct SymbolToken {
    value: Symbol,
    start: Position,
    end: Position,
}

impl SymbolToken {
    pub fn new(value: Symbol, start: Position, end: Position) -> Self {
        Self { value, start, end }
    }

    pub fn value(&self) -> Symbol {
        self.value
    }
}

impl_traits!(SymbolToken, Symbol, false);

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct VariableToken {
    value: String,
    start: Position,
    end: Position,
}

impl VariableToken {
    pub fn new(value: &str, start: Position, end: Position) -> Self {
        Self {
            value: value.to_owned(),
            start,
            end,
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl_traits!(VariableToken, Variable, true);

#[derive(Debug, Clone, PartialEq, Eq, Copy, Hash, serde::Serialize, serde::Deserialize)]
pub enum CommentKind {
    Post,
    Trailing,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct CommentToken {
    start: Position,
    end: Position,
    kind: CommentKind,
}

impl CommentToken {
    pub fn new(kind: CommentKind, start: Position, end: Position) -> Self {
        Self { start, end, kind }
    }

    pub fn kind(&self) -> CommentKind {
        self.kind
    }
}

impl Span for CommentToken {
    fn start_position(&self) -> Position {
        self.start
    }

    fn end_position(&self) -> Position {
        self.end
    }
}

impl Format2 for CommentToken {
    fn format2(&self, fmt: &mut Formatter2) {
        match self.kind() {
            CommentKind::Trailing => {
                fmt.add_spaces(2);
            }
            CommentKind::Post => {
                fmt.add_newline();
            }
        }
        fmt.add_token(VisibleToken::Comment(self.clone()));
        fmt.add_newline();
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WhitespaceToken {
    start: Position,
    end: Position,
}

impl WhitespaceToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl Span for WhitespaceToken {
    fn start_position(&self) -> Position {
        self.start
    }

    fn end_position(&self) -> Position {
        self.end
    }
}
