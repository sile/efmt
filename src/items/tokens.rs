//! Erlang tokens.
use crate::format::{Format, Formatter};
use crate::items::components::Element;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};

/// Token used in the parse phase.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Span, Format, serde::Serialize, serde::Deserialize)]
pub enum LexicalToken {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    Keyword(KeywordToken),
    String(StringToken),
    Symbol(SymbolToken),
    Variable(VariableToken),
}

impl LexicalToken {
    pub(crate) fn set_span(&mut self, span: &impl Span) {
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

impl Parse for LexicalToken {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        if let Some(token) = ts.next().transpose()? {
            Ok(token)
        } else {
            Err(parse::Error::unexpected_eof(ts))
        }
    }
}

/// Token used in the format phase.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Span)]
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
        if matches!((self, other), (Self::Comment(_), _) | (_, Self::Comment(_))) {
            return false;
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

    pub fn is_trailing_comment(&self) -> bool {
        if let Self::Comment(x) = self {
            x.is_trailing()
        } else {
            false
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(self, Self::Comment(_))
    }
}

macro_rules! impl_traits {
    ($name:ident, $variant:ident) => {
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
                    LexicalToken::$variant(token) => Ok(token),
                    token => Err(parse::Error::unexpected_token(ts, token)),
                }
            }
        }

        impl Format for $name {
            fn format(&self, fmt: &mut Formatter) {
                fmt.write_token(self.clone().into());
            }
        }

        impl From<$name> for LexicalToken {
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

impl_traits!(AtomToken, Atom);

impl Element for AtomToken {
    fn is_packable(&self) -> bool {
        true
    }
}

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

impl_traits!(CharToken, Char);

impl Element for CharToken {
    fn is_packable(&self) -> bool {
        true
    }
}

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

impl_traits!(FloatToken, Float);

impl Element for FloatToken {
    fn is_packable(&self) -> bool {
        true
    }
}

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

impl_traits!(IntegerToken, Integer);

impl Element for IntegerToken {
    fn is_packable(&self) -> bool {
        true
    }
}

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

impl_traits!(KeywordToken, Keyword);

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

impl_traits!(StringToken, String);

impl Element for StringToken {
    fn is_packable(&self) -> bool {
        true
    }
}

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

impl_traits!(SymbolToken, Symbol);

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

impl_traits!(VariableToken, Variable);

impl Element for VariableToken {
    fn is_packable(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct CommentToken {
    is_trailing: bool,
    start: Position,
    end: Position,
}

impl CommentToken {
    pub fn new(is_trailing: bool, start: Position, end: Position) -> Self {
        Self {
            is_trailing,
            start,
            end,
        }
    }

    pub fn is_trailing(&self) -> bool {
        self.is_trailing
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
