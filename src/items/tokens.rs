//! Erlang tokens.
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};

pub trait TokenStr {
    fn token_str(&self) -> &str;
}

// Note that the `Parse` trait for `Token` is implemented in the `parse` module.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Span, Format, serde::Serialize, serde::Deserialize)]
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
    // TODO: delete
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

        impl Format for $name {
            fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
                fmt.write_text(self)
            }

            fn should_be_packed(&self) -> bool {
                $should_be_packed
            }
        }

        impl From<$name> for Token {
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

impl TokenStr for KeywordToken {
    fn token_str(&self) -> &str {
        self.value.as_str()
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

impl TokenStr for SymbolToken {
    fn token_str(&self) -> &str {
        self.value.as_str()
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn atom_works() {
        crate::assert_format!("foo", Token);
    }

    #[test]
    fn char_works() {
        crate::assert_format!("$a", Token);
    }

    #[test]
    fn float_works() {
        crate::assert_format!("12.3", Token);
    }

    #[test]
    fn integer_works() {
        crate::assert_format!("12", Token);
    }

    #[test]
    fn keyword_works() {
        crate::assert_format!("case", Token);
    }

    #[test]
    fn string_works() {
        crate::assert_format!("\"foo\"", Token);
    }

    #[test]
    fn symbol_works() {
        crate::assert_format!("-", Token);
    }

    #[test]
    fn variable_works() {
        crate::assert_format!("Foo", Token);
    }
}
