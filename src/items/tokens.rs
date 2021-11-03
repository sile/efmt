use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};
use std::io::Write;

// Note that the `Parse` trait for `Token` is implemented in the `parse` module.
#[derive(Debug, Clone, Span, Format)]
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
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                match parser.parse()? {
                    Token::$variant(token) => Ok(token),
                    token => Err(parse::Error::unexpected_token(parser, token)),
                }
            }
        }

        impl Format for $name {
            fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
                fmt.write_text(self)?;
                Ok(())
            }
        }

        impl From<$name> for Token {
            fn from(x: $name) -> Self {
                Self::$variant(x)
            }
        }
    };
}

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct StringToken {
    start: Position,
    end: Position,
}

impl StringToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }
}

impl_traits!(StringToken, String);

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct CommentToken {
    start: Position,
    end: Position,
}

impl CommentToken {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
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

impl Format for CommentToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.write_text(self)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse_text;

    #[test]
    fn atom_works() {
        assert!(matches!(parse_text("foo").unwrap(), Token::Atom(_)));
    }

    #[test]
    fn char_works() {
        assert!(matches!(parse_text("$a").unwrap(), Token::Char(_)));
    }

    #[test]
    fn float_works() {
        assert!(matches!(parse_text("12.3").unwrap(), Token::Float(_)));
    }

    #[test]
    fn integer_works() {
        assert!(matches!(parse_text("12").unwrap(), Token::Integer(_)));
    }

    #[test]
    fn keyword_works() {
        assert!(matches!(parse_text("case").unwrap(), Token::Keyword(_)));
    }

    #[test]
    fn string_works() {
        assert!(matches!(parse_text("\"foo\"").unwrap(), Token::String(_)));
    }

    #[test]
    fn symbol_works() {
        assert!(matches!(parse_text("-").unwrap(), Token::Symbol(_)));
    }

    #[test]
    fn variable_works() {
        assert!(matches!(parse_text("Foo").unwrap(), Token::Variable(_)));
    }
}
