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

macro_rules! derive_traits {
    ($name:ident, $variant:ident, $expected:expr) => {
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
                    token => Err(parse::Error::unexpected_token(parser, token, $expected)),
                }
            }
        }

        impl Format for $name {
            fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
                fmt.noformat(self)?;
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

derive_traits!(AtomToken, Atom, "atom");

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

derive_traits!(CharToken, Char, "character");

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

derive_traits!(FloatToken, Float, "float");

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

derive_traits!(IntegerToken, Integer, "integer");

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

derive_traits!(KeywordToken, Keyword, "keyword");

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

derive_traits!(StringToken, String, "string");

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

derive_traits!(SymbolToken, Symbol, "symbol");

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

derive_traits!(VariableToken, Variable, "Variable");

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
        fmt.noformat(self)?;
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
