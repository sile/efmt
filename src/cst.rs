use crate::format::{self, Format, Formatter};
use crate::lex::Lexer;
use crate::parse::{self, Parse};
use crate::token::{Region, TokenRegion};
use std::io::Write;

pub mod attributes;
pub mod common;
pub mod expressions;
pub mod macros;

// pub mod types;

/// Concrete Syntax Tree. (or TopLevelItem)
#[derive(Debug, Clone)]
pub enum Cst {
    Attr(self::attributes::Attr),
}

impl Region for Cst {
    fn region(&self) -> TokenRegion {
        match self {
            Self::Attr(x) => x.region(),
        }
    }
}

impl Parse for Cst {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Attr(x))
        } else {
            Err(lexer.take_last_error().expect("unreachable").into())
        }
    }
}

impl Format for Cst {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Attr(x) => x.format(fmt),
        }
    }
}
