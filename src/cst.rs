use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, TokenReader};
use crate::token::{Region, TokenRegion};
use std::io::Write;

pub mod attributes;
// pub mod types;
// pub mod expressions;
// pub mod attributes;

/// Concrete Syntax Tree.
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
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::Attr(x))
        } else {
            Err(tokens.take_last_error().expect("unreachable"))
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
