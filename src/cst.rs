use crate::parse::{self, Parse, TokenReader};
use crate::token::{Region, TokenRegion};

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
