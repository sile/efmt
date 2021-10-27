use crate::cst::primitives::{Atom, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, TokenRegion};
use std::io::Write;

pub type AtomExpr = Atom;
pub type VariableExpr = Variable;
pub type StringExpr = String;

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(AtomExpr),
    Variable(VariableExpr),
    String(StringExpr),
}

impl Region for Expr {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
            Self::String(x) => x.region(),
        }
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::Atom(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Variable(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::String(x))
        } else {
            let (_, e) = parser.take_last_error().expect("unreachable");
            // TODO: check position
            Err(e)
        }
    }
}

impl Format for Expr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Atom(x) => x.format(fmt),
            Self::Variable(x) => x.format(fmt),
            Self::String(x) => x.format(fmt),
        }
    }
}
