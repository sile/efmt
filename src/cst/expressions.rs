use crate::cst::common::{Atom, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::lex::Lexer;
use crate::parse::{self, Parse};
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
    fn region(&self) -> TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
            Self::String(x) => x.region(),
        }
    }
}

impl Parse for Expr {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Atom(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Variable(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::String(x))
        } else {
            Err(lexer.take_last_error().expect("unreachable").into())
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
