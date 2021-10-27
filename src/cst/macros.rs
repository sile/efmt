use crate::cst::common::{Atom, Variable};
use crate::format::{self, Format, Formatter};
use crate::lex::Lexer;
use crate::parse::{self, Parse};
use crate::token::{Region, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum MacroName {
    Atom(Atom),
    Variable(Variable),
}

impl Region for MacroName {
    fn region(&self) -> TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
        }
    }
}

impl Parse for MacroName {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Atom(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Variable(x))
        } else {
            let index = lexer.current_index();
            let token = lexer.read_token()?;
            Err(parse::Error::UnexpectedToken {
                index,
                token,
                expected: "AtomToken or VariableToken",
            })
        }
    }
}

impl Format for MacroName {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Atom(x) => x.format(fmt),
            Self::Variable(x) => x.format(fmt),
        }
    }
}
