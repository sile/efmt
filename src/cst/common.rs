use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, TokenReader};
use crate::token::{AtomToken, Region, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct Atom {
    token: AtomToken,
    region: TokenRegion,
}

impl Region for Atom {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for Atom {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        Ok(Self {
            token: Parse::parse(tokens)?,
            region: tokens.region(start)?,
        })
    }
}

impl Format for Atom {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "{}", self.token.text())?;
        Ok(())
    }
}
