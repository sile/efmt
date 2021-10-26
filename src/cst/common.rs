use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, TokenReader};
use crate::token::{AtomToken, Region, TokenRegion, VariableToken};
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

#[derive(Debug, Clone)]
pub struct Variable {
    token: VariableToken,
    region: TokenRegion,
}

impl Region for Variable {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for Variable {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        Ok(Self {
            token: Parse::parse(tokens)?,
            region: tokens.region(start)?,
        })
    }
}

impl Format for Variable {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "{}", self.token.text())?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn atom_works() {
        for testname in ["atom", "quoted-atom"] {
            test_parse_and_format::<Atom>(&format!("cst/common/{}", testname)).expect(testname);
        }
    }

    #[test]
    fn variable_works() {
        for testname in ["variable"] {
            test_parse_and_format::<Variable>(&format!("cst/common/{}", testname)).expect(testname);
        }
    }
}
