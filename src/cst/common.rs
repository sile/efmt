use crate::format::{self, Format, Formatter};
use crate::lex::Lexer;
use crate::parse::{self, Parse};
use crate::token::{AtomToken, Region, StringToken, TokenRegion, VariableToken};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct Atom {
    token: AtomToken,
    region: TokenRegion,
}

impl Atom {
    pub fn token(&self) -> &AtomToken {
        &self.token
    }
}

impl Region for Atom {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for Atom {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        let start = lexer.current_index();
        Ok(Self {
            token: Parse::parse(lexer)?,
            region: lexer.region(start)?,
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

impl Variable {
    pub fn token(&self) -> &VariableToken {
        &self.token
    }
}

impl Region for Variable {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for Variable {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        let start = lexer.current_index();
        Ok(Self {
            token: Parse::parse(lexer)?,
            region: lexer.region(start)?,
        })
    }
}

impl Format for Variable {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "{}", self.token.text())?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct String {
    token: StringToken,
    region: TokenRegion,
}

impl String {
    pub fn token(&self) -> &StringToken {
        &self.token
    }
}

impl Region for String {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for String {
    fn parse(lexer: &mut Lexer) -> parse::Result<Self> {
        let start = lexer.current_index();
        Ok(Self {
            token: Parse::parse(lexer)?,
            region: lexer.region(start)?,
        })
    }
}

impl Format for String {
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

    #[test]
    fn string_works() {
        for testname in ["string"] {
            test_parse_and_format::<String>(&format!("cst/common/{}", testname)).expect(testname);
        }
    }
}
