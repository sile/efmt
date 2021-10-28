use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{
    AtomToken, IntegerToken, Region, StringToken, Symbol, TokenRegion, VariableToken,
};
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
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Atom {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        Ok(Self {
            token: parser.parse()?,
            region: parser.region(start),
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
pub struct Integer {
    token: IntegerToken,
    region: TokenRegion,
}

impl Integer {
    pub fn token(&self) -> &IntegerToken {
        &self.token
    }
}

impl Region for Integer {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Integer {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        Ok(Self {
            token: parser.parse()?,
            region: parser.region(start),
        })
    }
}

impl Format for Integer {
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
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Variable {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        Ok(Self {
            token: Parse::parse(parser)?,
            region: parser.region(start),
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
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for String {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        Ok(Self {
            token: Parse::parse(parser)?,
            region: parser.region(start),
        })
    }
}

impl Format for String {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "{}", self.token.text())?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct NameAndArity<Name, Arity> {
    name: Name,
    arity: Arity,
    region: TokenRegion,
}

impl<Name, Arity> Region for NameAndArity<Name, Arity> {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl<Name, Arity> Parse for NameAndArity<Name, Arity>
where
    Name: Parse,
    Arity: Parse,
{
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let name = parser.parse()?;
        parser.expect(Symbol::Slash)?;
        let arity = parser.parse()?;
        Ok(Self {
            name,
            arity,
            region: parser.region(start),
        })
    }
}

impl<Name, Arity> Format for NameAndArity<Name, Arity>
where
    Name: Format,
    Arity: Format,
{
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.name)?;
        write!(fmt, "/")?;
        fmt.format(&self.arity)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Parenthesized<T> {
    item: T,
    region: TokenRegion,
}

impl<T> Region for Parenthesized<T> {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl<T: Parse> Parse for Parenthesized<T> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::CloseParen)?;
        let item = parser.parse()?;
        parser.expect(Symbol::CloseParen)?;
        Ok(Self {
            item,
            region: parser.region(start),
        })
    }
}

impl<T: Format> Format for Parenthesized<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "(")?;
        fmt.format(&self.item)?;
        write!(fmt, ")")?;
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
            test_parse_and_format::<Atom>(&format!("cst/primitives/{}", testname)).expect(testname);
        }
    }

    #[test]
    fn variable_works() {
        for testname in ["variable"] {
            test_parse_and_format::<Variable>(&format!("cst/primitives/{}", testname))
                .expect(testname);
        }
    }

    #[test]
    fn string_works() {
        for testname in ["string"] {
            test_parse_and_format::<String>(&format!("cst/primitives/{}", testname))
                .expect(testname);
        }
    }
}
