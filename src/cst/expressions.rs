use crate::cst::primitives::{Atom, Integer, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, Symbol, TokenRegion};
use std::io::Write;

pub type AtomExpr = Atom;
pub type VariableExpr = Variable;
pub type StringExpr = String;
pub type IntegerExpr = Integer;

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(AtomExpr),
    Variable(VariableExpr),
    String(StringExpr),
    Integer(IntegerExpr),
    List(Box<ListExpr>),
    // For attributes such as `-export`.
    NameAndArity(NameAndArity<AtomExpr, IntegerExpr>),
}

impl Region for Expr {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
            Self::String(x) => x.region(),
            Self::Integer(x) => x.region(),
            Self::List(x) => x.region(),
            Self::NameAndArity(x) => x.region(),
        }
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::NameAndArity(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Atom(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Variable(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::String(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Integer(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::List(x))
        } else {
            let e = parser.take_last_error().expect("unreachable");
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
            Self::Integer(x) => x.format(fmt),
            Self::List(x) => x.format(fmt),
            Self::NameAndArity(x) => x.format(fmt),
        }
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
pub struct ListExpr {
    items: Vec<Expr>,
    region: TokenRegion,
}

impl Region for ListExpr {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for ListExpr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::OpenSquare)?;
        let items = parser.parse_items(Symbol::Comma)?;
        parser.expect(Symbol::CloseSquare)?;
        Ok(Self {
            items,
            region: parser.region(start),
        })
    }
}

impl Format for ListExpr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "[")?;
        fmt.format_children(&self.items, ",")?;
        write!(fmt, "]")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn list_works() {
        for testname in ["atom"] {
            test_parse_and_format::<Expr>(&format!("cst/expressions/list-{}", testname))
                .expect(testname);
        }
    }
}
