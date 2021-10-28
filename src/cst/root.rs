use crate::cst;
use crate::cst::primitives::Atom;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct RootItems {
    items: Vec<RootItem>,
    region: TokenRegion,
}

impl Region for RootItems {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for RootItems {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let mut items = Vec::new();
        while !parser.is_eof()? {
            items.push(parser.parse()?);
        }
        Ok(Self {
            items,
            region: parser.region(start),
        })
    }
}

impl Format for RootItems {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        for item in &self.items {
            fmt.format(item)?;
            writeln!(fmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum RootItem {
    Attr(cst::attributes::Attr),
    FunDecl(FunDecl),
}

impl Region for RootItem {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Attr(x) => x.region(),
            Self::FunDecl(x) => x.region(),
        }
    }
}

impl Parse for RootItem {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::Attr(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::FunDecl(x))
        } else {
            Err(parser.take_last_error().expect("unreachable"))
        }
    }
}

impl Format for RootItem {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Attr(x) => x.format(fmt),
            Self::FunDecl(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    clauses: Vec<cst::expressions::FunClause<Atom>>,
    region: TokenRegion,
}

impl Region for FunDecl {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for FunDecl {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let clauses = parser.parse_items(Symbol::Semicolon)?;
        parser.expect(Symbol::Dot)?;
        Ok(Self {
            clauses,
            region: parser.region(start),
        })
    }
}

impl Format for FunDecl {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_clauses(&self.clauses)?;
        write!(fmt, ".")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn root_items_works() {
        let testnames = ["weird-macro"];
        for testname in testnames {
            test_parse_and_format::<RootItems>(&format!("cst/root/{}", testname)).expect(testname);
        }
    }
}
