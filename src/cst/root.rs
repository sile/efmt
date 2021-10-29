use crate::cst;
use crate::cst::primitives::{NonEmptyItems, Semicolon};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{AtomToken, Region, Symbol, SymbolToken, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct RootItems {
    items: Vec<RootItem>,
}

impl Region for RootItems {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(
            self.items[0].region().start(),
            self.items[self.items.len() - 1].region().end(),
        )
    }
}

impl Parse for RootItems {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let mut items = vec![parser.parse()?];
        while !parser.is_eof()? {
            items.push(parser.parse()?);
        }
        Ok(Self { items })
    }
}

impl Format for RootItems {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        for item in &self.items {
            fmt.format_toplevel_item(item)?;
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
    fn region(&self) -> TokenRegion {
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
    clauses: NonEmptyItems<cst::expressions::FunClause<AtomToken>, Semicolon>,
    dot: SymbolToken,
}

impl Region for FunDecl {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.clauses.region().start(), self.dot.region().end())
    }
}

impl Parse for FunDecl {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            clauses: parser.parse()?,
            dot: parser.expect(Symbol::Dot)?,
        })
    }
}

impl Format for FunDecl {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.clauses)?;
        fmt.format(&self.dot)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn root_items_works() {
        let testnames = ["weird-macro", "empty-macro"];
        for testname in testnames {
            test_parse_and_format::<RootItems>(&format!("cst/root/{}", testname)).expect(testname);
        }
    }
}
