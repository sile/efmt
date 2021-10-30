use crate::cst;
use crate::cst::consts::{Dot, Semicolon};
use crate::cst::primitives::NonEmptyItems;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{AtomToken, Region, TokenRegion};
use efmt_derive::{Format, Parse, Region};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct RootItems {
    items: Vec<RootItem>,
}

impl Region for RootItems {
    fn region(&self) -> TokenRegion {
        if let (Some(first), Some(last)) = (self.items.first(), self.items.last()) {
            TokenRegion::new(first.region().start(), last.region().end())
        } else {
            unreachable!()
        }
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

#[derive(Debug, Clone, Region, Parse, Format)]
pub enum RootItem {
    Attr(cst::attributes::Attr),
    FunDecl(FunDecl),
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunDecl {
    clauses: NonEmptyItems<cst::expressions::FunClause<AtomToken>, Semicolon>,
    dot: Dot,
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

    #[test]
    fn examples_works() {
        // http://www1.erlang.org/examples/examples-2.0.html
        let testnames = ["ftp-client", "ftp-server"];
        for testname in testnames {
            test_parse_and_format::<RootItems>(&format!("cst/root/example-{}", testname))
                .expect(testname);
        }
    }
}
