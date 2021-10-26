use crate::cst::common::Atom;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Expect, Parse, TokenReader};
use crate::token::{Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Attr {
    Define(DefineAttr),
}

impl Region for Attr {
    fn region(&self) -> TokenRegion {
        match self {
            Self::Define(x) => x.region(),
        }
    }
}

impl Parse for Attr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::Define(x))
        } else {
            Err(tokens.take_last_error().expect("unreachable"))
        }
    }
}

impl Format for Attr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Define(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct DefineAttr {
    macro_name: Atom,
    region: TokenRegion,
}

impl Region for DefineAttr {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for DefineAttr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        let _ = Symbol::Hyphen.expect(tokens)?;
        let _ = "define".expect(tokens)?;
        let _ = Symbol::OpenParen.expect(tokens)?;
        let macro_name = Parse::parse(tokens)?;
        // TODO: try parsing args
        let _ = Symbol::Comma.expect(tokens)?;
        // TODO: replacement
        let _ = Symbol::CloseParen.expect(tokens)?;
        let _ = Symbol::Dot.expect(tokens)?;
        Ok(Self {
            macro_name,
            region: tokens.region(start)?,
        })
    }
}

impl Format for DefineAttr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-define(")?;
        fmt.format(&self.macro_name)?;
        // TODO: ags
        write!(fmt, ",")?;
        // TODO: replacement
        write!(fmt, ").")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn define_attr_works() {
        test_parse_and_format::<DefineAttr>("cst/attributes/define-attr-noargs-noreplacement")
            .expect("noargs-noreplacement");
    }
}
