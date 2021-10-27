use crate::cst::expressions::Expr;
use crate::cst::macros::{MacroName, Replacement};
use crate::cst::primitives::{Atom, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Attr {
    Define(DefineAttr),
    Include(IncludeAttr),
    IncludeLib(IncludeLibAttr),
    General(GeneralAttr),
}

impl Region for Attr {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Define(x) => x.region(),
            Self::Include(x) => x.region(),
            Self::IncludeLib(x) => x.region(),
            Self::General(x) => x.region(),
        }
    }
}

impl Parse for Attr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::Define(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Include(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::IncludeLib(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::General(x))
        } else {
            let (_, e) = parser.take_last_error().expect("unreachable");
            // TODO: check position
            Err(e)
        }
    }
}

impl Format for Attr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Define(x) => x.format(fmt),
            Self::Include(x) => x.format(fmt),
            Self::IncludeLib(x) => x.format(fmt),
            Self::General(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct GeneralAttr {
    name: Atom,
    items: Option<Vec<Expr>>,
    region: TokenRegion,
}

impl Region for GeneralAttr {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for GeneralAttr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::Hyphen)?;
        let name = Atom::parse(parser)?;

        let items = if parser.try_expect(Symbol::OpenParen).is_some() {
            let items = parser.parse_items(Symbol::Comma)?;
            parser.expect(Symbol::CloseParen)?;
            Some(items)
        } else {
            None
        };
        parser.expect(Symbol::Dot)?;
        Ok(Self {
            name,
            items,
            region: parser.region(start),
        })
    }
}

impl Format for GeneralAttr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-")?;
        fmt.format(&self.name)?;
        if let Some(items) = &self.items {
            write!(fmt, "(")?;
            fmt.format_children(items, ",")?;
            write!(fmt, ")")?;
        }
        write!(fmt, ".")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IncludeAttr {
    file: String,
    region: TokenRegion,
}

impl Region for IncludeAttr {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for IncludeAttr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::Hyphen)?;
        parser.expect("include")?;
        parser.expect(Symbol::OpenParen)?;
        let file = String::parse(parser)?;
        parser.expect(Symbol::CloseParen)?;
        parser.expect(Symbol::Dot)?;
        Ok(Self {
            file,
            region: parser.region(start),
        })
    }
}

impl Format for IncludeAttr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-include(")?;
        fmt.format_child(&self.file)?;
        write!(fmt, ").")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IncludeLibAttr {
    file: String,
    region: TokenRegion,
}

impl Region for IncludeLibAttr {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for IncludeLibAttr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::Hyphen)?;
        parser.expect("include_lib")?;
        parser.expect(Symbol::OpenParen)?;
        let file = String::parse(parser)?;
        parser.expect(Symbol::CloseParen)?;
        parser.expect(Symbol::Dot)?;
        Ok(Self {
            file,
            region: parser.region(start),
        })
    }
}

impl Format for IncludeLibAttr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-include_lib(")?;
        fmt.format_child(&self.file)?;
        write!(fmt, ").")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct DefineAttr {
    macro_name: MacroName,
    variables: Option<Vec<Variable>>,
    replacement: Replacement,
    region: TokenRegion,
}

impl DefineAttr {
    pub fn macro_name(&self) -> &str {
        match &self.macro_name {
            MacroName::Atom(x) => x.token().value(),
            MacroName::Variable(x) => x.token().value(),
        }
    }

    pub fn variables(&self) -> Option<&[Variable]> {
        self.variables.as_ref().map(|x| x.as_slice())
    }

    pub fn replacement(&self) -> &Replacement {
        &self.replacement
    }
}

impl Region for DefineAttr {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for DefineAttr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::Hyphen)?;
        parser.expect("define")?;
        parser.expect(Symbol::OpenParen)?;

        // Name.
        let macro_name = parser.parse()?;

        // Variables.
        let variables = if parser.try_expect(Symbol::OpenParen).is_some() {
            let variables = parser.parse_items(Symbol::Comma)?;
            parser.expect(Symbol::CloseParen)?;
            Some(variables)
        } else {
            None
        };
        parser.expect(Symbol::Comma)?;

        // Replacement.
        let replacement = parser.parse()?;
        parser.expect(Symbol::CloseParen)?;
        parser.expect(Symbol::Dot)?;

        Ok(Self {
            macro_name,
            variables,
            replacement,
            region: parser.region(start),
        })
    }
}

impl Format for DefineAttr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-define(")?;
        fmt.format(&self.macro_name)?;

        if let Some(vars) = &self.variables {
            write!(fmt, "(")?;
            fmt.format_children(vars, ",")?;
            write!(fmt, ")")?;
        }
        write!(fmt, ",")?;

        fmt.format_child(&self.replacement)?;
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
        let testnames = ["novars-noreplacement", "novars", "vars"];
        for testname in testnames {
            test_parse_and_format::<DefineAttr>(&format!(
                "cst/attributes/define-attr-{}",
                testname
            ))
            .expect(testname);
        }
    }

    #[test]
    fn include_attr_works() {
        test_parse_and_format::<IncludeAttr>("cst/attributes/include-attr").expect("include");
    }

    #[test]
    fn include_lib_attr_works() {
        test_parse_and_format::<IncludeLibAttr>("cst/attributes/include-lib-attr")
            .expect("include-lib");
    }

    #[test]
    fn general_attr_works() {
        let testnames = ["module"];
        for testname in testnames {
            test_parse_and_format::<GeneralAttr>(&format!(
                "cst/attributes/general-attr-{}",
                testname
            ))
            .expect(testname);
        }
    }
}
