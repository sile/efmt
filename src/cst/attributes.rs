use crate::cst::expressions::Expr;
use crate::cst::macros::{MacroName, Replacement};
use crate::cst::primitives::{Atom, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Attr {
    Define(Define),
    Include(Include),
    IncludeLib(IncludeLib),
    General(General),
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
            let e = parser.take_last_error().expect("unreachable");
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
pub struct General {
    name: Atom,
    items: Option<Vec<Expr>>,
    region: TokenRegion,
}

impl Region for General {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for General {
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

impl Format for General {
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
pub struct Include {
    file: String,
    region: TokenRegion,
}

impl Region for Include {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Include {
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

impl Format for Include {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-include(")?;
        fmt.format_child(&self.file)?;
        write!(fmt, ").")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IncludeLib {
    file: String,
    region: TokenRegion,
}

impl Region for IncludeLib {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for IncludeLib {
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

impl Format for IncludeLib {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "-include_lib(")?;
        fmt.format_child(&self.file)?;
        write!(fmt, ").")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Define {
    macro_name: MacroName,
    variables: Option<Vec<Variable>>,
    replacement: Replacement,
    region: TokenRegion,
}

impl Define {
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

impl Region for Define {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Define {
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

impl Format for Define {
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
    fn define_works() {
        let testnames = ["novars-noreplacement", "novars", "vars"];
        for testname in testnames {
            test_parse_and_format::<Attr>(&format!("cst/attributes/define-attr-{}", testname))
                .expect(testname);
        }
    }

    #[test]
    fn include_works() {
        test_parse_and_format::<Attr>("cst/attributes/include-attr").expect("include");
    }

    #[test]
    fn include_lib_works() {
        test_parse_and_format::<Attr>("cst/attributes/include-lib-attr").expect("include-lib");
    }

    #[test]
    fn module_works() {
        test_parse_and_format::<Attr>("cst/attributes/module-attr").unwrap()
    }

    #[test]
    fn export_works() {
        test_parse_and_format::<Attr>("cst/attributes/export-attr").unwrap()
    }
}
