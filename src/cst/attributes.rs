use crate::cst::common::{Atom, String, Variable};
use crate::cst::expressions::Expr;
use crate::cst::macros::MacroName;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Expect, Parse, TokenReader};
use crate::token::{LexicalToken, Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Attr {
    Define(DefineAttr),
    Include(IncludeAttr),
    IncludeLib(IncludeLibAttr),
    General(GeneralAttr),
}

impl Region for Attr {
    fn region(&self) -> TokenRegion {
        match self {
            Self::Define(x) => x.region(),
            Self::Include(x) => x.region(),
            Self::IncludeLib(x) => x.region(),
            Self::General(x) => x.region(),
        }
    }
}

impl Parse for Attr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::Define(x))
        } else if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::Include(x))
        } else if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::IncludeLib(x))
        } else if let Some(x) = Parse::try_parse(tokens) {
            Ok(Self::General(x))
        } else {
            Err(tokens.take_last_error().expect("unreachable"))
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
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for GeneralAttr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        let _ = Symbol::Hyphen.expect(tokens)?;
        let name = Atom::parse(tokens)?;

        let items = if Symbol::OpenParen.try_expect(tokens).is_some() {
            let items = Expr::parse_items(tokens, Symbol::Comma)?;
            let _ = Symbol::CloseParen.expect(tokens)?;
            Some(items)
        } else {
            None
        };
        let _ = Symbol::Dot.expect(tokens)?;
        Ok(Self {
            name,
            items,
            region: tokens.region(start)?,
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
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for IncludeAttr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        let _ = Symbol::Hyphen.expect(tokens)?;
        let _ = "include".expect(tokens)?;
        let _ = Symbol::OpenParen.expect(tokens)?;
        let file = String::parse(tokens)?;
        let _ = Symbol::CloseParen.expect(tokens)?;
        let _ = Symbol::Dot.expect(tokens)?;
        Ok(Self {
            file,
            region: tokens.region(start)?,
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
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for IncludeLibAttr {
    fn parse(tokens: &mut TokenReader) -> parse::Result<Self> {
        let start = tokens.current_index();
        let _ = Symbol::Hyphen.expect(tokens)?;
        let _ = "include_lib".expect(tokens)?;
        let _ = Symbol::OpenParen.expect(tokens)?;
        let file = String::parse(tokens)?;
        let _ = Symbol::CloseParen.expect(tokens)?;
        let _ = Symbol::Dot.expect(tokens)?;
        Ok(Self {
            file,
            region: tokens.region(start)?,
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
    replacement: Vec<LexicalToken>,
    replacement_region: TokenRegion,
    region: TokenRegion,
}

impl DefineAttr {
    pub fn macro_name(&self) -> &str {
        match &self.macro_name {
            MacroName::Atom(x) => x.token().value(),
            MacroName::Variable(x) => x.token().value(),
        }
    }
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

        // Name.
        let macro_name = Parse::parse(tokens)?;

        // Variables.
        let variables = if Symbol::OpenParen.try_expect(tokens).is_some() {
            let variables = Variable::parse_items(tokens, Symbol::Comma)?;
            let _ = Symbol::CloseParen.expect(tokens)?;
            Some(variables)
        } else {
            None
        };
        let _ = Symbol::Comma.expect(tokens)?;

        // Replacement.
        let replacement_start = tokens.current_index();
        let mut replacement_region = tokens.region(replacement_start)?;
        let mut replacement = Vec::new();
        let mut level = 0;
        loop {
            let token = tokens.read_token()?;
            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::OpenParen => {
                    level += 1;
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                    if level == 0 {
                        break;
                    }
                    level -= 1;
                }
                _ => {}
            }
            replacement.push(token);
            replacement_region = tokens.region(replacement_start)?;
        }

        let _ = Symbol::Dot.expect(tokens)?;
        Ok(Self {
            macro_name,
            variables,
            replacement,
            replacement_region,
            region: tokens.region(start)?,
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

        if !self.replacement.is_empty() {
            let mut tokens = TokenReader::new(self.replacement.clone());
            if let Some(expr) = Expr::try_parse(&mut tokens) {
                fmt.format_child(&expr)?;
            } else {
                fmt.write_original_text(self.replacement_region)?;
            }
        }
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
