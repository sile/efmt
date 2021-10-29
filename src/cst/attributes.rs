use crate::cst::expressions::Expr;
use crate::cst::macros::{MacroName, Replacement};
use crate::cst::primitives::{Comma, Items, Parenthesized};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{AtomToken, StringToken, Symbol, SymbolToken, VariableToken};
use efmt_derive::Region;
use std::io::Write;

#[derive(Debug, Clone, Region)]
pub enum Attr {
    Define(Define),
    Include(Include),
    IncludeLib(IncludeLib),
    General(General),
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

#[derive(Debug, Clone, Region)]
pub struct General {
    hyphen: SymbolToken,
    name: AtomToken,
    items: Option<Parenthesized<Items<Expr, Comma>>>,
    dot: SymbolToken,
}

impl Parse for General {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            hyphen: parser.expect(Symbol::Hyphen)?,
            name: parser.parse()?,
            items: parser.try_parse(),
            dot: parser.expect(Symbol::Dot)?,
        })
    }
}

impl Format for General {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.hyphen)?;
        fmt.format(&self.name)?;
        fmt.format_option(&self.items)?;
        fmt.format(&self.dot)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region)]
pub struct Include {
    hyphen: SymbolToken,
    include: AtomToken,
    open: SymbolToken,
    file: StringToken,
    close: SymbolToken,
    dot: SymbolToken,
}

impl Parse for Include {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            hyphen: parser.expect(Symbol::Hyphen)?,
            include: parser.expect("include")?,
            open: parser.expect(Symbol::OpenParen)?,
            file: parser.parse()?,
            close: parser.expect(Symbol::CloseParen)?,
            dot: parser.expect(Symbol::Dot)?,
        })
    }
}

impl Format for Include {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.hyphen)?;
        fmt.format(&self.include)?;
        fmt.format(&self.open)?;
        fmt.format_child(&self.file)?;
        fmt.format(&self.close)?;
        fmt.format(&self.dot)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region)]
pub struct IncludeLib {
    hyphen: SymbolToken,
    include_lib: AtomToken,
    open: SymbolToken,
    file: StringToken,
    close: SymbolToken,
    dot: SymbolToken,
}

impl Parse for IncludeLib {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            hyphen: parser.expect(Symbol::Hyphen)?,
            include_lib: parser.expect("include_lib")?,
            open: parser.expect(Symbol::OpenParen)?,
            file: parser.parse()?,
            close: parser.expect(Symbol::CloseParen)?,
            dot: parser.expect(Symbol::Dot)?,
        })
    }
}

impl Format for IncludeLib {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.hyphen)?;
        fmt.format(&self.include_lib)?;
        fmt.format(&self.open)?;
        fmt.format_child(&self.file)?;
        fmt.format(&self.close)?;
        fmt.format(&self.dot)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region)]
pub struct Define {
    hyphen: SymbolToken,
    define: AtomToken,
    open: SymbolToken,
    macro_name: MacroName,
    variables: Option<Parenthesized<Items<VariableToken, Comma>>>,
    comma: SymbolToken,
    replacement: Replacement,
    close: SymbolToken,
    dot: SymbolToken,
}

impl Define {
    pub fn macro_name(&self) -> &str {
        match &self.macro_name {
            MacroName::Atom(x) => x.value(),
            MacroName::Variable(x) => x.value(),
        }
    }

    pub fn variables(&self) -> Option<&[VariableToken]> {
        self.variables.as_ref().map(|x| x.get().items())
    }

    pub fn replacement(&self) -> &Replacement {
        &self.replacement
    }
}

impl Parse for Define {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            hyphen: parser.expect(Symbol::Hyphen)?,
            define: parser.expect("define")?,
            open: parser.expect(Symbol::OpenParen)?,
            macro_name: parser.parse()?,
            variables: parser.try_parse(),
            comma: parser.expect(Symbol::Comma)?,
            replacement: parser.parse()?,
            close: parser.expect(Symbol::CloseParen)?,
            dot: parser.expect(Symbol::Dot)?,
        })
    }
}

impl Format for Define {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.hyphen)?;
        fmt.format(&self.define)?;
        fmt.format(&self.open)?;
        fmt.format(&self.macro_name)?;
        fmt.format_option(&self.variables)?;
        fmt.format(&self.comma)?;
        fmt.format_child(&self.replacement)?;
        fmt.format(&self.close)?;
        fmt.format(&self.dot)?;
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
