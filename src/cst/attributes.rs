use crate::cst::common::Variable;
use crate::cst::macros::MacroName;
use crate::cst::Cst;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Expect, Parse, TokenReader};
use crate::token::{LexicalToken, Region, Symbol, TokenRegion};
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
        write!(fmt, ", ")?;

        if !self.replacement.is_empty() {
            let mut tokens = TokenReader::new(self.replacement.clone());
            if let Some(cst) = Cst::try_parse(&mut tokens) {
                fmt.format(&cst)?;
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
}
