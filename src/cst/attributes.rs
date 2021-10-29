use crate::cst::consts::{CloseParen, Comma, Dot, Hyphen, OpenParen};
use crate::cst::expressions::Expr;
use crate::cst::macros::{MacroName, Replacement};
use crate::cst::primitives::{Items, Maybe, Parenthesized};
use crate::parse::{self, Parse, Parser};
use crate::token::{AtomToken, StringToken, VariableToken};
use efmt_derive::{Format, Parse, Region};

#[derive(Debug, Clone, Region, Format)]
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

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct General {
    hyphen: Hyphen,
    name: AtomToken,
    items: Maybe<Parenthesized<Items<Expr, Comma>>>,
    dot: Dot,
}

#[derive(Debug, Clone, Region, Format)]
struct AttrNameInclude(AtomToken);

impl Parse for AttrNameInclude {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect("include").map(Self)
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Include {
    hyphen: Hyphen,
    include: AttrNameInclude,
    open: OpenParen,
    file: StringToken,
    close: CloseParen,
    dot: Dot,
}

#[derive(Debug, Clone, Region, Format)]
struct AttrNameIncludeLib(AtomToken);

impl Parse for AttrNameIncludeLib {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect("include_lib").map(Self)
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct IncludeLib {
    hyphen: Hyphen,
    include_lib: AttrNameIncludeLib,
    open: OpenParen,
    file: StringToken,
    close: CloseParen,
    dot: Dot,
}

#[derive(Debug, Clone, Region, Format)]
struct AttrNameDefine(AtomToken);

impl Parse for AttrNameDefine {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect("define").map(Self)
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Define {
    hyphen: Hyphen,
    define: AttrNameDefine,
    open: OpenParen,
    macro_name: MacroName,
    variables: Maybe<Parenthesized<Items<VariableToken, Comma>>>,
    comma: Comma,
    replacement: Replacement,
    close: CloseParen,
    dot: Dot,
}

impl Define {
    pub fn macro_name(&self) -> &str {
        match &self.macro_name {
            MacroName::Atom(x) => x.value(),
            MacroName::Variable(x) => x.value(),
        }
    }

    pub fn variables(&self) -> Option<&[VariableToken]> {
        self.variables.get().map(|x| x.get().items())
    }

    pub fn replacement(&self) -> &Replacement {
        &self.replacement
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
