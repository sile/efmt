use crate::cst::primitives::{Atom, String, Variable};
//use crate::cst::expressions::Expr;
use crate::cst::macros::MacroName;
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{LexicalToken, Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Attr {
    Define(DefineAttr),
    // Include(IncludeAttr),
    // IncludeLib(IncludeLibAttr),
    // General(GeneralAttr),
}

impl Region for Attr {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Define(x) => x.region(),
            // Self::Include(x) => x.region(),
            // Self::IncludeLib(x) => x.region(),
            // Self::General(x) => x.region(),
        }
    }
}

impl Parse for Attr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::Define(x))
        // } else if let Some(x) = Parse::try_parse(parser) {
        //     Ok(Self::Include(x))
        // } else if let Some(x) = Parse::try_parse(parser) {
        //     Ok(Self::IncludeLib(x))
        // } else if let Some(x) = Parse::try_parse(parser) {
        //     Ok(Self::General(x))
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
            // Self::Include(x) => x.format(fmt),
            // Self::IncludeLib(x) => x.format(fmt),
            // Self::General(x) => x.format(fmt),
        }
    }
}

// #[derive(Debug, Clone)]
// pub struct GeneralAttr {
//     name: Atom,
//     items: Option<Vec<Expr>>,
//     region: TokenRegion,
// }

// impl Region for GeneralAttr {
//     fn region(&self) -> TokenRegion {
//         self.region
//     }
// }

// impl Parse for GeneralAttr {
//     fn parse(parser: &mut Parser) -> parse::Result<Self> {
//         let start = parser.current_position();
//         let _ = Symbol::Hyphen.expect(parser)?;
//         let name = Atom::parse(parser)?;

//         let items = if Symbol::OpenParen.try_expect(parser).is_some() {
//             let items = Expr::parse_items(parser, Symbol::Comma)?;
//             let _ = Symbol::CloseParen.expect(parser)?;
//             Some(items)
//         } else {
//             None
//         };
//         let _ = Symbol::Dot.expect(parser)?;
//         Ok(Self {
//             name,
//             items,
//             region: parser.region(start)?,
//         })
//     }
// }

// impl Format for GeneralAttr {
//     fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
//         write!(fmt, "-")?;
//         fmt.format(&self.name)?;
//         if let Some(items) = &self.items {
//             write!(fmt, "(")?;
//             fmt.format_children(items, ",")?;
//             write!(fmt, ")")?;
//         }
//         write!(fmt, ".")?;
//         Ok(())
//     }
// }

// #[derive(Debug, Clone)]
// pub struct IncludeAttr {
//     file: String,
//     region: TokenRegion,
// }

// impl Region for IncludeAttr {
//     fn region(&self) -> TokenRegion {
//         self.region
//     }
// }

// impl Parse for IncludeAttr {
//     fn parse(parser: &mut Parser) -> parse::Result<Self> {
//         let start = parser.current_position();
//         let _ = Symbol::Hyphen.expect(parser)?;
//         let _ = "include".expect(parser)?;
//         let _ = Symbol::OpenParen.expect(parser)?;
//         let file = String::parse(parser)?;
//         let _ = Symbol::CloseParen.expect(parser)?;
//         let _ = Symbol::Dot.expect(parser)?;
//         Ok(Self {
//             file,
//             region: parser.region(start)?,
//         })
//     }
// }

// impl Format for IncludeAttr {
//     fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
//         write!(fmt, "-include(")?;
//         fmt.format_child(&self.file)?;
//         write!(fmt, ").")?;
//         Ok(())
//     }
// }

// #[derive(Debug, Clone)]
// pub struct IncludeLibAttr {
//     file: String,
//     region: TokenRegion,
// }

// impl Region for IncludeLibAttr {
//     fn region(&self) -> TokenRegion {
//         self.region
//     }
// }

// impl Parse for IncludeLibAttr {
//     fn parse(parser: &mut Parser) -> parse::Result<Self> {
//         let start = parser.current_position();
//         let _ = Symbol::Hyphen.expect(parser)?;
//         let _ = "include_lib".expect(parser)?;
//         let _ = Symbol::OpenParen.expect(parser)?;
//         let file = String::parse(parser)?;
//         let _ = Symbol::CloseParen.expect(parser)?;
//         let _ = Symbol::Dot.expect(parser)?;
//         Ok(Self {
//             file,
//             region: parser.region(start)?,
//         })
//     }
// }

// impl Format for IncludeLibAttr {
//     fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
//         write!(fmt, "-include_lib(")?;
//         fmt.format_child(&self.file)?;
//         write!(fmt, ").")?;
//         Ok(())
//     }
// }

#[derive(Debug, Clone)]
pub struct DefineAttr {
    macro_name: MacroName,
    variables: Option<Vec<Variable>>,
    replacement: Vec<LexicalToken>, // TODO: MaybeExpr
    replacement_region: TokenRegion,
    // TODO: replacement_expr: Option<Expr>
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
        let replacement_start = parser.current_position();
        let mut replacement_region = parser.region(replacement_start.clone());
        let mut replacement = Vec::new();
        loop {
            let token = parser.read_token()?;
            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                    if parser.try_expect(Symbol::Dot).is_some() {
                        break;
                    }
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::Dot => {
                    return Err(parse::Error::UnexpectedToken {
                        token: token.clone(),
                        expected: "non '.'",
                    });
                }
                _ => {}
            }
            replacement.push(token);
            replacement_region = parser.region(replacement_start.clone());
        }

        Ok(Self {
            macro_name,
            variables,
            replacement,
            replacement_region,
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

        if !self.replacement.is_empty() {
            fmt.write_original_text(&self.replacement_region)?;
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

    // #[test]
    // fn include_attr_works() {
    //     test_parse_and_format::<IncludeAttr>("cst/attributes/include-attr").expect("include");
    // }

    // #[test]
    // fn include_lib_attr_works() {
    //     test_parse_and_format::<IncludeLibAttr>("cst/attributes/include-lib-attr")
    //         .expect("include-lib");
    // }

    // #[test]
    // fn general_attr_works() {
    //     let testnames = ["module"];
    //     for testname in testnames {
    //         test_parse_and_format::<GeneralAttr>(&format!(
    //             "cst/attributes/general-attr-{}",
    //             testname
    //         ))
    //         .expect(testname);
    //     }
    // }
}
