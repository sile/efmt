use crate::ast::function::NameAndArity;
use crate::expect::{Either, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::values::Symbol;

/// `-` `export_type` `(` `ProperList<NameAndArity>` `)` `.`
#[derive(Debug, Clone)]
pub struct ExportTypeAttr {
    exports: Vec<NameAndArity>,
    region: Region,
}

impl ExportTypeAttr {
    pub fn exports(&self) -> &[NameAndArity] {
        &self.exports
    }
}

impl Parse for ExportTypeAttr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("export_type")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let _ = lexer.read_expect(Symbol::OpenSquare)?;

        let mut exports = Vec::new();
        if lexer.try_read_expect(Symbol::CloseSquare).is_none() {
            loop {
                let export = NameAndArity::parse(lexer)?;
                exports.push(export);

                if matches!(
                    lexer.read_expect(Or(Symbol::Comma, Symbol::CloseSquare))?,
                    Either::B(_)
                ) {
                    break;
                }
            }
        }

        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        let end = lexer.current_position();
        Ok(Self {
            exports,
            region: Region::new(start, end),
        })
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn parse_works() {
//         let mut lexer = Lexer::new("-export_type([foo/3, bar/0]).");
//         let attr = ExportTypeAttr::parse(&mut lexer).unwrap();
//         assert_eq!(attr.exports().len(), 2);
//     }
// }
