use crate::ast::ty::Type;
use crate::expect::{ExpectAtom, ExpectVariable, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::values::Symbol;

/// `-` `type|opaque` `AtomToken` `Args<VariableToken>` `::` `Type` `.`
#[derive(Debug, Clone)]
pub struct TypeDecl {
    type_or_opaque: AtomToken,
    type_name: AtomToken,
    params: Vec<VariableToken>,
    ty: Type,
    region: Region,
}

impl Parse for TypeDecl {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let type_or_opaque = lexer.read_expect(Or("type", "opaque"))?.into_token();
        let type_name = lexer.read_expect(ExpectAtom)?;

        let mut params = Vec::new();
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        if lexer.try_read_expect(Symbol::CloseParen)?.is_none() {
            loop {
                params.push(lexer.read_expect(ExpectVariable)?);
                if lexer
                    .read_expect(Or(Symbol::Comma, Symbol::CloseParen))?
                    .is_b()
                {
                    break;
                }
            }
        }

        let _ = lexer.read_expect(Symbol::DoubleColon)?;
        let ty = Type::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        let end = lexer.current_position();
        Ok(Self {
            type_or_opaque,
            type_name,
            params,
            ty,
            region: Region::new(start, end),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_works() {
        let mut lexer = Lexer::new("-type foo() :: atom().");
        let _decl = TypeDecl::parse(&mut lexer).unwrap();

        let mut lexer = Lexer::new("-opaque foo(A) :: list(A).");
        let _decl = TypeDecl::parse(&mut lexer).unwrap();
    }
}
