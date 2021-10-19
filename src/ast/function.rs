use crate::{Expect, Lexer, Parse, Result};
use erl_tokenize::tokens::{AtomToken, IntegerToken};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameAndArity<Name = AtomToken, Arity = IntegerToken> {
    name: Name,
    arity: Arity,
}

impl<Name, Arity> Parse for NameAndArity<Name, Arity>
where
    Name: Expect,
    Arity: Expect,
{
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        todo!()
    }
}
