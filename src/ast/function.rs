use crate::expect::{ExpectAtom, ExpectNonNegInteger};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, IntegerToken};
use erl_tokenize::values::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameAndArity<Name = AtomToken, Arity = IntegerToken> {
    name: Name,
    arity: Arity,
    region: Region,
}

impl Parse for NameAndArity<AtomToken, IntegerToken> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = lexer.read_expect(ExpectAtom)?;
        let _ = lexer.read_expect(Symbol::Slash)?;
        let arity = lexer.read_expect(ExpectNonNegInteger)?;
        let end = lexer.current_position();
        Ok(Self {
            name,
            arity,
            region: Region::new(start, end),
        })
    }
}
