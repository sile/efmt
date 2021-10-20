use crate::ast::function::Call;
use crate::expect::{ExpectAtom, Or};
use crate::{Lexer, Parse, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::values::Symbol;

#[derive(Debug, Clone)]
pub enum Type {
    Literal,
    Variable(VariableToken),
    Annotated,
    Tuple,
    Map,
    Record,
    List,
    Bits,
    Parenthesized,
    TypeCall(Box<Call<AtomToken, Type>>),
    UnaryOpCall,
    BinaryOpCall,
    Fun,
    Range,
    Union,
}

impl Parse for Type {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if lexer.expect_2tokens(ExpectAtom, Or(Symbol::Colon, Symbol::OpenParen)) {
            return Call::parse(lexer).map(Box::new).map(Type::TypeCall);
        }
        if let Some(token) = VariableToken::try_parse(lexer) {
            return Ok(Type::Variable(token));
        }
        Err(anyhow::anyhow!("not yet implemented: next={:?}", lexer.read_token()).into())
    }
}
