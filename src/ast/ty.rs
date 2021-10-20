use crate::ast::function::Call;
use crate::expect::{ExpectAtom, Or};
use crate::{Lexer, Parse, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::values::Symbol;
use erl_tokenize::LexicalToken;

// https://erlang.org/doc/reference_manual/typespec.html
#[derive(Debug, Clone)]
pub enum Type {
    Literal(TypeLiteral),
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
    Union(Box<Union>),
}

impl Type {
    fn parse_except_union(lexer: &mut Lexer) -> Result<Self> {
        if lexer.expect_2tokens(ExpectAtom, Or(Symbol::Colon, Symbol::OpenParen)) {
            return Call::parse(lexer).map(Box::new).map(Self::TypeCall);
        }
        if let Some(token) = VariableToken::try_parse(lexer) {
            return Ok(Self::Variable(token));
        }
        if let Some(literal) = TypeLiteral::try_parse(lexer) {
            return Ok(Self::Literal(literal));
        }
        Err(anyhow::anyhow!("not yet implemented: next={:?}", lexer.read_token()).into())
    }
}

impl Parse for Type {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let ty = Self::parse_except_union(lexer)?;
        if lexer.try_read_expect(Symbol::VerticalBar).is_some() {
            Ok(Self::Union(Box::new(Union {
                left: ty,
                right: Self::parse(lexer)?,
            })))
        } else {
            Ok(ty)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    left: Type,
    right: Type,
}

#[derive(Debug, Clone)]
pub enum TypeLiteral {
    Atom(AtomToken),
    // TODO: Integer, Float, Char
}

impl Parse for TypeLiteral {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        lexer.with_transaction(|lexer| {
            let token = lexer.read_token()?;
            match token {
                LexicalToken::Atom(token) => Ok(Self::Atom(token)),
                _ => Err(anyhow::anyhow!("TODO or unknown literal: {:?}", token).into()),
            }
        })
    }
}
