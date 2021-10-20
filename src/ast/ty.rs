use crate::ast::function::Call;
use crate::expect::{ExpectAtom, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::values::Symbol;
use erl_tokenize::LexicalToken;

// https://erlang.org/doc/reference_manual/typespec.html
#[derive(Debug, Clone)]
pub enum Type {
    Literal(TypeLiteral),
    Variable(VariableToken),
    Annotated,
    Tuple(Box<Tuple>),
    Map,
    Record,
    List(Box<List>),
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
        if let Some(x) = List::try_parse(lexer) {
            return Ok(Self::List(Box::new(x)));
        }
        if let Some(x) = Tuple::try_parse(lexer) {
            return Ok(Self::Tuple(Box::new(x)));
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

#[derive(Debug, Clone)]
pub enum List {
    Empty { region: Region },                   // []
    Proper { element: Type, region: Region },   // [A]
    NonEmpty { element: Type, region: Region }, // [A, ...]
}

impl Parse for List {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenSquare)?;
        if lexer.try_read_expect(Symbol::CloseSquare).is_some() {
            return Ok(Self::Empty {
                region: Region::new(start, lexer.current_position()),
            });
        }

        let element = Type::parse(lexer)?;
        if lexer
            .read_expect(Or(Symbol::CloseSquare, Symbol::Comma))?
            .is_a()
        {
            Ok(Self::Proper {
                element,
                region: Region::new(start, lexer.current_position()),
            })
        } else {
            let _ = lexer.read_expect(Symbol::TripleDot)?;
            let _ = lexer.read_expect(Symbol::CloseSquare)?;
            Ok(Self::NonEmpty {
                element,
                region: Region::new(start, lexer.current_position()),
            })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tuple {
    elements: Vec<Type>,
    region: Region,
}

impl Parse for Tuple {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenBrace)?;
        let mut elements = Vec::new();
        while let Some(element) = Type::try_parse(lexer) {
            elements.push(element);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::CloseBrace)?;
        Ok(Self {
            elements,
            region: Region::new(start, lexer.current_position()),
        })
    }
}
