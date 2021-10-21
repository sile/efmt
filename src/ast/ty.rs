use crate::ast::function::Call;
use crate::expect::{Either, ExpectAtom, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, CharToken, FloatToken, IntegerToken, VariableToken};
use erl_tokenize::values::{Keyword, Symbol};
use erl_tokenize::LexicalToken;

// https://erlang.org/doc/reference_manual/typespec.html
#[derive(Debug, Clone)]
pub enum Type {
    Literal(TypeLiteral),
    Variable(VariableToken),
    Annotated(Box<Annotated>),
    Tuple(Box<Tuple>),
    Map,
    Record,
    List(Box<List>),
    Bits,
    Parenthesized(Box<Parenthesized>),
    TypeCall(Box<Call<AtomToken, Type>>),
    UnaryOpCall(Box<UnaryOpCall>),
    BinaryOpCall,
    Fun(Box<Fun>),
    Range(Box<Range>),
    Union(Box<Union>),
}

impl Type {
    fn parse_except_union(lexer: &mut Lexer) -> Result<Self> {
        if lexer.expect_2tokens(ExpectAtom, Or(Symbol::Colon, Symbol::OpenParen)) {
            return Call::parse(lexer).map(Box::new).map(Self::TypeCall);
        }
        if let Some(x) = Parenthesized::try_parse(lexer) {
            return Ok(Self::Parenthesized(Box::new(x)));
        }
        if let Some(x) = List::try_parse(lexer) {
            return Ok(Self::List(Box::new(x)));
        }
        if let Some(x) = Tuple::try_parse(lexer) {
            return Ok(Self::Tuple(Box::new(x)));
        }
        if let Some(x) = Annotated::try_parse(lexer) {
            return Ok(Self::Annotated(Box::new(x)));
        }
        if let Some(x) = UnaryOpCall::try_parse(lexer) {
            return Ok(Self::UnaryOpCall(Box::new(x)));
        }
        if let Some(x) = Fun::try_parse(lexer) {
            return Ok(Self::Fun(Box::new(x)));
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
        let mut ty = Self::parse_except_union(lexer)?;
        if lexer.try_read_expect(Symbol::DoubleDot).is_some() {
            ty = Self::Range(Box::new(Range {
                min: ty,
                max: Self::parse(lexer)?,
            }));
        }
        if lexer.try_read_expect(Symbol::VerticalBar).is_some() {
            ty = Self::Union(Box::new(Union {
                left: ty,
                right: Self::parse(lexer)?,
            }));
        }
        Ok(ty)
    }
}

#[derive(Debug, Clone)]
pub struct Union {
    left: Type,
    right: Type,
}

/// `Type` `..` `Type`
#[derive(Debug, Clone)]
pub struct Range {
    min: Type,
    max: Type,
}

#[derive(Debug, Clone)]
pub enum TypeLiteral {
    Atom(AtomToken),
    Integer(IntegerToken),
    Char(CharToken),
    Float(FloatToken),
}

impl Parse for TypeLiteral {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        lexer.with_transaction(|lexer| {
            let token = lexer.read_token()?;
            match token {
                LexicalToken::Atom(token) => Ok(Self::Atom(token)),
                LexicalToken::Integer(token) => Ok(Self::Integer(token)),
                LexicalToken::Char(token) => Ok(Self::Char(token)),
                LexicalToken::Float(token) => Ok(Self::Float(token)),
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

/// `VariableToken` `::` `Type`
#[derive(Debug, Clone)]
pub struct Annotated {
    name: VariableToken,
    ty: Type,
    region: Region,
}

impl Parse for Annotated {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = VariableToken::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::DoubleColon)?;
        let ty = Type::parse(lexer)?;
        Ok(Self {
            name,
            ty,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    Plus,
    Minus,
    Bnot,
}

impl Parse for UnaryOp {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_expect(Or(Symbol::Plus, Or(Symbol::Hyphen, "bnot")))? {
            Either::A(_) => Ok(Self::Plus),
            Either::B(Either::A(_)) => Ok(Self::Minus),
            _ => Ok(Self::Bnot),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Parenthesized {
    ty: Type,
    region: Region,
}

impl Parse for Parenthesized {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let ty = Type::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        Ok(Self {
            ty,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOpCall {
    op: UnaryOp,
    ty: Type,
    region: Region,
}

impl Parse for UnaryOpCall {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let op = UnaryOp::parse(lexer)?;
        let ty = Type::parse(lexer)?;
        Ok(Self {
            op,
            ty,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Fun {
    args: Option<Vec<Type>>, // `None` means any arity
    return_type: Type,
    region: Region,
}

impl Parse for Fun {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        // TODO: Support any fun (i.e., 'fun()')
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Fun)?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let args = if lexer.try_read_expect(Symbol::TripleDot).is_some() {
            None
        } else {
            let mut args = Vec::new();
            while let Some(arg) = Type::try_parse(lexer) {
                args.push(arg);
                if lexer.try_read_expect(Symbol::Comma).is_none() {
                    break;
                }
            }
            Some(args)
        };
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let return_type = Type::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        Ok(Self {
            args,
            return_type,
            region: lexer.region(start),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_works() {
        let mut lexer = Lexer::new("{scientific, Decimals :: 0..249}");
        let _ = Type::parse(&mut lexer).unwrap();

        let text = "fun ((term()) -> {ok, json_value()} | error)";
        let mut lexer = Lexer::new(text);
        let _ = Type::parse(&mut lexer).unwrap();
    }
}
