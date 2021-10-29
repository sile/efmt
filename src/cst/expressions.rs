use crate::cst::consts::{CloseSquare, Colon, Comma, OpenSquare, RightArrow};
use crate::cst::primitives::{Child, Items, Maybe, NameAndArity, NonEmptyItems, Parenthesized};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Either, Parse, Parser};
use crate::token::{
    AtomToken, CharToken, FloatToken, IntegerToken, Keyword, Region, StringToken, TokenRegion,
    VariableToken,
};
use efmt_derive::{Format, Parse, Region};
use std::io::Write;

#[derive(Debug, Clone, Region, Format)]
pub enum Expr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringToken),
    Variable(VariableToken),
    List(Box<List>),
    FunCall(Box<FunCall>),
    NameAndArity(NameAndArity<AtomToken, IntegerToken>), // For attributes such as `-export`.
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let expr = if let Some(x) = parser.try_parse() {
            Self::NameAndArity(x)
        } else if let Some(x) = parser.try_parse() {
            Self::FunCall(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Atom(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Char(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Float(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Integer(x)
        } else if let Some(x) = parser.try_parse() {
            Self::String(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Variable(x)
        } else if let Some(x) = parser.try_parse() {
            Self::List(x)
        } else {
            let e = parser.take_last_error().expect("unreachable");
            return Err(e);
        };
        Ok(expr)
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct List {
    open: OpenSquare,
    items: Child<Items<Expr, Comma>>,
    close: CloseSquare,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunClause<Name> {
    name: Name,
    params: Parenthesized<Items<Expr, Comma>>,
    guard: Maybe<Guard>,
    arrow: RightArrow,
    body: Body,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Body {
    exprs: NonEmptyItems<Expr, Comma>,
}

#[derive(Debug, Clone)]
pub struct Guard {}

impl Region for Guard {
    fn region(&self) -> TokenRegion {
        todo!()
    }
}

impl Parse for Guard {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::When)?;
        todo!()
    }
}

impl Format for Guard {
    fn format<W: Write>(&self, _fmt: &mut Formatter<W>) -> format::Result<()> {
        todo!()
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunCallName {
    name: Either<Parenthesized<Expr>, Either<AtomToken, VariableToken>>,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunCallModule {
    module: Either<Parenthesized<Expr>, Either<AtomToken, VariableToken>>,
    colon: Colon,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunCall {
    module: Maybe<FunCallModule>,
    name: FunCallName,
    args: Parenthesized<Items<Expr, Comma>>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn list_works() {
        for testname in ["atom"] {
            test_parse_and_format::<Expr>(&format!("cst/expressions/list-{}", testname))
                .expect(testname);
        }
    }
}
