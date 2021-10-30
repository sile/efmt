use crate::cst::consts::{
    self, CloseBrace, CloseSquare, Colon, Comma, OpenBrace, OpenSquare, RightArrow,
};
use crate::cst::primitives::{
    EnterBlock, Items, LeaveBlock, Maybe, NameAndArity, NeedNewline, NeedRightSpace, NeedSpace,
    NewlineIfMultiline, NonEmptyItems, Parenthesized,
};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Either, Parse, Parser, ResumeParse};
use crate::token::{
    AtomToken, CharToken, FloatToken, IntegerToken, Keyword, Region, StringToken, Symbol, Token,
    TokenRegion, VariableToken,
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
    Tuple(Box<Tuple>),
    FunCall(Box<FunCall>),
    BinaryOpCall(Box<BinaryOpCall>),
    Case(Box<Case>),
    If(Box<If>),
    Receive(Box<Receive>),
    Catch(Box<Catch>),
    Parenthesized(Box<Parenthesized<Expr>>),
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
        } else if let Some(x) = parser.try_parse() {
            Self::Tuple(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Case(x)
        } else if let Some(x) = parser.try_parse() {
            Self::If(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Receive(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Catch(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Parenthesized(x)
        } else {
            let e = parser.take_last_error().expect("unreachable");
            return Err(e);
        };

        if let Some(x) = parser.try_resume_parse(expr.clone()) {
            Ok(Self::BinaryOpCall(x))
        } else {
            Ok(expr)
        }
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Catch {
    catch: NeedRightSpace<consts::Catch>,
    expr: Expr,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct If {
    k_if: EnterBlock<consts::If>,
    clauses: NonEmptyItems<IfClause, NeedNewline<consts::Semicolon>>,
    end: LeaveBlock<consts::End>,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct IfClause {
    condition: Expr,
    arrow: NeedSpace<RightArrow>,
    body: Body,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Case {
    case: consts::Case,
    pattern: NeedSpace<Expr>,
    of: EnterBlock<consts::Of>,
    clauses: Items<Clause, NeedNewline<consts::Semicolon>>,
    end: LeaveBlock<consts::End>,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Receive {
    receive: EnterBlock<consts::Receive>,
    clauses: Items<Clause, NeedNewline<consts::Semicolon>>,
    after: Maybe<ReceiveAfter>,
    end: LeaveBlock<consts::End>,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct ReceiveAfter {
    after: LeaveBlock<EnterBlock<consts::After>>,
    clause: Clause,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct BinaryOpCall {
    left: Expr,
    op: NeedSpace<BinaryOp>,
    right: NewlineIfMultiline<Expr>,
}

impl ResumeParse<Expr> for BinaryOpCall {
    fn resume_parse(parser: &mut Parser, left: Expr) -> parse::Result<Self> {
        Ok(Self {
            left,
            op: parser.parse()?,
            right: parser.parse()?,
        })
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct BinaryOp(Token);

impl Parse for BinaryOp {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let token = parser.read_token()?;
        let ok = match &token {
            Token::Symbol(x) => matches!(
                x.value(),
                Symbol::Not
                    | Symbol::Plus
                    | Symbol::Hyphen
                    | Symbol::Multiply
                    | Symbol::Slash
                    | Symbol::Eq
                    | Symbol::ExactEq
                    | Symbol::ExactNotEq
                    | Symbol::Less
                    | Symbol::LessEq
                    | Symbol::Greater
                    | Symbol::GreaterEq
                    | Symbol::Match
            ),
            Token::Keyword(x) => matches!(
                x.value(),
                Keyword::Bor
                    | Keyword::Band
                    | Keyword::Bsl
                    | Keyword::Bsr
                    | Keyword::Div
                    | Keyword::Rem
                    | Keyword::Or
                    | Keyword::Orelse
                    | Keyword::And
                    | Keyword::Andalso
            ),
            _ => false,
        };
        if !ok {
            return Err(parse::Error::unexpected_token(
                parser,
                token,
                "binary operator",
            ));
        }
        Ok(Self(token))
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct List {
    open: OpenSquare,
    items: Items<Expr, NeedRightSpace<Comma>>,
    close: CloseSquare,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Tuple {
    open: OpenBrace,
    items: Items<Expr, NeedRightSpace<Comma>>,
    close: CloseBrace,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct FunClause<Name> {
    name: Name,
    params: Parenthesized<Items<Expr, NeedRightSpace<Comma>>>,
    guard: Maybe<Guard>,
    arrow: NeedSpace<RightArrow>,
    body: Body,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Clause {
    pattern: Expr,
    guard: Maybe<Guard>,
    arrow: NeedSpace<RightArrow>,
    body: Body,
}

#[derive(Debug, Clone, Region, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Expr, Comma>,
}

impl Body {
    pub fn exprs(&self) -> &[Expr] {
        self.exprs.items()
    }

    pub fn delimiters(&self) -> &[Comma] {
        self.exprs.delimiters()
    }
}

impl Format for Body {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_body(self)?;
        Ok(())
    }
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
    args: Parenthesized<Items<Expr, NeedRightSpace<Comma>>>,
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

    #[test]
    fn match_works() {
        for testname in ["too-long-value"] {
            test_parse_and_format::<Expr>(&format!("cst/expressions/match-{}", testname))
                .expect(testname);
        }
    }
}
