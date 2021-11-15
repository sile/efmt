use crate::format::{self, Format};
use crate::items::expressions::{BaseExpr, Expr, NonLeftRecursiveExpr};
use crate::items::generics::{Args, Maybe};
use crate::items::keywords;
use crate::items::styles::{Child, RightSpace, Space};
use crate::items::symbols::{self, ColonSymbol};
use crate::items::tokens::Token;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionCallExpr {
    module: Maybe<(BaseExpr, ColonSymbol)>,
    function: BaseExpr,
    args: Args<Expr>,
}

impl ResumeParse<(BaseExpr, bool)> for FunctionCallExpr {
    fn resume_parse(
        ts: &mut parse::TokenStream,
        (expr, is_remote): (BaseExpr, bool),
    ) -> parse::Result<Self> {
        if is_remote {
            Ok(Self {
                module: Maybe::from_item((expr, ts.parse()?)),
                function: ts.parse()?,
                args: ts.parse()?,
            })
        } else {
            Ok(Self {
                module: Maybe::from_position(expr.start_position()),
                function: expr,
                args: ts.parse()?,
            })
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpCallExpr {
    op: UnaryOp,
    expr: Expr,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Not(RightSpace<keywords::NotKeyword>),
    Bnot(RightSpace<keywords::BnotKeyword>),
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpCallExpr {
    left: Child<NonLeftRecursiveExpr>,
    op: Space<BinaryOp>,
    right: Expr,
}

impl ResumeParse<NonLeftRecursiveExpr> for BinaryOpCallExpr {
    fn resume_parse(
        ts: &mut parse::TokenStream,
        left: NonLeftRecursiveExpr,
    ) -> parse::Result<Self> {
        Ok(Self {
            left: Child(left),
            op: ts.parse()?,
            right: ts.parse()?,
        })
    }
}

impl BinaryOpCallExpr {
    fn is_name_and_arity(&self) -> bool {
        self.left.get().is_atom_token()
            && matches!(self.op.get(), BinaryOp::FloatDiv(_))
            && self.right.is_integer_token()
    }
}

impl Format for BinaryOpCallExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.is_name_and_arity() {
            self.left.format(fmt)?;
            self.op.get().format(fmt)?;
            self.right.format(fmt)?;
            return Ok(());
        }

        self.left.format(fmt)?;
        self.op.format(fmt)?;
        if fmt
            .subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .enter(|fmt| self.format_right(fmt, false))
            .is_err()
        {
            fmt.subregion().enter(|fmt| self.format_right(fmt, true))?;
        }
        Ok(())
    }
}

impl BinaryOpCallExpr {
    fn format_right(&self, fmt: &mut format::Formatter, multi_line: bool) -> format::Result<()> {
        if !multi_line {
            self.right.format(fmt)?;
        } else if matches!(self.op.get(), BinaryOp::Send(_) | BinaryOp::Match(_)) {
            fmt.subregion().indent_offset(4).enter(|fmt| {
                fmt.write_newline()?;
                self.right.format(fmt)
            })?;
        } else {
            fmt.subregion().enter(|fmt| {
                fmt.write_newline()?;
                self.right.format(fmt)
            })?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Format)]
pub enum BinaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Mul(symbols::MultiplySymbol),
    FloatDiv(symbols::SlashSymbol),
    PlusPlus(symbols::PlusPlusSymbol),
    MinusMinus(symbols::MinusMinusSymbol),
    Match(symbols::MatchSymbol),
    Eq(symbols::EqSymbol),
    ExactEq(symbols::ExactEqSymbol),
    NotEq(symbols::NotEqSymbol),
    ExactNotEq(symbols::ExactNotEqSymbol),
    Less(symbols::LessSymbol),
    LessEq(symbols::LessEqSymbol),
    Greater(symbols::GreaterSymbol),
    GreaterEq(symbols::GreaterEqSymbol),
    Send(symbols::NotSymbol),
    IntDiv(keywords::DivKeyword),
    Rem(keywords::RemKeyword),
    Bor(keywords::BorKeyword),
    Bxor(keywords::BxorKeyword),
    Band(keywords::BandKeyword),
    Bsl(keywords::BslKeyword),
    Bsr(keywords::BsrKeyword),
    Or(keywords::OrKeyword),
    Xor(keywords::XorKeyword),
    And(keywords::AndKeyword),
    Andalso(keywords::AndalsoKeyword),
    Orelse(keywords::OrelseKeyword),
}

impl Parse for BinaryOp {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        match ts.peek::<Token>() {
            Some(Token::Symbol(token)) => match token.value() {
                Symbol::Plus => ts.parse().map(Self::Plus),
                Symbol::Hyphen => ts.parse().map(Self::Minus),
                Symbol::Multiply => ts.parse().map(Self::Mul),
                Symbol::Slash => ts.parse().map(Self::FloatDiv),
                Symbol::PlusPlus => ts.parse().map(Self::PlusPlus),
                Symbol::MinusMinus => ts.parse().map(Self::MinusMinus),
                Symbol::Match => ts.parse().map(Self::Match),
                Symbol::Eq => ts.parse().map(Self::Eq),
                Symbol::ExactEq => ts.parse().map(Self::ExactEq),
                Symbol::NotEq => ts.parse().map(Self::NotEq),
                Symbol::ExactNotEq => ts.parse().map(Self::ExactNotEq),
                Symbol::Less => ts.parse().map(Self::Less),
                Symbol::LessEq => ts.parse().map(Self::LessEq),
                Symbol::Greater => ts.parse().map(Self::Greater),
                Symbol::GreaterEq => ts.parse().map(Self::GreaterEq),
                Symbol::Not => ts.parse().map(Self::Send),
                _ => Err(parse::Error::unexpected_token(ts, token.into())),
            },
            Some(Token::Keyword(token)) => match token.value() {
                Keyword::Div => ts.parse().map(Self::IntDiv),
                Keyword::Rem => ts.parse().map(Self::Rem),
                Keyword::Bor => ts.parse().map(Self::Bor),
                Keyword::Bxor => ts.parse().map(Self::Bxor),
                Keyword::Band => ts.parse().map(Self::Band),
                Keyword::Bsl => ts.parse().map(Self::Bsl),
                Keyword::Bsr => ts.parse().map(Self::Bsr),
                Keyword::Or => ts.parse().map(Self::Or),
                Keyword::Xor => ts.parse().map(Self::Xor),
                Keyword::And => ts.parse().map(Self::And),
                Keyword::Andalso => ts.parse().map(Self::Andalso),
                Keyword::Orelse => ts.parse().map(Self::Orelse),
                _ => Err(parse::Error::unexpected_token(ts, token.into())),
            },
            Some(token) => Err(parse::Error::unexpected_token(ts, token)),
            None => Err(parse::Error::UnexpectedEof {
                position: ts.current_position(),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call_works() {
        let texts = [
            "foo()",
            "Foo(1, 2, 3)",
            indoc::indoc! {"
            (foo(Bar))(a,
                       b,
                       c())"},
            "foo:bar(baz)",
            indoc::indoc! {"
            foo(A *
                10 * B / 1_0.0)"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn unary_op_call_works() {
        let texts = ["-1", "bnot Foo(1, +2, 3)"];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn binary_op_call_works() {
        let texts = [
            "1 + 2",
            "1 - 2 * 3",
            indoc::indoc! {"
                {A, B, C} =
                    {foo, bar, baz} =
                        qux() /
                        quux() div 2"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
