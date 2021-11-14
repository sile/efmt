use crate::format::{self, Format};
use crate::items::expressions::{AtomLikeExpr, Expr, NonLeftRecursiveExpr};
use crate::items::generics::{Args, Maybe};
use crate::items::keywords;
use crate::items::styles::{Child, RightSpace, Space};
use crate::items::symbols::{self, ColonSymbol};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionCallExpr {
    module: Maybe<(AtomLikeExpr, ColonSymbol)>,
    function: AtomLikeExpr,
    args: Args<Expr>,
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

impl Format for BinaryOpCallExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
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

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BinaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Mul(symbols::MultiplySymbol),
    FloatDiv(symbols::SlashSymbol),
    IntDiv(keywords::DivKeyword),
    Rem(keywords::RemKeyword),
    Bor(keywords::BorKeyword),
    Bxor(keywords::BxorKeyword),
    Band(keywords::BandKeyword),
    Bsl(keywords::BslKeyword),
    Bsr(keywords::BsrKeyword),
    Or(keywords::OrKeyword),
    Xor(keywords::XorKeyword),
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
    And(keywords::AndKeyword),
    Andalso(keywords::AndalsoKeyword),
    Orelse(keywords::OrelseKeyword),
    Send(symbols::NotSymbol),
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
