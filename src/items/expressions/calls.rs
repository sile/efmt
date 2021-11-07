use crate::format::{self, Format};
use crate::items::expressions::{AtomLikeExpr, Expr, NonLeftRecursiveExpr};
use crate::items::generics::{Args, Maybe};
use crate::items::keywords;
use crate::items::styles::{Child, Space};
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
    Not(keywords::NotKeyword),
    Bnot(keywords::BnotKeyword),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BinaryOpCallExpr {
    left: Child<NonLeftRecursiveExpr>,
    op: Space<BinaryOp>,
    right: Child<BinaryOpRightExpr>,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpRightExpr {
    expr: Expr,
}

impl Format for BinaryOpRightExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if !fmt.multiline_mode() {
            fmt.format_child_item_with_options(
                &self.expr,
                format::ChildOptions::new().forbid_multiline(),
            )?;
        } else {
            fmt.format_child_item_with_options(
                &self.expr,
                format::ChildOptions::new().newline().base(1),
            )?;
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
    Andalso(keywords::AndalsoKeyword),
    Orelse(keywords::OrelseKeyword),
    Send(symbols::NotSymbol),
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn function_call_works() {
        let texts = ["foo()", "Foo(1,2,3)", "(foo(Bar))(a,b,c())", "foo:bar(baz)"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Expr::NonLeftRecursive(NonLeftRecursiveExpr::FunctionCall(_))
            ));
        }
    }

    #[test]
    fn unary_op_call_works() {
        let texts = ["-1", "bnot Foo(1,+2,3)"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Expr::NonLeftRecursive(NonLeftRecursiveExpr::UnaryOpCall(_))
            ));
        }
    }

    #[test]
    fn binary_op_call_works() {
        let texts = ["1+2", "1-2*3", "{A, B, C} = {foo, bar, baz} = qux()"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(x, Expr::BinaryOpCall(_)));
        }
    }
}
