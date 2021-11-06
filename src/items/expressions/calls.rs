use crate::format::{Item, Tree};
use crate::items::expressions::{AtomLikeExpr, Expr, NonLeftRecursiveExpr};
use crate::items::generics::{Items, Maybe, Parenthesized};
use crate::items::keywords;
use crate::items::symbols::{self, ColonSymbol};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub struct FunctionCallExpr {
    module: Maybe<(AtomLikeExpr, ColonSymbol)>,
    function: AtomLikeExpr,
    args: Parenthesized<Items<Expr>>,
}

impl Item for FunctionCallExpr {
    fn tree(&self) -> Tree {
        Tree::SideEffect(Box::new(Tree::Compound(vec![
            self.module.tree(),
            self.function.tree(),
            self.args.tree(),
        ])))
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct UnaryOpCallExpr {
    op: UnaryOp,
    expr: Expr,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum UnaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Not(keywords::NotKeyword),
    Bnot(keywords::BnotKeyword),
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpCallExpr {
    left: NonLeftRecursiveExpr,
    op: BinaryOp,
    right: Expr,
}

impl Item for BinaryOpCallExpr {
    fn tree(&self) -> Tree {
        Tree::BinaryOp {
            left: Box::new(self.left.tree()),
            delimiter: self.op.to_item_span(),
            right: Box::new(self.right.tree()),
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
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
