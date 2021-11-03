use crate::format::Format;
use crate::items::expressions::{Expr, IntegerLikeExpr, LiteralExpr};
use crate::items::generics::{Either, Items, Maybe, NonEmptyItems, Parenthesized};
use crate::items::qualifiers::Qualifier;
use crate::items::symbols::{
    ColonSymbol, CommaSymbol, DoubleLeftAngleSymbol, DoubleRightAngleSymbol,
    DoubleVerticalBarSymbol, HyphenSymbol, SlashSymbol,
};
use crate::items::tokens::{AtomToken, IntegerToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BitstringExpr {
    Construct(BitstringConstructExpr),
    Comprehension(BitstringComprehensionExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringConstructExpr {
    open: DoubleLeftAngleSymbol,
    segments: Items<BitstringSegment, CommaSymbol>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringComprehensionExpr {
    open: DoubleLeftAngleSymbol,
    item: Expr,
    bar: DoubleVerticalBarSymbol,
    qualifiers: NonEmptyItems<Qualifier, CommaSymbol>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegment {
    value: Either<LiteralExpr, Parenthesized<Expr>>,
    size: Maybe<BitstringSegmentSize>,
    ty: Maybe<BitstringSegmentType>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentSize {
    colon: ColonSymbol,
    size: IntegerLikeExpr,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentType {
    slash: SlashSymbol,
    specifiers: NonEmptyItems<BitstringSegmentTypeSpecifier, HyphenSymbol>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentTypeSpecifier {
    name: AtomToken,
    value: Maybe<(ColonSymbol, IntegerToken)>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn bitstring_construct_works() {
        let texts = [
            "<<>>",
            "<<1,2:16,3>>",
            "<<1,(foo()):4/little-signed-integer-unit:8,C/binary>>",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Bitstring(x)) = &x {
                assert!(matches!(**x, BitstringExpr::Construct(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn bitstring_comprehension_works() {
        let texts = [
            "<< <<X>> || X <- [1,2,3]>>",
            "<< foo(X,Y) || X <- [1,2,3], Y <= Z, false>>",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Bitstring(x)) = &x {
                assert!(matches!(**x, BitstringExpr::Comprehension(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }
}
