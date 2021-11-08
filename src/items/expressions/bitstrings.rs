use crate::format::{self, Format};
use crate::items::expressions::{Expr, IntegerLikeExpr, LiteralExpr};
use crate::items::generics::{Either, Elements, Maybe, NonEmptyItems, Parenthesized};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{ColumnIndent, RightSpace, Space};
use crate::items::symbols::{
    ColonSymbol, DoubleLeftAngleSymbol, DoubleRightAngleSymbol, DoubleVerticalBarSymbol,
    HyphenSymbol, SlashSymbol,
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
    segments: Elements<BitstringSegment>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringComprehensionExpr {
    open: RightSpace<DoubleLeftAngleSymbol>,
    item: Expr,
    bar: Space<DoubleVerticalBarSymbol>,
    qualifiers: ColumnIndent<NonEmptyItems<Qualifier>>,
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

#[derive(Debug, Clone, Span, Parse)]
pub struct BitstringSegmentType {
    slash: SlashSymbol,
    specifiers: NonEmptyItems<BitstringSegmentTypeSpecifier, HyphenSymbol>,
}

impl Format for BitstringSegmentType {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.format_item(&self.slash)?;
        for (item, delimiter) in self
            .specifiers
            .items
            .iter()
            .zip(self.specifiers.delimiters.iter())
        {
            fmt.format_item(item)?;
            fmt.format_item(delimiter)?;
        }
        fmt.format_item(self.specifiers.items.last().expect("unreachable"))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentTypeSpecifier {
    name: AtomToken,
    value: Maybe<(ColonSymbol, IntegerToken)>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    fn format(text: &str) -> String {
        crate::FormatOptions::<crate::items::styles::Child<Expr>>::new()
            .max_columns(20)
            .format_text(text)
            .expect("parse or format failed")
    }

    #[test]
    fn bitstring_construct_works() {
        let texts = [
            "<<>>",
            "<<1, 2:16, 3>>",
            indoc::indoc! {"
                <<1,
                  (foo()):4/little-signed-integer-unit:8,
                  C/binary>>"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Bitstring(x)) = &x {
                assert!(matches!(**x, BitstringExpr::Construct(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn bitstring_comprehension_works() {
        let texts = [
            indoc::indoc! {"
                << <<X>> || X <- [1,
                                  2,
                                  3]>>"},
            indoc::indoc! {"
                << foo(X,
                       Y,
                       Z,
                       bar(),
                       baz()) || X <- [1,
                                       2,
                                       3],
                                 Y <= Z,
                                 false>>"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Bitstring(x)) = &x {
                assert!(matches!(**x, BitstringExpr::Comprehension(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }
}
