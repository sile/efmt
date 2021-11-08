use crate::format::Format;
use crate::items::expressions::{Expr, VariableLikeExpr};
use crate::items::generics::{Either, Items};
use crate::items::styles::{ColumnIndent, Space};
use crate::items::symbols::{
    CloseBraceSymbol, DoubleRightArrowSymbol, MapMatchSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum MapExpr {
    Construct(Box<MapConstructExpr>),
    Update(Box<MapUpdateExpr>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr {
    sharp: SharpSymbol,
    open: OpenBraceSymbol,
    items: ColumnIndent<Items<MapItem>>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapUpdateExpr {
    value: VariableLikeExpr,
    sharp: SharpSymbol,
    open: OpenBraceSymbol,
    items: ColumnIndent<Items<MapItem>>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapItem {
    key: Expr,
    delimiter: Space<Either<DoubleRightArrowSymbol, MapMatchSymbol>>,
    value: Expr,
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
    fn map_construct_works() {
        let texts = [
            "#{}",
            indoc::indoc! {"
                #{1 => 2,
                  foo => {bar, baz}}"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Map(x)) = &x {
                assert!(matches!(**x, MapExpr::Construct(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn map_update_works() {
        let texts = [
            "M#{}",
            indoc::indoc! {"
                (foo())#{1 => 2,
                         foo := {bar,
                                 baz}}"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Map(x)) = &x {
                assert!(matches!(**x, MapExpr::Update(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }
}
