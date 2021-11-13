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

    #[test]
    fn map_construct_works() {
        let texts = [
            "#{}",
            indoc::indoc! {"
                #{1 => 2,
                  foo => {bar, baz}}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
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
            crate::assert_format!(text, Expr);
        }
    }
}
