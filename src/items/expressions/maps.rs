use crate::format::Format;
use crate::items::expressions::{BaseExpr, Expr};
use crate::items::generics::{Either, Items};
use crate::items::styles::{ColumnIndent, Space};
use crate::items::symbols::{
    CloseBraceSymbol, DoubleRightArrowSymbol, MapMatchSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum MapExpr {
    Construct(Box<MapConstructExpr>),
    Update(Box<MapUpdateExpr>),
}

impl ResumeParse<BaseExpr> for MapExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: BaseExpr) -> parse::Result<Self> {
        ts.resume_parse(value).map(MapExpr::Update)
    }
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
    value: BaseExpr,
    sharp: SharpSymbol,
    open: OpenBraceSymbol,
    items: ColumnIndent<Items<MapItem>>,
    close: CloseBraceSymbol,
}

impl ResumeParse<BaseExpr> for MapUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: BaseExpr) -> parse::Result<Self> {
        Ok(Self {
            value,
            sharp: ts.parse()?,
            open: ts.parse()?,
            items: ts.parse()?,
            close: ts.parse()?,
        })
    }
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
