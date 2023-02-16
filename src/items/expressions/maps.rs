use crate::format::{Format, Formatter};
use crate::items::components::MapLike;
use crate::items::symbols::{
    CloseBraceSymbol, DoubleRightArrowSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::items::Expr;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

use super::components::ComprehensionExpr;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum MapExpr {
    Construct(MapConstructExpr),
    Comprehension(MapComprehensionExpr),
}

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr(MapLike<SharpSymbol, Expr>);

/// `#` `{` `$ENTRY` || ([Qualifier] `,`?)+ `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapComprehensionExpr(
    ComprehensionExpr<(SharpSymbol, OpenBraceSymbol), CloseBraceSymbol, MapComprehensionValue>,
);

#[derive(Debug, Clone, Span, Parse)]
struct MapComprehensionValue {
    key: Expr,
    delimiter: DoubleRightArrowSymbol,
    value: Expr,
}

impl Format for MapComprehensionValue {
    fn format(&self, fmt: &mut Formatter) {
        self.key.format(fmt);
        fmt.write_space();
        self.delimiter.format(fmt);
        fmt.write_space();
        self.value.format(fmt);
    }
}

/// `$VALUE` `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $VALUE: `Expr`
/// - $ENTRY: `Expr` (`:=` | `=>`) `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapUpdateExpr(MapLike<(Expr, SharpSymbol), Expr>);

impl ResumeParse<Expr> for MapUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        Ok(Self(MapLike::new((value, ts.parse()?), ts.parse()?)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn map_construct_works() {
        let texts = [
            "#{}",
            indoc::indoc! {"
            #{
              1 => 2,
              333 => {444, 55},
              foo => {666, 777,
                      888}
             }"},
            indoc::indoc! {"
            #{
              1 => 2,
              333 => {444, 55}
             }"},
            indoc::indoc! {"
            #{1 => 2, 333 => {444, 55}}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn map_comprehension_works() {
        let texts = [
            "#{ Key => Value || Key := Value <- MapOrIterator }",
            "#{ K => V || <<K, V>> <= Binary }",
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn map_update_works() {
        let texts = [
            "M#{}",
            "M#{}#{}",
            indoc::indoc! {"
            (foo())#{
              1 => 2,
              foo := {Bar, baz}
             }"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
