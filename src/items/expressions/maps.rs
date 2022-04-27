use crate::format::Format;
use crate::items::components::MapLike;
use crate::items::symbols::SharpSymbol;
use crate::items::Expr;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr(MapLike<SharpSymbol, Expr>);

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
            %---10---|%---20---|
            #{
              1 => 2,
              333 => {444, 55},
              foo => {666, 777,
                      888}
             }"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn map_update_works() {
        let texts = [
            "M#{}",
            "1 #{}",
            "M#{}#{}",
            indoc::indoc! {"
            %---10---|%---20---|
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
