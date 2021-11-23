use crate::format::Format;
use crate::items::generics::MapLike;
use crate::items::Expr;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr(MapLike<Expr>);

/// `$VALUE` `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $VALUE: `Expr`
/// - $ENTRY: `Expr` (`:=` | `=>`) `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapUpdateExpr {
    value: Expr,
    map: MapLike<Expr>,
}

impl ResumeParse<Expr> for MapUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        Ok(Self {
            value,
            map: ts.parse()?,
        })
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
            #{1 => 2,
              333 => {444, 55},
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
            "1 #{}",
            "M#{}#{}",
            indoc::indoc! {"
            %---10---|%---20---|
            (foo())#{1 => 2,
                     foo :=
                         {bar,
                          baz}}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
