use crate::format::{self, Format};
use crate::items::expressions::Expr;
use crate::items::generics::{Either, MatchLike, TupleLike};
use crate::items::styles::Space;
use crate::items::symbols::{DoubleRightArrowSymbol, MapMatchSymbol, SharpSymbol};
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr {
    sharp: SharpSymbol,
    items: TupleLike<MapItem>,
}

/// `$VALUE` `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $VALUE: `Expr`
/// - $ENTRY: `Expr` (`:=` | `=>`) `Expr`
#[derive(Debug, Clone, Span, Parse)]
pub struct MapUpdateExpr {
    value: Expr,
    sharp: SharpSymbol,
    items: TupleLike<MapItem>,
}

impl ResumeParse<Expr> for MapUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        Ok(Self {
            value,
            sharp: ts.parse()?,
            items: ts.parse()?,
        })
    }
}

impl Format for MapUpdateExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.value.format(fmt)?;
        if self.value.is_integer_token() {
            fmt.write_space()?;
        }
        self.sharp.format(fmt)?;
        self.items.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct MapItem(MatchLike<Expr, Space<Either<DoubleRightArrowSymbol, MapMatchSymbol>>, Expr>);

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
