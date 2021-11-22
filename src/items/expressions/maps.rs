use crate::format::{self, Format};
use crate::format2::Format2;
use crate::items::expressions::Expr;
use crate::items::generics::{BinaryOpLike, BinaryOpStyle, Either, TupleLike};
use crate::items::symbols::{DoubleRightArrowSymbol, MapMatchSymbol, SharpSymbol};
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct MapConstructExpr {
    sharp: SharpSymbol,
    items: TupleLike<MapItem>,
}

/// `$VALUE` `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $VALUE: `Expr`
/// - $ENTRY: `Expr` (`:=` | `=>`) `Expr`
#[derive(Debug, Clone, Span, Parse, Format2)]
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

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct MapItem(BinaryOpLike<Expr, MapDelimiter, Expr>);

// TODO
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub(crate) struct MapDelimiter(Either<DoubleRightArrowSymbol, MapMatchSymbol>);

impl BinaryOpStyle for MapDelimiter {
    fn indent_offset(&self) -> usize {
        4
    }

    fn allow_newline(&self) -> bool {
        true
    }

    fn should_pack(&self) -> bool {
        false
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
            crate::assert_format2!(text, Expr);
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
            crate::assert_format2!(text, Expr);
        }
    }
}
