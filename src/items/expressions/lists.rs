use crate::format::{self, Format};
use crate::items::expressions::Expr;
use crate::items::generics::{Either, MaybePackedItems, NonEmptyItems2};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{ColumnIndent, Space, TrailingColumns};
use crate::items::symbols::{
    CloseSquareSymbol, CommaSymbol, DoubleVerticalBarSymbol, OpenSquareSymbol, VerticalBarSymbol,
};
use crate::parse::Parse;
use crate::span::Span;

/// [ListConstructExpr] | [ListComprehensionExpr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum ListExpr {
    Construct(ListConstructExpr),
    Comprehension(ListComprehensionExpr),
}

/// `[` ([Expr] (`,` | `|`)?)* `]`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListConstructExpr {
    open: OpenSquareSymbol,
    items: TrailingColumns<MaybePackedItems<Expr, ListItemDelimiter>, 1>, // "]"
    close: CloseSquareSymbol,
}

type ListItemDelimiter = Either<CommaSymbol, Space<VerticalBarSymbol>>;

/// `[` [Expr] `||` ([Qualifier] `,`?)* `]`
#[derive(Debug, Clone, Span, Parse)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    item: ColumnIndent<TrailingColumns<Expr, 3>>, // " ||"
    bar: Space<DoubleVerticalBarSymbol>,
    qualifiers: TrailingColumns<NonEmptyItems2<Qualifier>, 1>, // "]"
    close: CloseSquareSymbol,
}

impl Format for ListComprehensionExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.open.format(fmt)?;
            self.item.format(fmt)?;
            self.bar.format(fmt)?;

            fmt.subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .enter(|fmt| self.qualifiers.format(fmt))
                .or_else(|_| {
                    fmt.subregion().indent_offset(4).enter(|fmt| {
                        fmt.write_newline()?;
                        self.qualifiers.format(fmt)
                    })
                })?;

            self.close.format(fmt)?;
            Ok(())
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn proper_list_works() {
        let texts = [
            "[]",
            "[1]",
            "[foo, bar, baz]",
            indoc::indoc! {"
            %---10---|%---20---|
            [1, 2, 3, 4, 5, 6,
             7, 8, 9]"},
            indoc::indoc! {"
            %---10---|%---20---|
            [1,
             [2, 3, 4, 5, 6],
             7,
             8,
             9]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn improper_list_works() {
        let texts = [
            "[1 | 2]",
            "[1, 2 | 3]",
            indoc::indoc! {"
            %---10---|%---20---|
            [1,
             [[2] | 3] |
             [4, 5]]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn list_comprehension_works() {
        let texts = [
            indoc::indoc! {"
            %---10---|%---20---|
            [X || X <- [1, 2]]"},
            indoc::indoc! {"
            %---10---|%---20---|
            [X ||
                X <- [1, 2, 3]]"},
            indoc::indoc! {"
            %---10---|%---20---|
            [[X, Y] ||
                X <- [1, 2, 3,
                      4, 5],
                Y <= Z,
                false]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
