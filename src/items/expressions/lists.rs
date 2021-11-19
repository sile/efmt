use crate::format::{self, Format};
use crate::items::expressions::{Expr, Qualifier};
use crate::items::generics::{BinaryOpLike, Indent, ListLike, NonEmptyItems};
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
pub struct ListConstructExpr(ListLike<Expr, ListItemDelimiter>);

#[derive(Debug, Clone, Span, Parse)]
enum ListItemDelimiter {
    Comma(CommaSymbol),
    VerticalBar(VerticalBarSymbol),
}

impl Format for ListItemDelimiter {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        match self {
            Self::Comma(x) => x.format(fmt),
            Self::VerticalBar(x) => {
                fmt.write_space()?;
                x.format(fmt)
            }
        }
    }
}

/// `[` [Expr] `||` ([Qualifier] `,`?)+ `]`
#[derive(Debug, Clone, Span, Parse)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    body: ListComprehensionBody,
    close: CloseSquareSymbol,
}

impl Format for ListComprehensionExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .reset_trailing_columns(1) // "]"
            .enter(|fmt| self.body.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

type ListComprehensionBody =
    BinaryOpLike<Expr, Indent<DoubleVerticalBarSymbol, 3>, NonEmptyItems<Qualifier>>;

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
                X <-
                    [1, 2, 3, 4,
                     5],
                Y <= Z,
                false]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
