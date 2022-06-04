use crate::format::{Format, Formatter};
use crate::items::components::ListLike;
use crate::items::expressions::components::ComprehensionExpr;
#[cfg(doc)]
use crate::items::expressions::components::Qualifier;
use crate::items::symbols::{CloseSquareSymbol, CommaSymbol, OpenSquareSymbol, VerticalBarSymbol};
use crate::items::Expr;
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

impl ListConstructExpr {
    pub(crate) fn items(&self) -> &[Expr] {
        self.0.items()
    }
}

#[derive(Debug, Clone, Span, Parse)]
enum ListItemDelimiter {
    Comma(CommaSymbol),
    VerticalBar(VerticalBarSymbol),
}

impl Format for ListItemDelimiter {
    fn format(&self, fmt: &mut Formatter) {
        match self {
            Self::Comma(x) => {
                x.format(fmt);
                fmt.add_space();
            }
            Self::VerticalBar(x) => {
                fmt.add_space();
                x.format(fmt);
                fmt.add_space();
            }
        }
    }
}

/// `[` [Expr] `||` ([Qualifier] `,`?)+  `]`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListComprehensionExpr(ComprehensionExpr<OpenSquareSymbol, CloseSquareSymbol>);

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
             7]"},
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
            indoc::indoc! {"
            [1,
             [2, 3, 4, 5, 6],
             7]"},
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
            [ X || X <- [1, 2] ]"},
            indoc::indoc! {"
            %---10---|%---20---|
            [ X
              || X <- [1, 2,
                       3] ]"},
            indoc::indoc! {"
            %---10---|%---20---|
            [ [X, Y]
              || X <- [1, 2, 3,
                       4, 5],
                 Y <= Z,
                 false ]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
