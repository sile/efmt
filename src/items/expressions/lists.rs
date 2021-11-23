use crate::format::{Format, Formatter};
use crate::items::expressions::components::ComprehensionExpr;
use crate::items::expressions::Expr;
use crate::items::generics::ListLike;
use crate::items::symbols::{CloseSquareSymbol, CommaSymbol, OpenSquareSymbol, VerticalBarSymbol};
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

/// `[` [Expr] `||` (`$QUALIFIER` `,`?)+  `]`
///
/// - $QUALIFIER: (`$GENERATOR` | `$FILTER` `,`?)+
/// - $GENERATOR: `Expr` (`<-` | `<=`) `Expr`
/// - $FILTER: `Expr`
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
