use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::{Either, ListLike, NonEmptyItems2, UnbalancedBinaryOpLike};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{Space, TrailingColumns};
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

type ListItemDelimiter = Either<CommaSymbol, Space<VerticalBarSymbol>>;

/// `[` [Expr] `||` ([Qualifier] `,`?)+ `]`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    body: TrailingColumns<ListComprehensionBody, 1>, // "]"
    close: CloseSquareSymbol,
}

type ListComprehensionBody =
    UnbalancedBinaryOpLike<Expr, Space<DoubleVerticalBarSymbol>, NonEmptyItems2<Qualifier>>;

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
