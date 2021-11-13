use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::{Elements, NonEmptyItems};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{ColumnIndent, Space};
use crate::items::symbols::{
    CloseSquareSymbol, DoubleVerticalBarSymbol, OpenSquareSymbol, VerticalBarSymbol,
};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum ListExpr {
    Proper(ProperListExpr),
    Improper(ImproperListExpr),
    Comprehension(ListComprehensionExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ProperListExpr {
    open: OpenSquareSymbol,
    items: Elements<Expr>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ImproperListExpr {
    open: OpenSquareSymbol,
    items: Elements<Expr>,
    bar: Space<VerticalBarSymbol>,
    last_item: Expr,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    item: Expr,
    bar: Space<DoubleVerticalBarSymbol>,
    qualifiers: ColumnIndent<NonEmptyItems<Qualifier>>,
    close: CloseSquareSymbol,
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
                [1, 2, 3, 4, 5, 6,
                 7, 8, 9]"},
            indoc::indoc! {"
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
                [1, [[2] | 3] | [4,
                                 5]]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn list_comprehension_works() {
        let texts = [
            "[X || X <- [1, 2, 3]]",
            indoc::indoc! {"
                [[X, Y] || X <- [1,
                                 2,
                                 3],
                           Y <= Z,
                           false]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
