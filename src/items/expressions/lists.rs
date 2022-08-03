use crate::format::{Format, Formatter};
use crate::items::components::{ListLike, MaybePackedItems};
use crate::items::expressions::components::ComprehensionExpr;
#[cfg(doc)]
use crate::items::expressions::components::Qualifier;
use crate::items::symbols::{CloseSquareSymbol, OpenSquareSymbol, VerticalBarSymbol};
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

/// [ListConstructExpr] | [ListComprehensionExpr] | [ImproperListConstructExpr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum ListExpr {
    Construct(ListConstructExpr),
    Comprehension(ListComprehensionExpr),
    ConstructImproper(ImproperListConstructExpr),
}

/// `[` ([Expr] `,`?)* `]`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListConstructExpr(ListLike<Expr>);

impl ListConstructExpr {
    pub(crate) fn items(&self) -> &[Expr] {
        self.0.items()
    }
}

/// `[` ([Expr] (`,` | `|`)?)* `]`
#[derive(Debug, Clone, Span, Parse)]
pub struct ImproperListConstructExpr {
    open: OpenSquareSymbol,
    items: MaybePackedItems<Expr>,
    bar: VerticalBarSymbol,
    last: Expr,
    close: CloseSquareSymbol,
}

impl Format for ImproperListConstructExpr {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        self.items.format(fmt);
        fmt.write_space();
        self.bar.format(fmt);
        fmt.write_space();
        self.last.format(fmt);
        fmt.set_next_comment_indent(fmt.indent() + 1);
        self.close.format(fmt);
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
            [1, 2, 3, 4, 5, 6,
             7]"},
            indoc::indoc! {"
            [1, 2, 3, 4, 5, 6,
             7, 8, 9]"},
            indoc::indoc! {"
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
            [1,
             [[2] | 3] | [4, 5]]"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn list_comprehension_works() {
        let texts = [
            indoc::indoc! {"
            [ X || X <- [1, 2] ]"},
            indoc::indoc! {"
            [ X
              || X <- [1, 2,
                       3] ]"},
            indoc::indoc! {"
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
