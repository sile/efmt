use crate::format::{Format, Formatter};
use crate::items::components::TupleLike;
use crate::items::tokens::AtomToken;
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

/// `{` ([Expr] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleExpr(TupleLike<Expr>);

impl TupleExpr {
    pub fn children(&self) -> impl Iterator<Item = &Expr> {
        self.items().1.iter()
    }

    pub(crate) fn items(&self) -> (Option<&AtomToken>, &[Expr]) {
        self.0.items()
    }

    pub(crate) fn try_format_app_file(&self, fmt: &mut Formatter) -> bool {
        self.0.try_format_app_file(fmt)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tuple_works() {
        let texts = [
            "{}",
            "{1}",
            "{foo, bar, baz}",
            indoc::indoc! {"
            {1, 2, 3, 4, 5, 6,
             7, 8, 9}"},
            indoc::indoc! {"
            {1,
             2,
             {3, 4, 5},
             6,
             {7, 8, 9}}"},
            indoc::indoc! {"
            {error,
             {Foo, Bar,
              Baz},
             qux}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn tagged_tuple_works() {
        let texts = [indoc::indoc! {"
            {error, {Foo, Bar,
                     Baz},
                    qux}"}];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
