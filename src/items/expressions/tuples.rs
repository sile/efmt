use crate::format::Format;
use crate::items::components::TupleLike;
use crate::items::tokens::AtomToken;
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

/// `{` ([Expr] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleExpr(TupleLike<Expr>);

impl TupleExpr {
    pub(crate) fn items(&self) -> (Option<&AtomToken>, &[Expr]) {
        self.0.items()
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
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn tagged_tuple_works() {
        let texts = [indoc::indoc! {"
            %---10---|%---20---|
            {error, {Foo, Bar,
                     Baz},
                    qux}"}];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
