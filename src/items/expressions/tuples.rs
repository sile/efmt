use crate::format::Format;
use crate::items::components::TupleLike;
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

/// `{` ([Expr] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleExpr(TupleLike<Expr>);

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
            %---10---|%---20---|
            {1, 2, 3, 4, 5, 6,
             7, 8, 9}"},
            indoc::indoc! {"
            %---10---|%---20---|
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
}
