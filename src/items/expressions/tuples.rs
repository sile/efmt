use crate::format::Format;
use crate::format2::Format2;
use crate::items::expressions::Expr;
use crate::items::generics::TupleLike;
use crate::parse::Parse;
use crate::span::Span;

/// `{` ([Expr] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
            crate::assert_format2!(text, Expr);
        }
    }
}
