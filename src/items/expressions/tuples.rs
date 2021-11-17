use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::TupleLike;
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
        let texts = ["{}", "{1}", "{foo, bar, baz}"];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
