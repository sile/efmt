use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::Elements;
use crate::items::symbols::{CloseBraceSymbol, OpenBraceSymbol};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleExpr {
    open: OpenBraceSymbol,
    items: Elements<Expr>,
    close: CloseBraceSymbol,
}

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
