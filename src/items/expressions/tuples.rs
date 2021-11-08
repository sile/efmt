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
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    fn format(text: &str) -> String {
        crate::FormatOptions::<crate::items::styles::Child<Expr>>::new()
            .max_columns(20)
            .format_text(text)
            .expect("parse or format failed")
    }

    #[test]
    fn tuple_works() {
        let texts = ["{}", "{1}", "{foo, bar, baz}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Expr::NonLeftRecursive(NonLeftRecursiveExpr::Tuple(_))
            ));
            assert_eq!(format(text), text);
        }
    }
}
