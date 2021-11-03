use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::Items;
use crate::items::symbols::{CloseBraceSymbol, OpenBraceSymbol};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleExpr {
    open: OpenBraceSymbol,
    items: Items<Expr>,
    close: CloseBraceSymbol,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn tuple_works() {
        let texts = ["{}", "{1}", "{foo,bar,baz}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Expr::NonLeftRecursive(NonLeftRecursiveExpr::Tuple(_))
            ));
        }
    }
}
