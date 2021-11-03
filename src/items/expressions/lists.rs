use crate::format::Format;
use crate::items::expressions::Expr;
use crate::items::generics::{Items, NonEmptyItems};
use crate::items::qualifiers::Qualifier;
use crate::items::symbols::{
    CloseSquareSymbol, CommaSymbol, DoubleVerticalBarSymbol, OpenSquareSymbol, VerticalBarSymbol,
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
    items: Items<Expr, CommaSymbol>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ImproperListExpr {
    open: OpenSquareSymbol,
    items: NonEmptyItems<Expr, CommaSymbol>,
    bar: VerticalBarSymbol,
    last_item: Expr,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    item: Expr,
    bar: DoubleVerticalBarSymbol,
    qualifiers: NonEmptyItems<Qualifier, CommaSymbol>,
    close: CloseSquareSymbol,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn proper_list_works() {
        let texts = ["[]", "[1]", "[foo,bar,baz]"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::List(x)) = &x {
                assert!(matches!(**x, ListExpr::Proper(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn improper_list_works() {
        let texts = ["[1|2]", "[1,2|3]", "[1,[[2]|3]|[4,5]]"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::List(x)) = &x {
                assert!(matches!(**x, ListExpr::Improper(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn list_comprehension_works() {
        let texts = [
            "[X || X <- [1,2,3]]",
            "[[X,Y] || X <- [1,2,3], Y <= Z, false]",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::List(x)) = &x {
                assert!(matches!(**x, ListExpr::Comprehension(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }
}
