use crate::format::Item;
use crate::items::expressions::Expr;
use crate::items::generics::{Items, NonEmptyItems};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{Indent, Space};
use crate::items::symbols::{
    CloseSquareSymbol, DoubleVerticalBarSymbol, OpenSquareSymbol, VerticalBarSymbol,
};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum ListExpr {
    Proper(ProperListExpr),
    Improper(ImproperListExpr),
    Comprehension(ListComprehensionExpr),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct ProperListExpr {
    open: OpenSquareSymbol,
    items: Indent<Items<Expr>, 4>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct ImproperListExpr {
    open: OpenSquareSymbol,
    items: Indent<Space<NonEmptyItems<Expr>>, 4>,
    bar: Indent<Space<VerticalBarSymbol>, 4>,
    last_item: Indent<Expr, 4>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    item: Indent<Space<Expr>, 4>,
    bar: Space<DoubleVerticalBarSymbol>,
    qualifiers: Indent<NonEmptyItems<Qualifier>, 4>,
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
