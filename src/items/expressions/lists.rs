use crate::format::{Item, Tree};
use crate::items::expressions::Expr;
use crate::items::generics::{Items, NonEmptyItems};
use crate::items::qualifiers::Qualifier;
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
    items: Items<Expr>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ImproperListExpr {
    open: OpenSquareSymbol,
    items: NonEmptyItems<Expr>,
    bar: VerticalBarSymbol,
    last_item: Expr,
    close: CloseSquareSymbol,
}

impl Item for ImproperListExpr {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            self.open.tree(),
            Tree::BinaryOp {
                left: Box::new(self.items.tree()),
                delimiter: self.bar.to_item_span(),
                right: Box::new(self.last_item.tree()),
            },
            self.close.tree(),
        ])
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct ListComprehensionExpr {
    open: OpenSquareSymbol,
    item: Expr,
    bar: DoubleVerticalBarSymbol,
    qualifiers: NonEmptyItems<Qualifier>,
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
