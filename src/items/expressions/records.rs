use crate::format::Format;
use crate::items::expressions::{Expr, VariableLikeExpr};
use crate::items::generics::{Items, Maybe};
use crate::items::symbols::{
    CloseBraceSymbol, CommaSymbol, DotSymbol, MatchSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::items::tokens::AtomToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordExpr {
    Construct(RecordConstructExpr),
    Access(RecordAccessExpr),
    Update(RecordUpdateExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordConstructExpr {
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: Items<RecordField, CommaSymbol>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordAccessExpr {
    value: Maybe<VariableLikeExpr>,
    sharp: SharpSymbol,
    name: AtomToken,
    dot: DotSymbol,
    field: AtomToken,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordUpdateExpr {
    // TODO: allow `N2#nrec2.nrec1#nrec1.nrec0#nrec0.name`
    value: VariableLikeExpr,
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: Items<RecordField, CommaSymbol>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordField {
    name: AtomToken,
    delimiter: MatchSymbol,
    value: Expr,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn record_construct_works() {
        let texts = ["#foo{}", "#foo{bar=2, baz={bar,baz}}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Construct(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn record_access_works() {
        let texts = ["#foo.bar", "X#foo.bar", "(foo())#foo.bar"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Access(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn record_update_works() {
        let texts = ["M#foo{}", "(foo())#foo{bar=2, baz={bar,baz}}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Update(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }
}
