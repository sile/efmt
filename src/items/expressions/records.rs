use crate::format::Format;
use crate::items::expressions::{Expr, VariableLikeExpr};
use crate::items::generics::{Items, MaybeRepeat};
use crate::items::styles::{ColumnIndent, Space};
use crate::items::symbols::{
    CloseBraceSymbol, DotSymbol, MatchSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::items::tokens::AtomToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordExpr {
    Construct(RecordConstructExpr),
    Index(RecordIndexExpr),
    Access(RecordAccessExpr),
    Update(RecordUpdateExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordConstructExpr {
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: ColumnIndent<Items<RecordField>>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordAccessExpr {
    value: VariableLikeExpr,
    index: MaybeRepeat<RecordIndexExpr>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordIndexExpr {
    sharp: SharpSymbol,
    name: AtomToken,
    dot: DotSymbol,
    field: AtomToken,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordUpdateExpr {
    value: VariableLikeExpr,
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: ColumnIndent<Items<RecordField>>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordField {
    name: AtomToken,
    delimiter: Space<MatchSymbol>,
    value: Expr,
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
    fn record_construct_works() {
        let texts = [
            "#foo{}",
            indoc::indoc! {"
                #foo{bar = 2,
                     baz = {bar, baz}}"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Construct(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn record_index_works() {
        let texts = ["#foo.bar"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Index(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn record_access_works() {
        let texts = [
            "X#foo.bar",
            "(foo())#foo.bar",
            "N2#nrec2.nrec1#nrec1.nrec0#nrec0.name",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Access(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn record_update_works() {
        let texts = [
            "M#foo{}",
            indoc::indoc! {"
                (foo())#foo{bar = 2,
                            baz = {bar,
                                   baz}}"},
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Record(x)) = &x {
                assert!(matches!(**x, RecordExpr::Update(_)));
            } else {
                panic!("{:?}", x);
            }
            assert_eq!(format(text), text);
        }
    }
}
