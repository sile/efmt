use crate::format::Format;
use crate::items::expressions::{BaseExpr, Expr};
use crate::items::generics::{Items, MaybeRepeat};
use crate::items::styles::{ColumnIndent, Space};
use crate::items::symbols::{
    CloseBraceSymbol, DotSymbol, MatchSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::items::tokens::AtomToken;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordExpr {
    Construct(Box<RecordConstructExpr>),
    Index(Box<RecordIndexExpr>),
    Access(Box<RecordAccessExpr>),
    Update(Box<RecordUpdateExpr>),
}

impl ResumeParse<BaseExpr> for RecordExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: BaseExpr) -> parse::Result<Self> {
        if ts.peek::<(SharpSymbol, (AtomToken, DotSymbol))>().is_some() {
            ts.resume_parse(value).map(Self::Access)
        } else {
            ts.resume_parse(value).map(Self::Update)
        }
    }
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
    value: BaseExpr,
    index: MaybeRepeat<RecordIndexExpr>,
}

impl ResumeParse<BaseExpr> for RecordAccessExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: BaseExpr) -> parse::Result<Self> {
        Ok(Self {
            value,
            index: ts.parse()?,
        })
    }
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
    value: BaseExpr,
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: ColumnIndent<Items<RecordField>>,
    close: CloseBraceSymbol,
}

impl ResumeParse<BaseExpr> for RecordUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: BaseExpr) -> parse::Result<Self> {
        Ok(Self {
            value,
            sharp: ts.parse()?,
            name: ts.parse()?,
            open: ts.parse()?,
            fields: ts.parse()?,
            close: ts.parse()?,
        })
    }
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

    #[test]
    fn record_construct_works() {
        let texts = [
            "#foo{}",
            indoc::indoc! {"
                #foo{bar = 2,
                     baz = {bar,
                            baz}}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn record_index_works() {
        let texts = ["#foo.bar"];
        for text in texts {
            crate::assert_format!(text, Expr);
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
            crate::assert_format!(text, Expr);
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
            crate::assert_format!(text, Expr);
        }
    }
}
