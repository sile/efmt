use crate::format::Format;
use crate::items::Expr;
use crate::items::components::{Element, RecordLike};
use crate::items::expressions::Either;
use crate::items::symbols::{DotSymbol, MatchSymbol, SharpSymbol};
use crate::items::tokens::AtomToken;
use crate::items::variables::UnderscoreVariable;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordConstructOrIndexExpr {
    Construct(Box<RecordConstructExpr>),
    Index(Box<RecordIndexExpr>),
}

impl RecordConstructOrIndexExpr {
    pub fn record_name(&self) -> &AtomToken {
        match self {
            Self::Construct(x) => &x.record.prefix().1,
            Self::Index(x) => &x.name,
        }
    }

    pub fn field_names(&self) -> impl Iterator<Item = &AtomToken> {
        match self {
            Self::Construct(x) => Either::A(x.record.fields().filter_map(|x| x.name())),
            Self::Index(x) => Either::B(std::iter::once(&x.field)),
        }
    }

    pub fn children(&self) -> impl Iterator<Item = &Expr> {
        match self {
            Self::Construct(x) => Either::A(x.record.fields().map(|x| &x.value)),
            Self::Index(_) => Either::B(std::iter::empty()),
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordAccessOrUpdateExpr {
    Access(Box<RecordAccessExpr>),
    Update(Box<RecordUpdateExpr>),
}

impl RecordAccessOrUpdateExpr {
    pub fn record_name(&self) -> &AtomToken {
        match self {
            Self::Access(x) => &x.index.name,
            Self::Update(x) => &x.record.prefix().1.1,
        }
    }

    pub fn field_names(&self) -> impl Iterator<Item = &AtomToken> {
        match self {
            Self::Access(x) => Either::A(std::iter::once(&x.index.field)),
            Self::Update(x) => Either::B(x.record.fields().filter_map(|x| x.name())),
        }
    }

    pub fn children(&self) -> impl Iterator<Item = &Expr> {
        match self {
            Self::Access(x) => Either::A(std::iter::once(&x.value)),
            Self::Update(x) => Either::B(
                std::iter::once(&x.record.prefix().0).chain(x.record.fields().map(|x| &x.value)),
            ),
        }
    }
}

impl ResumeParse<Expr> for RecordAccessOrUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        if ts.peek::<(SharpSymbol, (AtomToken, DotSymbol))>().is_some() {
            ts.resume_parse(value).map(Self::Access)
        } else {
            ts.resume_parse(value).map(Self::Update)
        }
    }
}

/// `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $NAME: [AtomToken]
/// - $FIELD: ([AtomToken] | `_`) `=` [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordConstructExpr {
    record: RecordLike<(SharpSymbol, AtomToken), RecordField>,
}

/// `$VALUE` `#` `$NAME` `.` `$FIELD`
///
/// - $VALUE: [Expr]
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordAccessExpr {
    value: Expr,
    index: RecordIndexExpr,
}

impl ResumeParse<Expr> for RecordAccessExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        Ok(Self {
            value,
            index: ts.parse()?,
        })
    }
}

/// `#` `$NAME` `.` `$FIELD`
///
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordIndexExpr {
    sharp: SharpSymbol,
    name: AtomToken,
    dot: DotSymbol,
    field: AtomToken,
}

/// `$VALUE` `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $VALUE: [Expr]
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordUpdateExpr {
    record: RecordLike<(Expr, (SharpSymbol, AtomToken)), RecordField>,
}

impl ResumeParse<Expr> for RecordUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        let sharp = ts.parse()?;
        let name = ts.parse()?;
        let fields = ts.parse()?;
        Ok(Self {
            record: RecordLike::new((value, (sharp, name)), fields),
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Element)]
struct RecordField {
    name: Either<AtomToken, UnderscoreVariable>,
    delimiter: MatchSymbol,
    value: Expr,
}

impl RecordField {
    pub fn name(&self) -> Option<&AtomToken> {
        if let Either::A(x) = &self.name {
            Some(x)
        } else {
            None
        }
    }
}

impl Format for RecordField {
    fn format(&self, fmt: &mut crate::format::Formatter) {
        fmt.with_scoped_indent(|fmt| {
            self.name.format(fmt);

            let newline = fmt.has_newline_until(&self.value);
            fmt.write_space();
            self.delimiter.format(fmt);
            if newline {
                fmt.set_indent(fmt.indent() + 4);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }
            self.value.format(fmt);
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn record_construct_works() {
        let texts = [
            "#foo{}",
            indoc::indoc! {"
            #foo{
              module = Mod,
              _ = '_'
             }"},
            indoc::indoc! {"
            #a{b = 1, c = 2}"},
            indoc::indoc! {"
            #foo{
              bar = 2,
              baz = {Bar, baz,
                     qux}
             }"},
            indoc::indoc! {"
            #foo{
              bar = 2,
              baz =
                  {Bar, baz, qux}
             }"},
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
            M#baz{
              qux = 1
             }#foo.bar"},
            indoc::indoc! {"
            M#foo.bar#baz{
              qux = 1
             }"},
            indoc::indoc! {"
            (foo())#foo{
              bar = 2,
              baz = {Bar, baz}
             }"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
