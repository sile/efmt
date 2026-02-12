use crate::format::{Format, Formatter};
use crate::items::Expr;
use crate::items::components::{Either, Element, Maybe, RecordLike};
use crate::items::symbols::{ColonSymbol, DotSymbol, MatchSymbol, SharpSymbol};
use crate::items::tokens::{AtomToken, KeywordToken, VariableToken};
use crate::items::variables::UnderscoreVariable;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span)]
struct RecordName {
    token: Either<AtomToken, Either<KeywordToken, VariableToken>>,
    atom: AtomToken,
}

impl RecordName {
    fn as_atom_token(&self) -> &AtomToken {
        &self.atom
    }
}

impl Parse for RecordName {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        if let Ok(token) = ts.parse::<AtomToken>() {
            return Ok(Self {
                atom: token.clone(),
                token: Either::A(token),
            });
        }
        if let Ok(token) = ts.parse::<KeywordToken>() {
            let atom = AtomToken::new(
                token.value().as_str(),
                token.start_position(),
                token.end_position(),
            );
            return Ok(Self {
                atom,
                token: Either::B(Either::A(token)),
            });
        }

        let token: VariableToken = ts.parse()?;
        if token.value() == "_" {
            return Err(parse::Error::unexpected_token(ts, token.into()));
        }
        let atom = AtomToken::new(token.value(), token.start_position(), token.end_position());
        Ok(Self {
            atom,
            token: Either::B(Either::B(token)),
        })
    }
}

impl Format for RecordName {
    fn format(&self, fmt: &mut Formatter) {
        self.token.format(fmt);
    }
}

#[derive(Debug, Clone, Span)]
struct QualifiedRecordName {
    module: Maybe<(AtomToken, ColonSymbol)>,
    name: RecordName,
}

impl QualifiedRecordName {
    fn as_atom_token(&self) -> &AtomToken {
        self.name.as_atom_token()
    }
}

impl Parse for QualifiedRecordName {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let module = if ts.peek::<(AtomToken, ColonSymbol)>().is_some() {
            Maybe::some(ts.parse()?)
        } else {
            Maybe::parse_none(ts)?
        };
        Ok(Self {
            module,
            name: ts.parse()?,
        })
    }
}

impl Format for QualifiedRecordName {
    fn format(&self, fmt: &mut Formatter) {
        self.module.format(fmt);
        self.name.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
#[expect(clippy::large_enum_variant)]
enum RecordNameRef {
    Named(QualifiedRecordName),
    Anonymous(UnderscoreVariable),
}

impl RecordNameRef {
    fn as_atom_token(&self) -> Option<&AtomToken> {
        if let Self::Named(x) = self {
            Some(x.as_atom_token())
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum RecordConstructOrIndexExpr {
    Construct(Box<RecordConstructExpr>),
    Index(Box<RecordIndexExpr>),
}

impl RecordConstructOrIndexExpr {
    pub fn record_name(&self) -> &AtomToken {
        match self {
            Self::Construct(x) => x
                .record
                .prefix()
                .1
                .as_atom_token()
                .expect("anonymous record has no record name"),
            Self::Index(x) => x
                .name
                .as_atom_token()
                .expect("anonymous record has no record name"),
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
            Self::Access(x) => x
                .index
                .name
                .as_atom_token()
                .expect("anonymous record has no record name"),
            Self::Update(x) => x
                .record
                .prefix()
                .1
                .1
                .as_atom_token()
                .expect("anonymous record has no record name"),
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
        if ts
            .peek::<(SharpSymbol, (RecordNameRef, DotSymbol))>()
            .is_some()
        {
            ts.resume_parse(value).map(Self::Access)
        } else {
            ts.resume_parse(value).map(Self::Update)
        }
    }
}

/// `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $NAME: [AtomToken] | [KeywordToken] | [VariableToken] | [AtomToken] `:` [AtomToken]
/// - $FIELD: ([AtomToken] | `_`) `=` [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordConstructExpr {
    record: RecordLike<(SharpSymbol, RecordNameRef), RecordField>,
}

/// `$VALUE` `#` `$NAME` `.` `$FIELD`
///
/// - $VALUE: [Expr]
/// - $NAME: [AtomToken] | [KeywordToken] | [VariableToken] | [AtomToken] `:` [AtomToken] | `_`
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
/// - $NAME: [AtomToken] | [KeywordToken] | [VariableToken] | [AtomToken] `:` [AtomToken] | `_`
/// - $FIELD: [AtomToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordIndexExpr {
    sharp: SharpSymbol,
    name: RecordNameRef,
    dot: DotSymbol,
    field: AtomToken,
}

/// `$VALUE` `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $VALUE: [Expr]
/// - $NAME: [AtomToken] | [KeywordToken] | [VariableToken] | [AtomToken] `:` [AtomToken] | `_`
/// - $FIELD: [AtomToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordUpdateExpr {
    record: RecordLike<(Expr, (SharpSymbol, RecordNameRef)), RecordField>,
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
            "#state{}",
            "#mod:state{}",
            "#_{}",
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
        let texts = ["#foo.bar", "#mod:foo.bar", "#_.bar"];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn record_access_works() {
        let texts = [
            "X#foo.bar",
            "X#mod:foo.bar",
            "X#_.bar",
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
            "M#mod:foo{}",
            "M#_{}",
            indoc::indoc! {"
            M#baz{
              qux = 1
             }#foo.bar"},
            indoc::indoc! {"
            M#mod:foo{
              qux = 1
             }"},
            indoc::indoc! {"
            M#_{
              qux = 1
             }"},
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
