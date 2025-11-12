use crate::format::{Format, Formatter};
use crate::items::components::{Either, MapLike};
use crate::items::symbols::{
    CloseBraceSymbol, DoubleRightArrowSymbol, OpenBraceSymbol, SharpSymbol,
};
use crate::items::Expr;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

use super::components::ComprehensionExpr;

#[derive(Debug, Clone, Span, Parse, Format)]
#[expect(clippy::large_enum_variant)]
pub enum MapExpr {
    Construct(Box<MapConstructExpr>),
    Comprehension(MapComprehensionExpr),
}

impl MapExpr {
    pub fn children(&self) -> impl Iterator<Item = &Expr> {
        match self {
            Self::Construct(x) => Either::A(
                x.0.items()
                    .flat_map(|(k, v)| std::iter::once(k).chain(std::iter::once(v))),
            ),
            Self::Comprehension(x) => Either::B(x.0.children()),
        }
    }
}

/// `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapConstructExpr(MapLike<SharpSymbol, Expr>);

/// `#` `{` `$ENTRY` || ([Qualifier] `,`?)+ `}`
///
/// - $ENTRY: `Expr` `=>` `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapComprehensionExpr(
    ComprehensionExpr<(SharpSymbol, OpenBraceSymbol), CloseBraceSymbol, MapComprehensionValue>,
);

impl<Open, Close> ComprehensionExpr<Open, Close, MapComprehensionValue> {
    pub(crate) fn children(&self) -> impl Iterator<Item = &Expr> {
        std::iter::once(&self.value().key)
            .chain(std::iter::once(&self.value().value))
            .chain(self.qualifiers().iter().flat_map(|x| x.children()))
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct MapComprehensionValue {
    key: Expr,
    delimiter: DoubleRightArrowSymbol,
    value: Expr,
}

impl Format for MapComprehensionValue {
    fn format(&self, fmt: &mut Formatter) {
        self.key.format(fmt);
        fmt.write_space();
        self.delimiter.format(fmt);
        fmt.write_space();
        self.value.format(fmt);
    }
}

/// `$VALUE` `#` `{` (`$ENTRY`, `,`?)* `}`
///
/// - $VALUE: `Expr`
/// - $ENTRY: `Expr` (`:=` | `=>`) `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapUpdateExpr(MapLike<(Expr, SharpSymbol), Expr>);

impl MapUpdateExpr {
    pub fn children(&self) -> impl Iterator<Item = &Expr> {
        std::iter::once(&self.0.prefix().0).chain(
            self.0
                .items()
                .flat_map(|(k, v)| std::iter::once(k).chain(std::iter::once(v))),
        )
    }
}

impl ResumeParse<Expr> for MapUpdateExpr {
    fn resume_parse(ts: &mut parse::TokenStream, value: Expr) -> parse::Result<Self> {
        Ok(Self(MapLike::new((value, ts.parse()?), ts.parse()?)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn map_construct_works() {
        let texts = [
            "#{}",
            indoc::indoc! {"
            #{
              1 => 2,
              333 => {444, 55},
              foo => {666, 777,
                      888}
             }"},
            indoc::indoc! {"
            #{
              1 => 2,
              333 => {444, 55}
             }"},
            indoc::indoc! {"
            #{1 => 2, 333 => {444, 55}}"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn map_comprehension_works() {
        let texts = [
            "#{ Key => Value || Key := Value <- MapOrIterator }",
            "#{ K => V || <<K, V>> <= Binary }",
            "#{ K => V || <<K, V>> <:= Binary }",
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn map_update_works() {
        let texts = [
            "M#{}",
            "M#{}#{}",
            indoc::indoc! {"
            (foo())#{
              1 => 2,
              foo := {Bar, baz}
             }"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
