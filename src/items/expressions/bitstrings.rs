use crate::format::{Format, Formatter};
use crate::items::components::{BitstringLike, Either, Element, Maybe, NonEmptyItems};
use crate::items::expressions::components::ComprehensionExpr;
#[cfg(doc)]
use crate::items::expressions::components::Qualifier;
use crate::items::expressions::BaseExpr;
use crate::items::symbols::{
    ColonSymbol, DoubleLeftAngleSymbol, DoubleRightAngleSymbol, HyphenSymbol, SlashSymbol,
};
use crate::items::tokens::{AtomToken, IntegerToken};
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BitstringExpr {
    Construct(BitstringConstructExpr),
    Comprehension(BitstringComprehensionExpr),
}

impl BitstringExpr {
    pub fn children(&self) -> impl Iterator<Item = Either<&BaseExpr, &Expr>> {
        match self {
            BitstringExpr::Construct(x) => Either::A(x.children().map(Either::A)),
            BitstringExpr::Comprehension(x) => Either::B(x.children().map(Either::B)),
        }
    }
}

/// `<<` (`$SEGMENT` `,`?)* `>>`
///
/// - $SEGMENT: [Expr] `$SIZE`? `$TYPE`?
/// - $SIZE: `:` [Expr]
/// - $TYPE: `/` ([AtomToken] `-`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringConstructExpr(BitstringLike<BitstringSegment>);

impl BitstringConstructExpr {
    fn children(&self) -> impl Iterator<Item = &BaseExpr> {
        self.0.items().iter().flat_map(|x| x.children())
    }
}

/// `<<` [Expr] `||` ([Qualifier] `,`?)+  `>>`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringComprehensionExpr(
    ComprehensionExpr<DoubleLeftAngleSymbol, DoubleRightAngleSymbol>,
);

impl BitstringComprehensionExpr {
    fn children(&self) -> impl Iterator<Item = &Expr> {
        self.0.children()
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringSegment {
    value: BaseExpr,
    size: Maybe<BitstringSegmentSize>,
    ty: Maybe<BitstringSegmentType>,
}

impl BitstringSegment {
    fn children(&self) -> impl Iterator<Item = &BaseExpr> {
        std::iter::once(&self.value).chain(self.size.get().into_iter().map(|x| &x.size))
    }
}

impl Element for BitstringSegment {
    fn is_packable(&self) -> bool {
        self.value.is_packable() && self.size.is_packable()
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringSegmentSize {
    colon: ColonSymbol,
    size: BaseExpr,
}

impl Element for BitstringSegmentSize {
    fn is_packable(&self) -> bool {
        self.size.is_packable()
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct BitstringSegmentType {
    slash: SlashSymbol,
    specifiers: NonEmptyItems<BitstringSegmentTypeSpecifier, HyphenSymbol>,
}

impl Format for BitstringSegmentType {
    fn format(&self, fmt: &mut Formatter) {
        self.slash.format(fmt);
        for (item, delimiter) in self
            .specifiers
            .items()
            .iter()
            .zip(self.specifiers.delimiters().iter())
        {
            item.format(fmt);
            delimiter.format(fmt);
        }
        self.specifiers
            .items()
            .last()
            .expect("unreachable")
            .format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringSegmentTypeSpecifier {
    name: AtomToken,
    value: Maybe<(ColonSymbol, IntegerToken)>,
}

#[cfg(test)]
mod tests {
    use crate::items::Expr;

    #[test]
    fn bitstring_construct_works() {
        let texts = [
            "<<>>",
            indoc::indoc! {"
            <<1, 2, 3, 4, 5, 6,
              7, 8, 9>>"},
            "<<1, 2:16, 3>>",
            "<<<<\"foo\">>/binary>>",
            indoc::indoc! {"
            <<(3 bsr 30 + 2):0,
              $k:[]/signed-integer>>"},
            indoc::indoc! {"
            <<1,
              (foo()):4/little-signed-integer-unit:8,
              C/binary>>"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn bitstring_comprehension_works() {
        let texts = [
            indoc::indoc! {"
            << <<X>>
               || X <- [1, 2,
                        3] >>"},
            indoc::indoc! {"
            << <<X, Y>>
               || X := Y <- [1, 2,
                             3] >>"},
            indoc::indoc! {"
            << (foo(X,
                    Y,
                    Z,
                    bar(),
                    baz()))
               || X <- [1, 2, 3,
                        4, 5],
                  Y <= Z,
                  false >>"},
            indoc::indoc! {"
            << <<if
                     X < 10 ->
                         X + $0;
                     true ->
                         X - 10 + $A
                 end>>
               || <<X:4>> <= B >>"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
