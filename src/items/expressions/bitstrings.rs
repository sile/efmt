use crate::format::{self, Format};
use crate::items::expressions::{BaseExpr, Expr};
use crate::items::generics::{Elements, Maybe, NonEmptyItems};
use crate::items::qualifiers::Qualifier;
use crate::items::styles::{ColumnIndent, RightSpace, Space};
use crate::items::symbols::{
    ColonSymbol, DoubleLeftAngleSymbol, DoubleRightAngleSymbol, DoubleVerticalBarSymbol,
    HyphenSymbol, SlashSymbol,
};
use crate::items::tokens::{AtomToken, IntegerToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BitstringExpr {
    Construct(BitstringConstructExpr),
    Comprehension(BitstringComprehensionExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringConstructExpr {
    open: DoubleLeftAngleSymbol,
    segments: Elements<BitstringSegment>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringComprehensionExpr {
    open: RightSpace<DoubleLeftAngleSymbol>,
    item: Expr,
    bar: Space<DoubleVerticalBarSymbol>,
    qualifiers: ColumnIndent<NonEmptyItems<Qualifier>>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BitstringSegment {
    value: BaseExpr,
    size: Maybe<BitstringSegmentSize>,
    ty: Maybe<BitstringSegmentType>,
}

impl Format for BitstringSegment {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.value.format(fmt)?;
        self.size.format(fmt)?;
        self.ty.format(fmt)?;
        Ok(())
    }

    fn should_be_packed(&self) -> bool {
        self.value.should_be_packed() && self.size.get().map_or(true, |x| x.size.should_be_packed())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentSize {
    colon: ColonSymbol,
    size: BaseExpr,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BitstringSegmentType {
    slash: SlashSymbol,
    specifiers: NonEmptyItems<BitstringSegmentTypeSpecifier, HyphenSymbol>,
}

impl Format for BitstringSegmentType {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.slash.format(fmt)?;
        for (item, delimiter) in self
            .specifiers
            .items
            .iter()
            .zip(self.specifiers.delimiters.iter())
        {
            item.format(fmt)?;
            delimiter.format(fmt)?;
        }
        self.specifiers
            .items
            .last()
            .expect("unreachable")
            .format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringSegmentTypeSpecifier {
    name: AtomToken,
    value: Maybe<(ColonSymbol, IntegerToken)>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bitstring_construct_works() {
        let texts = [
            "<<>>",
            indoc::indoc! {"
            <<1, 2, 3, 4, 5,
              6, 7, 8, 9>>"},
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
            << <<X>> || X <- [1,
                              2,
                              3]>>"},
            indoc::indoc! {"
            << foo(X,
                   Y,
                   Z,
                   bar(),
                   baz()) || X <- [1,
                                   2,
                                   3],
                             Y <= Z,
                             false>>"},
            indoc::indoc! {"
            << <<if
                     X < 10 ->
                         X +
                         $0;
                     true ->
                         X -
                         10 +
                         $A
                 end>> || <<X:4>> <= B>>"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
