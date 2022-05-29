use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::components::{BinaryOpStyle, Either, Element};
use crate::items::keywords::{
    BandKeyword, BnotKeyword, BorKeyword, BslKeyword, BsrKeyword, BxorKeyword, DivKeyword,
    RemKeyword,
};
use crate::items::symbols::{
    ColonSymbol, DoubleDotSymbol, HyphenSymbol, MultiplySymbol, PlusSymbol,
};
use crate::items::variables::UnderscoreVariable;
use crate::items::Type;
use crate::parse::Parse;
use crate::span::Span;

/// `*` | `+` | `-` | `div` | `rem` | `band` | `bor` | `bxor` | `bsl` | `bsr` | `..`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BinaryOp {
    Mul(MultiplySymbol),
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Div(DivKeyword),
    Rem(RemKeyword),
    Band(BandKeyword),
    Bor(BorKeyword),
    Bxor(BxorKeyword),
    Bsl(BslKeyword),
    Bsr(BsrKeyword),
    Range(DoubleDotSymbol),
}

impl<RHS> BinaryOpStyle<RHS> for BinaryOp {
    fn indent(&self) -> Indent {
        Indent::inherit()
    }

    fn newline(&self, _rhs: &RHS, _fmt: &Formatter) -> Newline {
        Newline::IfTooLong
    }

    fn needs_spaces(&self) -> bool {
        !matches!(self, Self::Range(_))
    }
}

/// `+` | `-` | `bnot`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Bnot(BnotKeyword),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub(super) struct BitstringItem(Either<BitstringUnitSize, BitstringBitsSize>);

impl Element for BitstringItem {
    fn is_packable(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringBitsSize {
    underscore: UnderscoreVariable,
    colon: ColonSymbol,
    size: Type,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringUnitSize {
    underscore0: UnderscoreVariable,
    colon: ColonSymbol,
    underscore1: UnderscoreVariable,
    mul: MultiplySymbol,
    size: Type,
}
