use crate::format::Format;
use crate::items::tokens::SymbolToken;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::Symbol;

macro_rules! impl_traits {
    ($name:ident, $value:ident) => {
        impl Parse for $name {
            fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
                let token: SymbolToken = ts.parse()?;
                if token.value() == Symbol::$value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(ts, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format)]
pub struct OpenSquareSymbol(SymbolToken);
impl_traits!(OpenSquareSymbol, OpenSquare);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseSquareSymbol(SymbolToken);
impl_traits!(CloseSquareSymbol, CloseSquare);

#[derive(Debug, Clone, Span, Format)]
pub struct OpenParenSymbol(SymbolToken);
impl_traits!(OpenParenSymbol, OpenParen);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseParenSymbol(SymbolToken);
impl_traits!(CloseParenSymbol, CloseParen);

#[derive(Debug, Clone, Span, Format)]
pub struct OpenBraceSymbol(SymbolToken);
impl_traits!(OpenBraceSymbol, OpenBrace);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseBraceSymbol(SymbolToken);
impl_traits!(CloseBraceSymbol, CloseBrace);

#[derive(Debug, Clone, Span, Format)]
pub struct SharpSymbol(SymbolToken);
impl_traits!(SharpSymbol, Sharp);

#[derive(Debug, Clone, Span, Format)]
pub struct SlashSymbol(SymbolToken);
impl_traits!(SlashSymbol, Slash);

#[derive(Debug, Clone, Span, Format)]
pub struct DotSymbol(SymbolToken);
impl_traits!(DotSymbol, Dot);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleDotSymbol(SymbolToken);
impl_traits!(DoubleDotSymbol, DoubleDot);

#[derive(Debug, Clone, Span, Format)]
pub struct TripleDotSymbol(SymbolToken);
impl_traits!(TripleDotSymbol, TripleDot);

#[derive(Debug, Clone, Span, Format)]
pub struct CommaSymbol(SymbolToken);
impl_traits!(CommaSymbol, Comma);

#[derive(Debug, Clone, Span, Format)]
pub struct ColonSymbol(SymbolToken);
impl_traits!(ColonSymbol, Colon);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleColonSymbol(SymbolToken);
impl_traits!(DoubleColonSymbol, DoubleColon);

#[derive(Debug, Clone, Span, Format)]
pub struct SemicolonSymbol(SymbolToken);
impl_traits!(SemicolonSymbol, Semicolon);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleAmpersandSymbol(SymbolToken);
impl_traits!(DoubleAmpersandSymbol, DoubleAmpersand);

#[derive(Debug, Clone, Span, Format)]
pub struct MatchSymbol(SymbolToken);
impl_traits!(MatchSymbol, Match);

#[derive(Debug, Clone, Span, Format)]
pub struct MapMatchSymbol(SymbolToken);
impl_traits!(MapMatchSymbol, MapMatch);

#[derive(Debug, Clone, Span, Format)]
pub struct VerticalBarSymbol(SymbolToken);
impl_traits!(VerticalBarSymbol, VerticalBar);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleVerticalBarSymbol(SymbolToken);
impl_traits!(DoubleVerticalBarSymbol, DoubleVerticalBar);

#[derive(Debug, Clone, Span, Format)]
pub struct QuestionSymbol(SymbolToken);
impl_traits!(QuestionSymbol, Question);

impl QuestionSymbol {
    pub fn new(start: Position) -> Self {
        let end = Position::new(start.offset() + 1, start.line(), start.column() + 1);
        Self(SymbolToken::new(Symbol::Question, start, end))
    }
}

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleQuestionSymbol(SymbolToken);
impl_traits!(DoubleQuestionSymbol, DoubleQuestion);

#[derive(Debug, Clone, Span, Format)]
pub struct NotSymbol(SymbolToken);
impl_traits!(NotSymbol, Not);

#[derive(Debug, Clone, Span, Format)]
pub struct HyphenSymbol(SymbolToken);
impl_traits!(HyphenSymbol, Hyphen);

#[derive(Debug, Clone, Span, Format)]
pub struct MinusMinusSymbol(SymbolToken);
impl_traits!(MinusMinusSymbol, MinusMinus);

#[derive(Debug, Clone, Span, Format)]
pub struct PlusSymbol(SymbolToken);
impl_traits!(PlusSymbol, Plus);

#[derive(Debug, Clone, Span, Format)]
pub struct PlusPlusSymbol(SymbolToken);
impl_traits!(PlusPlusSymbol, PlusPlus);

#[derive(Debug, Clone, Span, Format)]
pub struct MultiplySymbol(SymbolToken);
impl_traits!(MultiplySymbol, Multiply);

#[derive(Debug, Clone, Span, Format)]
pub struct RightArrowSymbol(SymbolToken);
impl_traits!(RightArrowSymbol, RightArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct LeftArrowSymbol(SymbolToken);
impl_traits!(LeftArrowSymbol, LeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleRightArrowSymbol(SymbolToken);
impl_traits!(DoubleRightArrowSymbol, DoubleRightArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct StrictLeftArrowSymbol(SymbolToken);
impl_traits!(StrictLeftArrowSymbol, StrictLeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleLeftArrowSymbol(SymbolToken);
impl_traits!(DoubleLeftArrowSymbol, DoubleLeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleRightAngleSymbol(SymbolToken);
impl_traits!(DoubleRightAngleSymbol, DoubleRightAngle);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleLeftAngleSymbol(SymbolToken);
impl_traits!(DoubleLeftAngleSymbol, DoubleLeftAngle);

#[derive(Debug, Clone, Span, Format)]
pub struct StrictDoubleLeftArrowSymbol(SymbolToken);
impl_traits!(StrictDoubleLeftArrowSymbol, StrictDoubleLeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct EqSymbol(SymbolToken);
impl_traits!(EqSymbol, Eq);

#[derive(Debug, Clone, Span, Format)]
pub struct ExactEqSymbol(SymbolToken);
impl_traits!(ExactEqSymbol, ExactEq);

#[derive(Debug, Clone, Span, Format)]
pub struct NotEqSymbol(SymbolToken);
impl_traits!(NotEqSymbol, NotEq);

#[derive(Debug, Clone, Span, Format)]
pub struct ExactNotEqSymbol(SymbolToken);
impl_traits!(ExactNotEqSymbol, ExactNotEq);

#[derive(Debug, Clone, Span, Format)]
pub struct GreaterSymbol(SymbolToken);
impl_traits!(GreaterSymbol, Greater);

#[derive(Debug, Clone, Span, Format)]
pub struct GreaterEqSymbol(SymbolToken);
impl_traits!(GreaterEqSymbol, GreaterEq);

#[derive(Debug, Clone, Span, Format)]
pub struct LessSymbol(SymbolToken);
impl_traits!(LessSymbol, Less);

#[derive(Debug, Clone, Span, Format)]
pub struct LessEqSymbol(SymbolToken);
impl_traits!(LessEqSymbol, LessEq);

#[derive(Debug, Clone, Span, Format)]
pub struct MaybeMatchSymbol(SymbolToken);
impl_traits!(MaybeMatchSymbol, MaybeMatch);
