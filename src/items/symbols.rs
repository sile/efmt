use crate::format::Item;
use crate::items::tokens::SymbolToken;
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};
use erl_tokenize::values::Symbol;

macro_rules! impl_parse {
    ($name:ident, $value:ident) => {
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                let token: SymbolToken = parser.parse()?;
                if token.value() == Symbol::$value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(parser, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Item)]
pub struct OpenSquareSymbol(SymbolToken);
impl_parse!(OpenSquareSymbol, OpenSquare);

#[derive(Debug, Clone, Span, Item)]
pub struct CloseSquareSymbol(SymbolToken);
impl_parse!(CloseSquareSymbol, CloseSquare);

#[derive(Debug, Clone, Span, Item)]
pub struct OpenParenSymbol(SymbolToken);
impl_parse!(OpenParenSymbol, OpenParen);

#[derive(Debug, Clone, Span, Item)]
pub struct CloseParenSymbol(SymbolToken);
impl_parse!(CloseParenSymbol, CloseParen);

#[derive(Debug, Clone, Span, Item)]
pub struct OpenBraceSymbol(SymbolToken);
impl_parse!(OpenBraceSymbol, OpenBrace);

#[derive(Debug, Clone, Span, Item)]
pub struct CloseBraceSymbol(SymbolToken);
impl_parse!(CloseBraceSymbol, CloseBrace);

#[derive(Debug, Clone, Span, Item)]
pub struct SharpSymbol(SymbolToken);
impl_parse!(SharpSymbol, Sharp);

#[derive(Debug, Clone, Span, Item)]
pub struct SlashSymbol(SymbolToken);
impl_parse!(SlashSymbol, Slash);

#[derive(Debug, Clone, Span, Item)]
pub struct DotSymbol(SymbolToken);
impl_parse!(DotSymbol, Dot);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleDotSymbol(SymbolToken);
impl_parse!(DoubleDotSymbol, DoubleDot);

#[derive(Debug, Clone, Span, Item)]
pub struct TripleDotSymbol(SymbolToken);
impl_parse!(TripleDotSymbol, TripleDot);

#[derive(Debug, Clone, Span, Item)]
pub struct CommaSymbol(SymbolToken);
impl_parse!(CommaSymbol, Comma);

#[derive(Debug, Clone, Span, Item)]
pub struct ColonSymbol(SymbolToken);
impl_parse!(ColonSymbol, Colon);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleColonSymbol(SymbolToken);
impl_parse!(DoubleColonSymbol, DoubleColon);

#[derive(Debug, Clone, Span, Item)]
pub struct SemicolonSymbol(SymbolToken);
impl_parse!(SemicolonSymbol, Semicolon);

#[derive(Debug, Clone, Span, Item)]
pub struct MatchSymbol(SymbolToken);
impl_parse!(MatchSymbol, Match);

#[derive(Debug, Clone, Span, Item)]
pub struct MapMatchSymbol(SymbolToken);
impl_parse!(MapMatchSymbol, MapMatch);

#[derive(Debug, Clone, Span, Item)]
pub struct VerticalBarSymbol(SymbolToken);
impl_parse!(VerticalBarSymbol, VerticalBar);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleVerticalBarSymbol(SymbolToken);
impl_parse!(DoubleVerticalBarSymbol, DoubleVerticalBar);

#[derive(Debug, Clone, Span, Item)]
pub struct QuestionSymbol(SymbolToken);
impl_parse!(QuestionSymbol, Question);

impl QuestionSymbol {
    pub fn new(start: Position) -> Self {
        let end = Position::new(start.offset() + 1, start.line(), start.column() + 1);
        Self(SymbolToken::new(Symbol::Question, start, end))
    }
}

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleQuestionSymbol(SymbolToken);
impl_parse!(DoubleQuestionSymbol, DoubleQuestion);

#[derive(Debug, Clone, Span, Item)]
pub struct NotSymbol(SymbolToken);
impl_parse!(NotSymbol, Not);

#[derive(Debug, Clone, Span, Item)]
pub struct HyphenSymbol(SymbolToken);
impl_parse!(HyphenSymbol, Hyphen);

#[derive(Debug, Clone, Span, Item)]
pub struct MinusMinusSymbol(SymbolToken);
impl_parse!(MinusMinusSymbol, MinusMinus);

#[derive(Debug, Clone, Span, Item)]
pub struct PlusSymbol(SymbolToken);
impl_parse!(PlusSymbol, Plus);

#[derive(Debug, Clone, Span, Item)]
pub struct PlusPlusSymbol(SymbolToken);
impl_parse!(PlusPlusSymbol, PlusPlus);

#[derive(Debug, Clone, Span, Item)]
pub struct MultiplySymbol(SymbolToken);
impl_parse!(MultiplySymbol, Multiply);

#[derive(Debug, Clone, Span, Item)]
pub struct RightArrowSymbol(SymbolToken);
impl_parse!(RightArrowSymbol, RightArrow);

#[derive(Debug, Clone, Span, Item)]
pub struct LeftArrowSymbol(SymbolToken);
impl_parse!(LeftArrowSymbol, LeftArrow);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleRightArrowSymbol(SymbolToken);
impl_parse!(DoubleRightArrowSymbol, DoubleRightArrow);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleLeftArrowSymbol(SymbolToken);
impl_parse!(DoubleLeftArrowSymbol, DoubleLeftArrow);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleRightAngleSymbol(SymbolToken);
impl_parse!(DoubleRightAngleSymbol, DoubleRightAngle);

#[derive(Debug, Clone, Span, Item)]
pub struct DoubleLeftAngleSymbol(SymbolToken);
impl_parse!(DoubleLeftAngleSymbol, DoubleLeftAngle);

#[derive(Debug, Clone, Span, Item)]
pub struct EqSymbol(SymbolToken);
impl_parse!(EqSymbol, Eq);

#[derive(Debug, Clone, Span, Item)]
pub struct ExactEqSymbol(SymbolToken);
impl_parse!(ExactEqSymbol, ExactEq);

#[derive(Debug, Clone, Span, Item)]
pub struct NotEqSymbol(SymbolToken);
impl_parse!(NotEqSymbol, NotEq);

#[derive(Debug, Clone, Span, Item)]
pub struct ExactNotEqSymbol(SymbolToken);
impl_parse!(ExactNotEqSymbol, ExactNotEq);

#[derive(Debug, Clone, Span, Item)]
pub struct GreaterSymbol(SymbolToken);
impl_parse!(GreaterSymbol, Greater);

#[derive(Debug, Clone, Span, Item)]
pub struct GreaterEqSymbol(SymbolToken);
impl_parse!(GreaterEqSymbol, GreaterEq);

#[derive(Debug, Clone, Span, Item)]
pub struct LessSymbol(SymbolToken);
impl_parse!(LessSymbol, Less);

#[derive(Debug, Clone, Span, Item)]
pub struct LessEqSymbol(SymbolToken);
impl_parse!(LessEqSymbol, LessEq);
