use crate::format::Format;
use crate::items::tokens::{KeywordToken, SymbolToken};
use crate::parse::{self, Parse, Parser};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

macro_rules! impl_parse_keyword {
    ($name:ident) => {
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                let token: KeywordToken = parser.parse()?;
                if token.value() == Keyword::$name {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(
                        parser,
                        token.into(),
                        &format!("{:?}", Keyword::$name.as_str()),
                    ))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format)]
pub struct After(KeywordToken);
impl_parse_keyword!(After);

#[derive(Debug, Clone, Span, Format)]
pub struct And(KeywordToken);
impl_parse_keyword!(And);

#[derive(Debug, Clone, Span, Format)]
pub struct Andalso(KeywordToken);
impl_parse_keyword!(Andalso);

#[derive(Debug, Clone, Span, Format)]
pub struct Band(KeywordToken);
impl_parse_keyword!(Band);

#[derive(Debug, Clone, Span, Format)]
pub struct Begin(KeywordToken);
impl_parse_keyword!(Begin);

#[derive(Debug, Clone, Span, Format)]
pub struct Bnot(KeywordToken);
impl_parse_keyword!(Bnot);

#[derive(Debug, Clone, Span, Format)]
pub struct Bor(KeywordToken);
impl_parse_keyword!(Bor);

#[derive(Debug, Clone, Span, Format)]
pub struct Bsl(KeywordToken);
impl_parse_keyword!(Bsl);

#[derive(Debug, Clone, Span, Format)]
pub struct Bsr(KeywordToken);
impl_parse_keyword!(Bsr);

#[derive(Debug, Clone, Span, Format)]
pub struct Bxor(KeywordToken);
impl_parse_keyword!(Bxor);

#[derive(Debug, Clone, Span, Format)]
pub struct Case(KeywordToken);
impl_parse_keyword!(Case);

#[derive(Debug, Clone, Span, Format)]
pub struct Catch(KeywordToken);
impl_parse_keyword!(Catch);

#[derive(Debug, Clone, Span, Format)]
pub struct Cond(KeywordToken);
impl_parse_keyword!(Cond);

#[derive(Debug, Clone, Span, Format)]
pub struct Div(KeywordToken);
impl_parse_keyword!(Div);

#[derive(Debug, Clone, Span, Format)]
pub struct End(KeywordToken);
impl_parse_keyword!(End);

#[derive(Debug, Clone, Span, Format)]
pub struct Fun(KeywordToken);
impl_parse_keyword!(Fun);

#[derive(Debug, Clone, Span, Format)]
pub struct If(KeywordToken);
impl_parse_keyword!(If);

#[derive(Debug, Clone, Span, Format)]
pub struct Let(KeywordToken);
impl_parse_keyword!(Let);

#[derive(Debug, Clone, Span, Format)]
pub struct Not(KeywordToken);
impl_parse_keyword!(Not);

#[derive(Debug, Clone, Span, Format)]
pub struct Of(KeywordToken);
impl_parse_keyword!(Of);

#[derive(Debug, Clone, Span, Format)]
pub struct Or(KeywordToken);
impl_parse_keyword!(Or);

#[derive(Debug, Clone, Span, Format)]
pub struct Orelse(KeywordToken);
impl_parse_keyword!(Orelse);

#[derive(Debug, Clone, Span, Format)]
pub struct Receive(KeywordToken);
impl_parse_keyword!(Receive);

#[derive(Debug, Clone, Span, Format)]
pub struct Rem(KeywordToken);
impl_parse_keyword!(Rem);

#[derive(Debug, Clone, Span, Format)]
pub struct Try(KeywordToken);
impl_parse_keyword!(Try);

#[derive(Debug, Clone, Span, Format)]
pub struct When(KeywordToken);
impl_parse_keyword!(When);

#[derive(Debug, Clone, Span, Format)]
pub struct Xor(KeywordToken);
impl_parse_keyword!(Xor);

macro_rules! impl_parse_symbol {
    ($name:ident) => {
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                let token: SymbolToken = parser.parse()?;
                if token.value() == Symbol::$name {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(
                        parser,
                        token.into(),
                        &format!("{:?}", Symbol::$name.as_str()),
                    ))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format)]
pub struct OpenSquare(SymbolToken);
impl_parse_symbol!(OpenSquare);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseSquare(SymbolToken);
impl_parse_symbol!(CloseSquare);

#[derive(Debug, Clone, Span, Format)]
pub struct OpenParen(SymbolToken);
impl_parse_symbol!(OpenParen);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseParen(SymbolToken);
impl_parse_symbol!(CloseParen);

#[derive(Debug, Clone, Span, Format)]
pub struct OpenBrace(SymbolToken);
impl_parse_symbol!(OpenBrace);

#[derive(Debug, Clone, Span, Format)]
pub struct CloseBrace(SymbolToken);
impl_parse_symbol!(CloseBrace);

#[derive(Debug, Clone, Span, Format)]
pub struct Sharp(SymbolToken);
impl_parse_symbol!(Sharp);

#[derive(Debug, Clone, Span, Format)]
pub struct Slash(SymbolToken);
impl_parse_symbol!(Slash);

#[derive(Debug, Clone, Span, Format)]
pub struct Dot(SymbolToken);
impl_parse_symbol!(Dot);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleDot(SymbolToken);
impl_parse_symbol!(DoubleDot);

#[derive(Debug, Clone, Span, Format)]
pub struct TripleDot(SymbolToken);
impl_parse_symbol!(TripleDot);

#[derive(Debug, Clone, Span, Format)]
pub struct Comma(SymbolToken);
impl_parse_symbol!(Comma);

#[derive(Debug, Clone, Span, Format)]
pub struct Colon(SymbolToken);
impl_parse_symbol!(Colon);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleColon(SymbolToken);
impl_parse_symbol!(DoubleColon);

#[derive(Debug, Clone, Span, Format)]
pub struct Semicolon(SymbolToken);
impl_parse_symbol!(Semicolon);

#[derive(Debug, Clone, Span, Format)]
pub struct Match(SymbolToken);
impl_parse_symbol!(Match);

#[derive(Debug, Clone, Span, Format)]
pub struct MapMatch(SymbolToken);
impl_parse_symbol!(MapMatch);

#[derive(Debug, Clone, Span, Format)]
pub struct VerticalBar(SymbolToken);
impl_parse_symbol!(VerticalBar);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleVerticalBar(SymbolToken);
impl_parse_symbol!(DoubleVerticalBar);

#[derive(Debug, Clone, Span, Format)]
pub struct Question(SymbolToken);
impl_parse_symbol!(Question);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleQuestion(SymbolToken);
impl_parse_symbol!(DoubleQuestion);

// TODO
// #[derive(Debug, Clone, Span, Format)]
// pub struct Not(SymbolToken);
// impl_parse_symbol!(Not);

#[derive(Debug, Clone, Span, Format)]
pub struct Hyphen(SymbolToken);
impl_parse_symbol!(Hyphen);

#[derive(Debug, Clone, Span, Format)]
pub struct MinusMinus(SymbolToken);
impl_parse_symbol!(MinusMinus);

#[derive(Debug, Clone, Span, Format)]
pub struct Plus(SymbolToken);
impl_parse_symbol!(Plus);

#[derive(Debug, Clone, Span, Format)]
pub struct PlusPlus(SymbolToken);
impl_parse_symbol!(PlusPlus);

#[derive(Debug, Clone, Span, Format)]
pub struct Multiply(SymbolToken);
impl_parse_symbol!(Multiply);

#[derive(Debug, Clone, Span, Format)]
pub struct RightArrow(SymbolToken);
impl_parse_symbol!(RightArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct LeftArrow(SymbolToken);
impl_parse_symbol!(LeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleRightArrow(SymbolToken);
impl_parse_symbol!(DoubleRightArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleLeftArrow(SymbolToken);
impl_parse_symbol!(DoubleLeftArrow);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleRightAngle(SymbolToken);
impl_parse_symbol!(DoubleRightAngle);

#[derive(Debug, Clone, Span, Format)]
pub struct DoubleLeftAngle(SymbolToken);
impl_parse_symbol!(DoubleLeftAngle);

#[derive(Debug, Clone, Span, Format)]
pub struct Eq(SymbolToken);
impl_parse_symbol!(Eq);

#[derive(Debug, Clone, Span, Format)]
pub struct ExactEq(SymbolToken);
impl_parse_symbol!(ExactEq);

#[derive(Debug, Clone, Span, Format)]
pub struct NotEq(SymbolToken);
impl_parse_symbol!(NotEq);

#[derive(Debug, Clone, Span, Format)]
pub struct ExactNotEq(SymbolToken);
impl_parse_symbol!(ExactNotEq);

#[derive(Debug, Clone, Span, Format)]
pub struct Greater(SymbolToken);
impl_parse_symbol!(Greater);

#[derive(Debug, Clone, Span, Format)]
pub struct GreaterEq(SymbolToken);
impl_parse_symbol!(GreaterEq);

#[derive(Debug, Clone, Span, Format)]
pub struct Less(SymbolToken);
impl_parse_symbol!(Less);

#[derive(Debug, Clone, Span, Format)]
pub struct LessEq(SymbolToken);
impl_parse_symbol!(LessEq);
