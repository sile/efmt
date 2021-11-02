use crate::format::Format;
use crate::items::tokens::KeywordToken;
use crate::parse::{self, Parse, Parser};
use crate::span::Span;
use erl_tokenize::values::Keyword;

macro_rules! impl_parse {
    ($name:ident,$value:ident) => {
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                let token: KeywordToken = parser.parse()?;
                if token.value() == Keyword::$value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(parser, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format)]
pub struct AfterKeyword(KeywordToken);
impl_parse!(AfterKeyword, After);

#[derive(Debug, Clone, Span, Format)]
pub struct AndKeyword(KeywordToken);
impl_parse!(AndKeyword, And);

#[derive(Debug, Clone, Span, Format)]
pub struct AndalsoKeyword(KeywordToken);
impl_parse!(AndalsoKeyword, Andalso);

#[derive(Debug, Clone, Span, Format)]
pub struct BandKeyword(KeywordToken);
impl_parse!(BandKeyword, Band);

#[derive(Debug, Clone, Span, Format)]
pub struct BeginKeyword(KeywordToken);
impl_parse!(BeginKeyword, Begin);

#[derive(Debug, Clone, Span, Format)]
pub struct BnotKeyword(KeywordToken);
impl_parse!(BnotKeyword, Bnot);

#[derive(Debug, Clone, Span, Format)]
pub struct BorKeyword(KeywordToken);
impl_parse!(BorKeyword, Bor);

#[derive(Debug, Clone, Span, Format)]
pub struct BslKeyword(KeywordToken);
impl_parse!(BslKeyword, Bsl);

#[derive(Debug, Clone, Span, Format)]
pub struct BsrKeyword(KeywordToken);
impl_parse!(BsrKeyword, Bsr);

#[derive(Debug, Clone, Span, Format)]
pub struct BxorKeyword(KeywordToken);
impl_parse!(BxorKeyword, Bxor);

#[derive(Debug, Clone, Span, Format)]
pub struct CaseKeyword(KeywordToken);
impl_parse!(CaseKeyword, Case);

#[derive(Debug, Clone, Span, Format)]
pub struct CatchKeyword(KeywordToken);
impl_parse!(CatchKeyword, Catch);

#[derive(Debug, Clone, Span, Format)]
pub struct CondKeyword(KeywordToken);
impl_parse!(CondKeyword, Cond);

#[derive(Debug, Clone, Span, Format)]
pub struct DivKeyword(KeywordToken);
impl_parse!(DivKeyword, Div);

#[derive(Debug, Clone, Span, Format)]
pub struct EndKeyword(KeywordToken);
impl_parse!(EndKeyword, End);

#[derive(Debug, Clone, Span, Format)]
pub struct FunKeyword(KeywordToken);
impl_parse!(FunKeyword, Fun);

#[derive(Debug, Clone, Span, Format)]
pub struct IfKeyword(KeywordToken);
impl_parse!(IfKeyword, If);

#[derive(Debug, Clone, Span, Format)]
pub struct LetKeyword(KeywordToken);
impl_parse!(LetKeyword, Let);

#[derive(Debug, Clone, Span, Format)]
pub struct NotKeyword(KeywordToken);
impl_parse!(NotKeyword, Not);

#[derive(Debug, Clone, Span, Format)]
pub struct OfKeyword(KeywordToken);
impl_parse!(OfKeyword, Of);

#[derive(Debug, Clone, Span, Format)]
pub struct OrKeyword(KeywordToken);
impl_parse!(OrKeyword, Or);

#[derive(Debug, Clone, Span, Format)]
pub struct OrelseKeyword(KeywordToken);
impl_parse!(OrelseKeyword, Orelse);

#[derive(Debug, Clone, Span, Format)]
pub struct ReceiveKeyword(KeywordToken);
impl_parse!(ReceiveKeyword, Receive);

#[derive(Debug, Clone, Span, Format)]
pub struct RemKeyword(KeywordToken);
impl_parse!(RemKeyword, Rem);

#[derive(Debug, Clone, Span, Format)]
pub struct TryKeyword(KeywordToken);
impl_parse!(TryKeyword, Try);

#[derive(Debug, Clone, Span, Format)]
pub struct WhenKeyword(KeywordToken);
impl_parse!(WhenKeyword, When);

#[derive(Debug, Clone, Span, Format)]
pub struct XorKeyword(KeywordToken);
impl_parse!(XorKeyword, Xor);
