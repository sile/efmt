use crate::format::Format;
use crate::format2::Format2;
use crate::items::tokens::KeywordToken;
use crate::parse::{self, Parse, TokenStream};
use crate::span::Span;
use erl_tokenize::values::Keyword;

macro_rules! impl_traits {
    ($name:ident,$value:ident) => {
        impl Parse for $name {
            fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
                let token: KeywordToken = ts.parse()?;
                if token.value() == Keyword::$value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(ts, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct AfterKeyword(KeywordToken);
impl_traits!(AfterKeyword, After);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct AndKeyword(KeywordToken);
impl_traits!(AndKeyword, And);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct AndalsoKeyword(KeywordToken);
impl_traits!(AndalsoKeyword, Andalso);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BandKeyword(KeywordToken);
impl_traits!(BandKeyword, Band);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BeginKeyword(KeywordToken);
impl_traits!(BeginKeyword, Begin);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BnotKeyword(KeywordToken);
impl_traits!(BnotKeyword, Bnot);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BorKeyword(KeywordToken);
impl_traits!(BorKeyword, Bor);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BslKeyword(KeywordToken);
impl_traits!(BslKeyword, Bsl);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BsrKeyword(KeywordToken);
impl_traits!(BsrKeyword, Bsr);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct BxorKeyword(KeywordToken);
impl_traits!(BxorKeyword, Bxor);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct CaseKeyword(KeywordToken);
impl_traits!(CaseKeyword, Case);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct CatchKeyword(KeywordToken);
impl_traits!(CatchKeyword, Catch);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct CondKeyword(KeywordToken);
impl_traits!(CondKeyword, Cond);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct DivKeyword(KeywordToken);
impl_traits!(DivKeyword, Div);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct EndKeyword(KeywordToken);
impl_traits!(EndKeyword, End);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct FunKeyword(KeywordToken);
impl_traits!(FunKeyword, Fun);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct IfKeyword(KeywordToken);
impl_traits!(IfKeyword, If);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct LetKeyword(KeywordToken);
impl_traits!(LetKeyword, Let);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct NotKeyword(KeywordToken);
impl_traits!(NotKeyword, Not);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct OfKeyword(KeywordToken);
impl_traits!(OfKeyword, Of);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct OrKeyword(KeywordToken);
impl_traits!(OrKeyword, Or);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct OrelseKeyword(KeywordToken);
impl_traits!(OrelseKeyword, Orelse);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct ReceiveKeyword(KeywordToken);
impl_traits!(ReceiveKeyword, Receive);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct RemKeyword(KeywordToken);
impl_traits!(RemKeyword, Rem);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct TryKeyword(KeywordToken);
impl_traits!(TryKeyword, Try);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct WhenKeyword(KeywordToken);
impl_traits!(WhenKeyword, When);

#[derive(Debug, Clone, Span, Format, Format2)]
pub struct XorKeyword(KeywordToken);
impl_traits!(XorKeyword, Xor);
