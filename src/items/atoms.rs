use crate::format::Format;
use crate::items::tokens::AtomToken;
use crate::parse::{self, Parse, Parser};
use crate::span::Span;

macro_rules! impl_parse {
    ($name:ident, $value:expr) => {
        impl Parse for $name {
            fn parse(parser: &mut Parser) -> parse::Result<Self> {
                let token: AtomToken = parser.parse()?;
                if token.value() == $value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(parser, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format)]
pub struct DefineAtom(AtomToken);
impl_parse!(DefineAtom, "define");

#[derive(Debug, Clone, Span, Format)]
pub struct IncludeAtom(AtomToken);
impl_parse!(IncludeAtom, "include");

#[derive(Debug, Clone, Span, Format)]
pub struct IncludeLibAtom(AtomToken);
impl_parse!(IncludeLibAtom, "include_lib");
