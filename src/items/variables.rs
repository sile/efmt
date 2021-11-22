use crate::format2::Format2;
use crate::items::tokens::VariableToken;
use crate::parse::{self, Parse, TokenStream};
use crate::span::Span;

macro_rules! impl_parse {
    ($name:ident, $value:expr) => {
        impl Parse for $name {
            fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
                let token: VariableToken = ts.parse()?;
                if token.value() == $value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(ts, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format2)]
pub struct UnderscoreVariable(VariableToken);
impl_parse!(UnderscoreVariable, "_");
