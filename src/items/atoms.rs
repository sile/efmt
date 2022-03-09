use crate::format::Format;
use crate::items::components::Element;
use crate::items::tokens::AtomToken;
use crate::parse::{self, Parse, TokenStream};
use crate::span::Span;

macro_rules! impl_parse {
    ($name:ident, $value:expr) => {
        impl Parse for $name {
            fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
                let token: AtomToken = ts.parse()?;
                if token.value() == $value {
                    Ok(Self(token))
                } else {
                    Err(parse::Error::unexpected_token(ts, token.into()))
                }
            }
        }
    };
}

#[derive(Debug, Clone, Span, Format, Element)]
pub struct DefineAtom(AtomToken);
impl_parse!(DefineAtom, "define");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct IncludeAtom(AtomToken);
impl_parse!(IncludeAtom, "include");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct IncludeLibAtom(AtomToken);
impl_parse!(IncludeLibAtom, "include_lib");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct SpecAtom(AtomToken);
impl_parse!(SpecAtom, "spec");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct TypeAtom(AtomToken);
impl_parse!(TypeAtom, "type");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct OpaqueAtom(AtomToken);
impl_parse!(OpaqueAtom, "opaque");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct CallbackAtom(AtomToken);
impl_parse!(CallbackAtom, "callback");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct RecordAtom(AtomToken);
impl_parse!(RecordAtom, "record");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct ExportAtom(AtomToken);
impl_parse!(ExportAtom, "export");

#[derive(Debug, Clone, Span, Format, Element)]
pub struct ExportTypeAtom(AtomToken);
impl_parse!(ExportTypeAtom, "export_type");
