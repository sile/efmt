use crate::format::Format;
use crate::items::expressions::{AtomLikeExpr, IntegerLikeExpr};
use crate::items::symbols::SlashSymbol;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionExpr {
    NameAndArity(NameAndArity), // For attributes such as `-export`
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NameAndArity {
    name: AtomLikeExpr,
    slash: SlashSymbol,
    arity: IntegerLikeExpr,
}
