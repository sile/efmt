use crate::format2::Format2;
use crate::parse::Parse;
use crate::span::Span;

pub mod expressions;
pub mod forms;
pub mod tokens;
pub mod types;

pub(crate) mod atoms;
pub(crate) mod generics;
pub(crate) mod keywords;
pub(crate) mod macros;
pub(crate) mod symbols;
pub(crate) mod variables;

mod module;

pub use self::macros::Macro;
pub use self::module::Module;

/// One of [forms].
#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct Form(self::forms::Form);

/// One of [types].
#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct Type(self::types::UnionType);
