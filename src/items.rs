use crate::format::Format;
use crate::items::components::{Either, Element};
use crate::parse::Parse;
use crate::span::Span;

pub mod expressions;
pub mod forms;
pub mod tokens;
pub mod types;

pub(crate) mod atoms;
pub(crate) mod components;
pub(crate) mod config;
pub(crate) mod keywords;
pub(crate) mod macros;
pub(crate) mod symbols;
pub(crate) mod variables;

mod module;

pub use self::config::Config;
pub use self::macros::Macro;
pub use self::module::Module;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ModuleOrConfig(Either<Module, Config>);

/// One of [forms].
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Form(self::forms::Form);

impl Form {
    pub(crate) fn is_func_spec(&self) -> bool {
        matches!(self.0, self::forms::Form::FunSpec(_))
    }

    pub(crate) fn is_func_decl(&self) -> bool {
        matches!(self.0, self::forms::Form::FunDecl(_))
    }
}

/// One of [types].
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Type(self::types::UnionType);

impl Element for Type {
    fn is_packable(&self) -> bool {
        false
    }
}

/// One of [expressions].
#[derive(Debug, Clone, Span, Parse, Format, Element)]
pub struct Expr(self::expressions::FullExpr);

impl Expr {
    pub(crate) fn get(&self) -> &self::expressions::FullExpr {
        &self.0
    }
}
