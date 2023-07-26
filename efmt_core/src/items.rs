use crate::format::{Format, Formatter};
use crate::items::components::Element;
use crate::items::expressions::{BaseExpr, FullExpr, ListExpr, LiteralExpr};
use crate::items::tokens::AtomToken;
use crate::parse::Parse;
use crate::span::Span;

pub mod expressions;
pub mod forms;
pub mod symbols;
pub mod tokens;
pub mod types;

pub(crate) mod atoms;
pub(crate) mod components;
pub(crate) mod config;
pub(crate) mod keywords;
pub(crate) mod macros;
pub(crate) mod variables;

mod module;

pub use self::config::Config;
use self::expressions::BinaryOpCallExpr;
pub use self::macros::Macro;
pub use self::module::Module;
use self::tokens::VariableToken;
pub use crate::items::components::Either;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ModuleOrConfig<const ALLOW_PARTIAL_FAILURE: bool = false>(
    Either<Module<ALLOW_PARTIAL_FAILURE>, Config>,
);

/// One of [forms].
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Form(self::forms::Form);

impl Form {
    pub fn get(&self) -> &self::forms::Form {
        &self.0
    }

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

impl Type {
    pub fn children(&self) -> impl Iterator<Item = &self::types::NonUnionType> {
        self.0.items().iter()
    }
}

impl Element for Type {
    fn is_packable(&self) -> bool {
        false
    }
}

/// One of [expressions].
#[derive(Debug, Clone, Span, Parse, Format, Element)]
pub struct Expr(FullExpr);

impl Expr {
    pub fn get(&self) -> &FullExpr {
        &self.0
    }

    pub(crate) fn from_variable(v: VariableToken) -> Self {
        Expr(FullExpr::Base(BaseExpr::Literal(LiteralExpr::Variable(v))))
    }

    pub(crate) fn as_binary_op(&self) -> Option<&BinaryOpCallExpr> {
        if let FullExpr::BinaryOpCall(x) = self.get() {
            Some(x)
        } else {
            None
        }
    }

    pub fn as_atom(&self) -> Option<&str> {
        if let FullExpr::Base(BaseExpr::Literal(LiteralExpr::Atom(x))) = &self.0 {
            Some(x.value())
        } else {
            None
        }
    }

    pub fn as_string(&self) -> Option<&str> {
        if let FullExpr::Base(BaseExpr::Literal(LiteralExpr::String(x))) = &self.0 {
            if x.tokens().len() == 1 {
                return Some(x.tokens()[0].value());
            }
        }
        None
    }

    pub fn as_u32(&self, text: &str) -> Option<u32> {
        if let FullExpr::Base(BaseExpr::Literal(LiteralExpr::Integer(x))) = &self.0 {
            if let Ok(x) = text[x.start_position().offset()..x.end_position().offset()].parse() {
                return Some(x);
            }
        }
        None
    }

    pub fn as_list(&self) -> Option<&[Expr]> {
        if let FullExpr::Base(BaseExpr::List(x)) = &self.0 {
            if let ListExpr::Construct(x) = &**x {
                return Some(x.items());
            }
        }
        None
    }

    pub fn as_tuple(&self) -> Option<(Option<&AtomToken>, &[Expr])> {
        if let FullExpr::Base(BaseExpr::Tuple(x)) = &self.0 {
            return Some(x.items());
        }
        None
    }

    fn try_format_app_file(&self, fmt: &mut Formatter) -> bool {
        if let FullExpr::Base(BaseExpr::Tuple(x)) = &self.0 {
            x.try_format_app_file(fmt)
        } else {
            false
        }
    }
}
