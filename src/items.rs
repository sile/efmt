use crate::format::Format;
use crate::items::components::{Either, Element};
use crate::items::tokens::AtomToken;
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
    fn get(&self) -> &self::forms::Form {
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

    pub(crate) fn is_block(&self) -> bool {
        self.0.is_block()
    }

    pub(crate) fn is_parenthesized(&self) -> bool {
        self.0.is_parenthesized()
    }

    pub(crate) fn as_atom(&self) -> Option<&str> {
        if let self::expressions::FullExpr::Base(x) = &self.0 {
            if let self::expressions::BaseExpr::Literal(x) = x {
                if let self::expressions::LiteralExpr::Atom(x) = x {
                    return Some(x.value());
                }
            }
        }
        None
    }

    pub(crate) fn as_string(&self) -> Option<&str> {
        if let self::expressions::FullExpr::Base(x) = &self.0 {
            if let self::expressions::BaseExpr::Literal(x) = x {
                if let self::expressions::LiteralExpr::String(x) = x {
                    if x.tokens().len() == 1 {
                        return Some(x.tokens()[0].value());
                    }
                }
            }
        }
        None
    }

    pub(crate) fn as_u32(&self, text: &str) -> Option<u32> {
        if let self::expressions::FullExpr::Base(x) = &self.0 {
            if let self::expressions::BaseExpr::Literal(x) = x {
                if let self::expressions::LiteralExpr::Integer(x) = x {
                    if let Ok(x) =
                        text[x.start_position().offset()..x.end_position().offset()].parse()
                    {
                        return Some(x);
                    }
                }
            }
        }
        None
    }

    pub(crate) fn as_list(&self) -> Option<&[Expr]> {
        if let self::expressions::FullExpr::Base(x) = &self.0 {
            if let self::expressions::BaseExpr::List(x) = x {
                if let self::expressions::ListExpr::Construct(x) = &**x {
                    return Some(x.items());
                }
            }
        }
        None
    }

    pub(crate) fn as_tuple(&self) -> Option<(Option<&AtomToken>, &[Expr])> {
        if let self::expressions::FullExpr::Base(x) = &self.0 {
            if let self::expressions::BaseExpr::Tuple(x) = x {
                return Some(x.items());
            }
        }
        None
    }
}
