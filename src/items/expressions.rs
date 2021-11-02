use crate::format::Format;
use crate::items::generics::Parenthesized;
use crate::items::tokens::{
    AtomToken, CharToken, FloatToken, IntegerToken, StringToken, VariableToken,
};
use crate::parse::Parse;
use crate::span::Span;

pub mod functions;
pub mod lists;

pub use self::functions::FunctionExpr;
pub use self::lists::ListExpr;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Expr {
    List(Box<ListExpr>),
    Function(Box<FunctionExpr>),
    Literal(LiteralExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum LiteralExpr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringToken),
    VariableToken(VariableToken),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum AtomLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum IntegerLikeExpr {
    Integer(IntegerToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}
