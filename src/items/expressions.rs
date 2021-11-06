use crate::format::{Item, Tree};
use crate::items::generics::{Either, NonEmptyItems, Parenthesized};
use crate::items::keywords::WhenKeyword;
use crate::items::symbols::{CommaSymbol, SemicolonSymbol};
use crate::items::tokens::{
    AtomToken, CharToken, FloatToken, IntegerToken, StringToken, VariableToken,
};
use crate::parse::Parse;
use crate::span::Span;

pub mod bitstrings;
pub mod blocks;
pub mod calls;
pub mod functions;
pub mod lists;
pub mod maps;
pub mod records;
pub mod tuples;

pub use self::bitstrings::BitstringExpr;
pub use self::blocks::BlockExpr;
pub use self::calls::{BinaryOpCallExpr, FunctionCallExpr, UnaryOpCallExpr};
pub use self::functions::FunctionExpr;
pub use self::lists::ListExpr;
pub use self::maps::MapExpr;
pub use self::records::RecordExpr;
pub use self::tuples::TupleExpr;

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum Expr {
    BinaryOpCall(Box<BinaryOpCallExpr>),
    NonLeftRecursive(NonLeftRecursiveExpr),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum NonLeftRecursiveExpr {
    List(Box<ListExpr>),
    Tuple(Box<TupleExpr>),
    Map(Box<MapExpr>),
    Record(Box<RecordExpr>),
    Bitstring(Box<BitstringExpr>),
    Function(Box<FunctionExpr>),
    FunctionCall(Box<FunctionCallExpr>),
    UnaryOpCall(Box<UnaryOpCallExpr>),
    Parenthesized(Box<Parenthesized<Expr>>),
    Literal(LiteralExpr),
    Block(Box<BlockExpr>),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum LiteralExpr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringToken),
    VariableToken(VariableToken),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum AtomLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum VariableLikeExpr {
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum IntegerLikeExpr {
    Integer(IntegerToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Expr>,
}

impl Body {
    pub fn exprs(&self) -> &[Expr] {
        self.exprs.get()
    }
}

impl Item for Body {
    fn tree(&self) -> Tree {
        let mut tree = Box::new(self.exprs.tree());
        if self.exprs.get().len() > 1 {
            tree = Box::new(Tree::Linefeed(tree));
        }
        Tree::Child {
            tree,
            maybe_newline: false,
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct Guard {
    when: WhenKeyword,
    condition: GuardCondition,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct GuardCondition {
    conditions: NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>,
}
