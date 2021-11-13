use crate::format::{self, Format};
use crate::items::generics::{Either, NonEmptyItems, Parenthesized};
use crate::items::keywords::WhenKeyword;
use crate::items::styles::{Child, ColumnIndent, Newline, Space};
use crate::items::symbols::{CommaSymbol, SemicolonSymbol};
use crate::items::tokens::{AtomToken, CharToken, FloatToken, IntegerToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

pub mod bitstrings;
pub mod blocks;
pub mod calls;
pub mod functions;
pub mod lists;
pub mod maps;
pub mod records;
pub mod strings;
pub mod tuples;

pub use self::bitstrings::BitstringExpr;
pub use self::blocks::BlockExpr;
pub use self::calls::{BinaryOpCallExpr, FunctionCallExpr, UnaryOpCallExpr};
pub use self::functions::FunctionExpr;
pub use self::lists::ListExpr;
pub use self::maps::MapExpr;
pub use self::records::RecordExpr;
pub use self::strings::StringExpr;
pub use self::tuples::TupleExpr;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Expr {
    BinaryOpCall(Box<BinaryOpCallExpr>),
    NonLeftRecursive(NonLeftRecursiveExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
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

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum LiteralExpr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringExpr),
    VariableToken(VariableToken),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum AtomLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum VariableLikeExpr {
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum IntegerLikeExpr {
    Integer(IntegerToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Child<Expr>, Newline<CommaSymbol>>,
}

impl Format for Body {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.with_subregion(
            format::RegionOptions::new()
                .newline()
                .indent(format::IndentMode::offset(4))
                .trailing_item_size(1),
            |fmt| fmt.format_item(&self.exprs),
        )
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MaybeInlineBody {
    exprs: NonEmptyItems<Child<Expr>, Newline<CommaSymbol>>,
}

impl Format for MaybeInlineBody {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let mut options = format::RegionOptions::new().trailing_item_size(4); // ' end'
        if self.exprs.get().len() > 1 {
            options = options.newline().indent(format::IndentMode::offset(4));
        }
        fmt.with_subregion(options, |fmt| fmt.format_item(&self.exprs))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Guard {
    when: Space<WhenKeyword>,
    condition: ColumnIndent<GuardCondition>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct GuardCondition {
    conditions: NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>,
}
