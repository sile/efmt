use crate::format::{self, Format};
use crate::items::generics::{Either, NonEmptyItems, Parenthesized};
use crate::items::keywords::WhenKeyword;
use crate::items::styles::{Child, ColumnIndent, Newline, Space};
use crate::items::symbols::{CommaSymbol, OpenBraceSymbol, SemicolonSymbol};
use crate::items::tokens::{
    AtomToken, CharToken, FloatToken, IntegerToken, SymbolToken, Token, VariableToken,
};
use crate::parse::{self, Parse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

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
pub use self::calls::{BinaryOp, BinaryOpCallExpr, FunctionCallExpr, UnaryOpCallExpr};
pub use self::functions::FunctionExpr;
pub use self::lists::ListExpr;
pub use self::maps::{MapConstructExpr, MapUpdateExpr};
pub use self::records::{RecordAccessOrUpdateExpr, RecordConstructOrIndexExpr};
pub use self::strings::StringExpr;
pub use self::tuples::TupleExpr;

#[derive(Debug, Clone, Span, Format)]
pub enum Expr {
    BinaryOpCall(Box<BinaryOpCallExpr>),
    NonLeftRecursive(NonLeftRecursiveExpr),
}

impl Parse for Expr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr: NonLeftRecursiveExpr = ts.parse()?;
        if ts.peek::<BinaryOp>().is_some() {
            ts.resume_parse(expr).map(Self::BinaryOpCall)
        } else {
            Ok(Self::NonLeftRecursive(expr))
        }
    }
}

impl Expr {
    pub fn is_integer_token(&self) -> bool {
        if let Self::NonLeftRecursive(x) = self {
            x.is_integer_token()
        } else {
            false
        }
    }
}

// TODO: rename
#[derive(Debug, Clone, Span, Format)]
pub enum BaseExpr {
    List(Box<ListExpr>),
    Tuple(Box<TupleExpr>),
    Map(Box<MapConstructExpr>),
    Record(Box<RecordConstructOrIndexExpr>),
    Bitstring(Box<BitstringExpr>),
    Function(Box<FunctionExpr>),
    UnaryOpCall(Box<UnaryOpCallExpr>),
    Parenthesized(Box<Parenthesized<Expr>>),
    Literal(LiteralExpr),
    Block(Box<BlockExpr>),
}

impl BaseExpr {
    pub fn is_atom_token(&self) -> bool {
        matches!(self, Self::Literal(LiteralExpr::Atom(_)))
    }

    pub fn is_integer_token(&self) -> bool {
        matches!(self, Self::Literal(LiteralExpr::Integer(_)))
    }
}

impl Parse for BaseExpr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        match ts.peek::<Token>() {
            Some(Token::Symbol(token)) => match token.value() {
                Symbol::OpenSquare => ts.parse().map(Self::List),
                Symbol::OpenBrace => ts.parse().map(Self::Tuple),
                Symbol::DoubleLeftAngle => ts.parse().map(Self::Bitstring),
                Symbol::OpenParen => ts.parse().map(Self::Parenthesized),
                Symbol::Sharp => {
                    if ts.peek::<(Token, OpenBraceSymbol)>().is_some() {
                        ts.parse().map(Self::Map)
                    } else {
                        ts.parse().map(Self::Record)
                    }
                }
                _ => ts.parse().map(Self::UnaryOpCall),
            },
            Some(Token::Keyword(token)) => match token.value() {
                Keyword::Fun => ts.parse().map(Self::Function),
                Keyword::Bnot | Keyword::Not => ts.parse().map(Self::UnaryOpCall),
                _ => ts.parse().map(Self::Block),
            },
            Some(_) => ts.parse().map(Self::Literal),
            None => Err(parse::Error::UnexpectedEof {
                position: ts.current_position(),
            }),
        }
    }
}

#[derive(Debug, Clone, Span, Format)]
pub enum NonLeftRecursiveExpr {
    Base(BaseExpr),
    FunctionCall(Box<FunctionCallExpr>),
    Map(Box<MapUpdateExpr>),
    Record(Box<RecordAccessOrUpdateExpr>),
}

impl NonLeftRecursiveExpr {
    pub fn is_atom_token(&self) -> bool {
        if let Self::Base(x) = self {
            x.is_atom_token()
        } else {
            false
        }
    }

    pub fn is_integer_token(&self) -> bool {
        if let Self::Base(x) = self {
            x.is_integer_token()
        } else {
            false
        }
    }
}

impl Parse for NonLeftRecursiveExpr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr: BaseExpr = ts.parse()?;
        match ts.peek::<SymbolToken>() {
            Some(token) => match token.value() {
                Symbol::Sharp => {
                    if ts.peek::<(Token, OpenBraceSymbol)>().is_some() {
                        ts.resume_parse(expr).map(Self::Map)
                    } else {
                        ts.resume_parse(expr).map(Self::Record)
                    }
                }
                Symbol::Colon => {
                    if let Ok(x) = ts.resume_parse((expr.clone(), true)) {
                        Ok(Self::FunctionCall(x))
                    } else {
                        Ok(Self::Base(expr))
                    }
                }
                Symbol::OpenParen => ts.resume_parse((expr, false)).map(Self::FunctionCall),
                _ => Ok(Self::Base(expr)),
            },
            None => Ok(Self::Base(expr)),
        }
    }
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

// TODO: delete?
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

#[derive(Debug, Clone, Span, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Child<Expr>, Newline<CommaSymbol>>,
}

impl Format for Body {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion()
            .indent_offset(4)
            .trailing_columns(1) // '.' or ',' or ';' (TODO move other place?)
            .enter(|fmt| {
                fmt.write_newline()?;
                self.exprs.format(fmt)
            })
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MaybeInlineBody {
    exprs: NonEmptyItems<Child<Expr>, Newline<CommaSymbol>>,
}

impl Format for MaybeInlineBody {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let mut options = fmt.subregion().trailing_columns(4); // ' end'
        if self.exprs.get().len() > 1 {
            options = options.indent_offset(4)
        }
        options.enter(|fmt| {
            if self.exprs.get().len() > 1 {
                fmt.write_newline()?;
            }
            self.exprs.format(fmt)
        })
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Guard {
    when: Space<WhenKeyword>,
    condition: ColumnIndent<GuardCondition>,
}

impl Format for Guard {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let format = |fmt: &mut format::Formatter| {
            self.when.format(fmt)?;
            self.condition.format(fmt)?;
            Ok(())
        };

        if fmt.current_relative_column() < 2 {
            fmt.subregion()
                .trailing_columns(3) // ' ->'
                .enter(format)?;
        } else if fmt
            .subregion()
            .trailing_columns(3) // ' ->'
            .forbid_too_long_line()
            .forbid_multi_line()
            .enter(format)
            .is_err()
        {
            fmt.subregion()
                .indent_offset(2)
                .trailing_columns(3) // ' ->'
                .enter_with_newline(format)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct GuardCondition {
    conditions: NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>,
}

#[cfg(test)]
mod tests {
    use crate::items::forms::Form;

    #[test]
    fn when_works() {
        let texts = [
            indoc::indoc! {"
            foo(A) when A ->
                A."},
            indoc::indoc! {"
            foo(A, B, C)
              when A =:= B ->
                C."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }
}
