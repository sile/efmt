//! Erlang expressions.
use self::bitstrings::BitstringExpr;
use self::blocks::BlockExpr;
use self::functions::FunctionExpr;
use self::records::{RecordAccessOrUpdateExpr, RecordConstructOrIndexExpr};
use crate::format::{self, Format};
use crate::items::generics::{Either, NonEmptyItems2, Parenthesized};
use crate::items::keywords::WhenKeyword;
use crate::items::styles::{ColumnIndent, Newline, Space};
use crate::items::symbols::{CommaSymbol, OpenBraceSymbol, SemicolonSymbol};
use crate::items::tokens::{
    AtomToken, CharToken, FloatToken, IntegerToken, SymbolToken, Token, VariableToken,
};
use crate::parse::{self, Parse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

mod bitstrings;
mod blocks;
mod calls;
pub(crate) mod functions; // TODO
mod lists;
mod maps;
mod records;
mod strings;
mod tuples;

pub use self::bitstrings::{BitstringComprehensionExpr, BitstringConstructExpr};
pub use self::blocks::{BeginExpr, CaseExpr, CatchExpr, IfExpr, ReceiveExpr, TryExpr};
pub use self::calls::{BinaryOp, BinaryOpCallExpr, FunctionCallExpr, UnaryOp, UnaryOpCallExpr};
pub use self::functions::{AnonymousFunctionExpr, DefinedFunctionExpr, NamedFunctionExpr};
pub use self::lists::{ListComprehensionExpr, ListConstructExpr, ListExpr};
pub use self::maps::{MapConstructExpr, MapUpdateExpr};
pub use self::records::{RecordAccessExpr, RecordConstructExpr, RecordIndexExpr, RecordUpdateExpr};
pub use self::strings::StringExpr;
pub use self::tuples::TupleExpr;

// TODO: refactor
#[derive(Debug, Clone, Span, Format)]
pub enum BaseExpr {
    List(Box<ListExpr>),
    Tuple(Box<TupleExpr>),
    MapConstruct(Box<MapConstructExpr>),
    RecordConstructOrIndex(Box<RecordConstructOrIndexExpr>),
    Bitstring(Box<BitstringExpr>),
    Function(Box<FunctionExpr>),
    UnaryOpCall(Box<UnaryOpCallExpr>),
    Parenthesized(Box<Parenthesized<Expr>>),
    Literal(LiteralExpr),
    Block(Box<BlockExpr>),

    // Left recursive.
    MapUpdate(Box<MapUpdateExpr>),
    RecordAccessOrUpdate(Box<RecordAccessOrUpdateExpr>),
}

impl Parse for BaseExpr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr = match ts.peek::<Token>() {
            Some(Token::Symbol(token)) => match token.value() {
                Symbol::OpenSquare => ts.parse().map(Self::List),
                Symbol::OpenBrace => ts.parse().map(Self::Tuple),
                Symbol::DoubleLeftAngle => ts.parse().map(Self::Bitstring),
                Symbol::OpenParen => ts.parse().map(Self::Parenthesized),
                Symbol::Sharp => {
                    if ts.peek::<(Token, OpenBraceSymbol)>().is_some() {
                        ts.parse().map(Self::MapConstruct)
                    } else {
                        ts.parse().map(Self::RecordConstructOrIndex)
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
        }?;

        let mut expr = expr;
        loop {
            match ts.peek::<SymbolToken>() {
                Some(token) => match token.value() {
                    Symbol::Sharp => {
                        if ts.peek::<(Token, OpenBraceSymbol)>().is_some() {
                            expr = ts.resume_parse(Expr::Base(expr)).map(Self::MapUpdate)?;
                        } else {
                            expr = ts
                                .resume_parse(Expr::Base(expr))
                                .map(Self::RecordAccessOrUpdate)?;
                        }
                    }
                    _ => return Ok(expr),
                },
                None => return Ok(expr),
            }
        }
    }
}

impl BaseExpr {
    pub fn is_atom_token(&self) -> bool {
        matches!(self, Self::Literal(LiteralExpr::Atom(_)))
    }

    pub fn is_integer_token(&self) -> bool {
        matches!(self, Self::Literal(LiteralExpr::Integer(_)))
    }

    pub fn is_unary_op_call_expr(&self) -> bool {
        matches!(self, Self::UnaryOpCall(_))
    }
}

#[derive(Debug, Clone, Span, Format)]
pub enum Expr {
    Base(BaseExpr),
    FunctionCall(Box<FunctionCallExpr>),
    BinaryOpCall(Box<BinaryOpCallExpr>),
}

impl Parse for Expr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr: BaseExpr = ts.parse()?;

        let expr = if let Some(token) = ts.peek::<SymbolToken>() {
            match token.value() {
                Symbol::Colon => ts
                    .resume_parse((expr.clone(), true))
                    .map(Self::FunctionCall)
                    .unwrap_or_else(|_| Self::Base(expr)),
                Symbol::OpenParen => ts.resume_parse((expr, false)).map(Self::FunctionCall)?,
                _ => Self::Base(expr),
            }
        } else {
            Self::Base(expr)
        };

        if ts.peek::<BinaryOp>().is_some() {
            ts.resume_parse(expr).map(Self::BinaryOpCall)
        } else {
            Ok(expr)
        }
    }
}

impl Expr {
    pub fn is_atom_token(&self) -> bool {
        matches!(self, Self::Base(BaseExpr::Literal(LiteralExpr::Atom(_))))
    }

    pub fn is_integer_token(&self) -> bool {
        matches!(self, Self::Base(BaseExpr::Literal(LiteralExpr::Integer(_))))
    }
}

/// [AtomToken] | [CharToken] | [FloatToken] | [IntegerToken] | [VariableToken] | [StringExpr]
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
enum AtomLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

impl From<AtomLikeExpr> for BaseExpr {
    fn from(x: AtomLikeExpr) -> Self {
        match x {
            AtomLikeExpr::Atom(x) => Self::Literal(LiteralExpr::Atom(x)),
            AtomLikeExpr::Variable(x) => Self::Literal(LiteralExpr::VariableToken(x)),
            AtomLikeExpr::Expr(x) => Self::Parenthesized(Box::new(x)),
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
enum IntegerLikeExpr {
    Integer(IntegerToken),
    Variable(VariableToken),
    Expr(Parenthesized<Expr>),
}

#[derive(Debug, Clone, Span, Parse)]
struct Body {
    exprs: NonEmptyItems2<Expr, Newline<CommaSymbol>>,
}

impl Body {
    fn exprs(&self) -> &[Expr] {
        self.exprs.items()
    }
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
struct Guard {
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
struct GuardCondition {
    conditions: NonEmptyItems2<Expr, Either<CommaSymbol, SemicolonSymbol>>,
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
            %---10---|%---20---|
            foo(A, B, C)
              when A =:= B ->
                C."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn space_char_works() {
        let texts = [indoc::indoc! {"
            foo($ , $ ) ->
                $ ."}];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }
}
