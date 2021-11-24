//! Erlang expressions.
use self::bitstrings::BitstringExpr;
use self::blocks::BlockExpr;
use self::functions::FunctionExpr;
use self::lists::ListExpr;
use self::records::{RecordAccessOrUpdateExpr, RecordConstructOrIndexExpr};
use crate::format::Format;
use crate::items::components::{Either, Element, Parenthesized};
use crate::items::symbols::OpenBraceSymbol;
use crate::items::tokens::{
    AtomToken, CharToken, FloatToken, IntegerToken, LexicalToken, SymbolToken, VariableToken,
};
use crate::items::Expr;
use crate::parse::{self, Parse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

pub mod components;

mod bitstrings;
mod blocks;
mod calls;
mod functions;
mod lists;
mod maps;
mod records;
mod strings;
mod tuples;

pub use self::bitstrings::{BitstringComprehensionExpr, BitstringConstructExpr};
pub use self::blocks::{BeginExpr, CaseExpr, CatchExpr, IfExpr, ReceiveExpr, TryExpr};
pub use self::calls::{BinaryOpCallExpr, FunctionCallExpr, UnaryOpCallExpr};
pub use self::functions::{AnonymousFunctionExpr, DefinedFunctionExpr, NamedFunctionExpr};
pub use self::lists::{ListComprehensionExpr, ListConstructExpr};
pub use self::maps::{MapConstructExpr, MapUpdateExpr};
pub use self::records::{RecordAccessExpr, RecordConstructExpr, RecordIndexExpr, RecordUpdateExpr};
pub use self::strings::StringExpr;
pub use self::tuples::TupleExpr;

#[derive(Debug, Clone, Span, Format)]
pub(crate) enum BaseExpr {
    List(Box<ListExpr>),
    Tuple(Box<TupleExpr>),
    MapConstruct(Box<MapConstructExpr>),
    RecordConstructOrIndex(Box<RecordConstructOrIndexExpr>),
    Bitstring(Box<BitstringExpr>),
    Function(Box<FunctionExpr>),
    UnaryOpCall(Box<UnaryOpCallExpr>),
    Parenthesized(Box<Parenthesized<FullExpr>>),
    Literal(LiteralExpr),
    Block(Box<BlockExpr>),

    // Left recursive.
    MapUpdate(Box<MapUpdateExpr>),
    RecordAccessOrUpdate(Box<RecordAccessOrUpdateExpr>),
}

impl Parse for BaseExpr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr = match ts.peek::<LexicalToken>() {
            Some(LexicalToken::Symbol(token)) => match token.value() {
                Symbol::OpenSquare => ts.parse().map(Self::List),
                Symbol::OpenBrace => ts.parse().map(Self::Tuple),
                Symbol::DoubleLeftAngle => ts.parse().map(Self::Bitstring),
                Symbol::OpenParen => ts.parse().map(Self::Parenthesized),
                Symbol::Sharp => {
                    if ts.peek::<(LexicalToken, OpenBraceSymbol)>().is_some() {
                        ts.parse().map(Self::MapConstruct)
                    } else {
                        ts.parse().map(Self::RecordConstructOrIndex)
                    }
                }
                _ => ts.parse().map(Self::UnaryOpCall),
            },
            Some(LexicalToken::Keyword(token)) => match token.value() {
                Keyword::Fun => ts.parse().map(Self::Function),
                Keyword::Bnot | Keyword::Not => ts.parse().map(Self::UnaryOpCall),
                _ => ts.parse().map(Self::Block),
            },
            Some(_) => ts.parse().map(Self::Literal),
            None => Err(parse::Error::unexpected_eof(ts)),
        }?;

        let mut expr = expr;
        loop {
            match ts.peek::<SymbolToken>() {
                Some(token) => match token.value() {
                    Symbol::Sharp => {
                        if ts.peek::<(LexicalToken, OpenBraceSymbol)>().is_some() {
                            expr = ts
                                .resume_parse(Expr(FullExpr::Base(expr)))
                                .map(Self::MapUpdate)?;
                        } else {
                            expr = ts
                                .resume_parse(Expr(FullExpr::Base(expr)))
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

impl Element for BaseExpr {
    fn is_packable(&self) -> bool {
        match self {
            Self::UnaryOpCall(x) => x.item().is_packable(),
            Self::Literal(x) => x.is_packable(),
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Span, Format)]
pub(crate) enum FullExpr {
    Base(BaseExpr),
    FunctionCall(Box<FunctionCallExpr>),
    BinaryOpCall(Box<BinaryOpCallExpr>),
}

impl Parse for FullExpr {
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

        if ts.peek::<self::components::BinaryOp>().is_some() {
            ts.resume_parse(Expr(expr)).map(Self::BinaryOpCall)
        } else {
            Ok(expr)
        }
    }
}

impl FullExpr {
    pub fn is_atom_token(&self) -> bool {
        matches!(self, Self::Base(BaseExpr::Literal(LiteralExpr::Atom(_))))
    }

    pub fn is_integer_token(&self) -> bool {
        matches!(self, Self::Base(BaseExpr::Literal(LiteralExpr::Integer(_))))
    }
}

impl Element for FullExpr {
    fn is_packable(&self) -> bool {
        if let Self::Base(x) = self {
            x.is_packable()
        } else {
            false
        }
    }
}

/// [AtomToken] | [CharToken] | [FloatToken] | [IntegerToken] | [VariableToken] | [StringExpr]
#[derive(Debug, Clone, Span, Parse, Format, Element)]
pub enum LiteralExpr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringExpr),
    VariableToken(VariableToken),
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
