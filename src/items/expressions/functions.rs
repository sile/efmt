use crate::format::Format;
use crate::items::expressions::{
    AtomLikeExpr, Body, Expr, Guard, IntegerLikeExpr, MaybeInlineBody,
};
use crate::items::generics::{Clauses, Items, Maybe, Parenthesized};
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::styles::{RightSpace, Space};
use crate::items::symbols::{ColonSymbol, RightArrowSymbol, SlashSymbol};
use crate::items::tokens::{AtomToken, IntegerToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionExpr {
    Defined(Box<DefinedFunctionExpr>),
    Anonymous(Box<AnonymousFunctionExpr>),
    Named(Box<NamedFunctionExpr>),
    NameAndArity(Box<NameAndArity<AtomToken, IntegerToken>>), // For attributes such as `-export`
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct DefinedFunctionExpr {
    fun: RightSpace<FunKeyword>,
    module: Maybe<ModulePrefix>,
    name_and_arity: NameAndArity,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct AnonymousFunctionExpr {
    fun: RightSpace<FunKeyword>,
    clauses: RightSpace<Clauses<FunctionClause<MaybeInlineBody>>>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionExpr {
    fun: RightSpace<FunKeyword>,
    clauses: RightSpace<Clauses<NamedFunctionClause>>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionClause<B = Body> {
    params: Parenthesized<Items<Expr>>,
    guard: Maybe<Guard>,
    arrow: Space<RightArrowSymbol>,
    body: B,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionClause {
    name: VariableToken,
    params: Parenthesized<Items<Expr>>,
    guard: Maybe<Guard>,
    arrow: Space<RightArrowSymbol>,
    body: MaybeInlineBody,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ModulePrefix {
    name: AtomLikeExpr,
    colon: ColonSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NameAndArity<N = AtomLikeExpr, A = IntegerLikeExpr> {
    name: N,
    slash: SlashSymbol,
    arity: A,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn defined_function_works() {
        let texts = ["fun foo/1", "fun foo:bar/Arity", "fun (foo()):Bar/(baz())"];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn anonymous_function_works() {
        let texts = [
            "fun () -> hi end",
            indoc::indoc! {"
                fun (a) -> a;
                    (A) -> A end"},
            indoc::indoc! {"
                fun ({a, b}, C) -> C;
                    (A, B) when is_integer(A);
                                is_atom(B) -> A end"},
            indoc::indoc! {"
                fun (A) ->
                        foo(),
                        bar,
                        baz(A) end"},
            indoc::indoc! {"
                fun (a) ->
                        foo(),
                        bar,
                        baz();
                    (A) -> A end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn named_function_works() {
        let texts = [
            "fun Foo() -> hi end",
            indoc::indoc! {"
                fun Foo(a) -> a;
                    Foo(A) -> A end"},
            indoc::indoc! {"
                fun Foo({a, b}, C) -> C;
                    Foo(A, _B) when is_integer(A) -> A end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
