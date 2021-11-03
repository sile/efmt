use crate::format::Format;
use crate::items::expressions::{AtomLikeExpr, Body, Expr, Guard, IntegerLikeExpr};
use crate::items::generics::{Items, Maybe, NonEmptyItems, Parenthesized};
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::styles::Space;
use crate::items::symbols::{
    ColonSymbol, CommaSymbol, RightArrowSymbol, SemicolonSymbol, SlashSymbol,
};
use crate::items::tokens::VariableToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionExpr {
    Defined(Box<DefinedFunctionExpr>),
    Anonymous(Box<AnonymousFunctionExpr>),
    Named(Box<NamedFunctionExpr>),
    NameAndArity(Box<NameAndArity>), // For attributes such as `-export`
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct DefinedFunctionExpr {
    fun: FunKeyword,
    module: Maybe<ModulePrefix>,
    name_and_arity: NameAndArity,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct AnonymousFunctionExpr {
    fun: FunKeyword,
    clauses: NonEmptyItems<FunctionClause, SemicolonSymbol>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionExpr {
    fun: FunKeyword,
    clauses: NonEmptyItems<NamedFunctionClause, SemicolonSymbol>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionClause {
    params: Space<Parenthesized<Items<Expr, CommaSymbol>>>,
    guard: Space<Maybe<Guard>>,
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionClause {
    name: VariableToken,
    params: Parenthesized<Items<Expr, CommaSymbol>>,
    guard: Maybe<Guard>,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ModulePrefix {
    name: AtomLikeExpr,
    colon: ColonSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NameAndArity {
    name: AtomLikeExpr,
    slash: SlashSymbol,
    arity: IntegerLikeExpr,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::{Expr, NonLeftRecursiveExpr};
    use crate::parse::parse_text;

    #[test]
    fn defined_function_works() {
        let texts = ["fun foo/1", "fun foo:bar/Arity", "fun (foo()):Bar/(baz())"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Function(x)) = &x {
                assert!(matches!(**x, FunctionExpr::Defined(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn anonymous_function_works() {
        let texts = [
            "fun () -> hi end",
            "fun (a) -> a; (A) -> A end",
            "fun ({a,b},C) -> C; (A, _B) when is_integer(A) -> A end",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Function(x)) = &x {
                assert!(matches!(**x, FunctionExpr::Anonymous(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn named_function_works() {
        let texts = [
            "fun Foo() -> hi end",
            "fun Foo(a) -> a; Foo(A) -> A end",
            "fun Foo({a,b},C) -> C; Foo(A, _B) when is_integer(A) -> A end",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Function(x)) = &x {
                assert!(matches!(**x, FunctionExpr::Named(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }
}
