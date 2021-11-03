use crate::format::Format;
use crate::items::expressions::{Body, Expr, Guard, GuardCondition, IntegerLikeExpr};
use crate::items::generics::{Either, Items, Maybe, NonEmptyItems};
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, EndKeyword, IfKeyword, OfKeyword,
    ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::{ColonSymbol, CommaSymbol, RightArrowSymbol, SemicolonSymbol};
use crate::items::tokens::{AtomToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BlockExpr {
    Case(CaseExpr),
    If(IfExpr),
    Receive(ReceiveExpr),
    Begin(BeginExpr),
    Try(TryExpr),
    Catch(CatchExpr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CaseExpr {
    case: CaseKeyword,
    value: Expr,
    of: OfKeyword,
    clauses: NonEmptyItems<CaseClause, SemicolonSymbol>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CaseClause {
    pattern: Expr,
    guard: Maybe<Guard>,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: NonEmptyItems<IfClause, SemicolonSymbol>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct IfClause {
    condigion: GuardCondition,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: NonEmptyItems<Expr, CommaSymbol>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Items<CaseClause, SemicolonSymbol>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ReceiveTimeout {
    after: AfterKeyword,
    millis: IntegerLikeExpr,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TryExpr {
    r#try: TryKeyword,
    body: Body,
    clauses: Maybe<(OfKeyword, NonEmptyItems<CaseClause, SemicolonSymbol>)>,
    catch: Maybe<TryCatch>,
    after: Maybe<TryAfter>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TryCatch {
    catch: CatchKeyword,
    clauses: NonEmptyItems<CatchClause, SemicolonSymbol>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CatchClause {
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
    guard: Maybe<Guard>,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TryAfter {
    after: AfterKeyword,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CatchExpr {
    catch: CatchKeyword,
    expr: Expr,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::items::expressions::NonLeftRecursiveExpr;
    use crate::parse::parse_text;

    #[test]
    fn case_works() {
        let texts = [
            "case Foo of 1 -> 2 end",
            "case foo() of {1, 2} -> 3; A when is_integer(A), A > 100 -> A / 10 end",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::Case(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn if_works() {
        let texts = [
            "if true -> 2 end",
            "if A =:= {1, 2} -> 3; is_integer(A), A > 100 -> A / 10 end",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::If(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn receive_works() {
        let texts = [
            "receive {A, B} -> [A, B]; A when is_integer(A); is_atom(A) -> A end",
            "receive A -> A after 1000 -> timeout end",
            "receive after N -> timeout end",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::Receive(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn begin_works() {
        let texts = ["begin 1 end", "begin foo(bar,Baz), {[#{}]} end"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::Begin(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn try_works() {
        let texts = [
            "try 1 after 2 end",
            "try 1, 2, 3 catch E -> E end",
            "try X of {_, _} -> 1; [_, _] -> 2 catch _:E:Stacktrace when is_atom(E) -> foo after bar end",
            "try foo() catch throw:_ -> throw; error:_ -> error end"
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::Try(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }

    #[test]
    fn catch_works() {
        let texts = ["catch 1", "catch foo(bar,Baz) + 3"];
        for text in texts {
            let x = parse_text(text).unwrap();
            if let Expr::NonLeftRecursive(NonLeftRecursiveExpr::Block(x)) = &x {
                assert!(matches!(**x, BlockExpr::Catch(_)));
            } else {
                panic!("{:?}", x);
            }
        }
    }
}
