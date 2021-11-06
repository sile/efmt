use crate::format::{Item, Tree};
use crate::items::expressions::{Body, Expr, Guard, GuardCondition, IntegerLikeExpr};
use crate::items::generics::{Clauses, Either, Maybe, NonEmptyItems};
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, EndKeyword, IfKeyword, OfKeyword,
    ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::{ColonSymbol, RightArrowSymbol};
use crate::items::tokens::{AtomToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum BlockExpr {
    Case(Box<CaseExpr>),
    If(Box<IfExpr>),
    Receive(Box<ReceiveExpr>),
    Begin(Box<BeginExpr>),
    Try(Box<TryExpr>),
    Catch(Box<CatchExpr>),
}

#[derive(Debug, Clone, Span, Parse)]
pub struct CaseExpr {
    case: CaseKeyword,
    value: Expr,
    of: OfKeyword,
    clauses: Clauses<CaseClause>,
    end: EndKeyword,
}

impl Item for CaseExpr {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            Tree::linefeed(Tree::space(self.case.tree())),
            Tree::child(self.value.tree(), false),
            Tree::space(self.of.tree()),
            Tree::child(Tree::linefeed(self.clauses.tree()), true),
            self.end.tree(),
        ])
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct CaseClause {
    pattern: Expr,
    guard: Maybe<Guard>,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Item for CaseClause {
    fn tree(&self) -> Tree {
        let left = if let Some(_guard) = self.guard.get() {
            todo!()
        } else {
            Box::new(self.pattern.tree())
        };
        Tree::Unbalanced {
            left,
            delimiter: self.arrow.to_item_span(),
            right: Box::new(self.body.tree()),
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Clauses<IfClause>,
    end: EndKeyword,
}

impl Item for IfExpr {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            Tree::newline(self.r#if.tree()),
            Tree::child(Tree::linefeed(self.clauses.tree()), true),
            self.end.tree(),
        ])
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct IfClause {
    condigion: GuardCondition,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: NonEmptyItems<Expr>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Maybe<Clauses<CaseClause>>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

impl Item for ReceiveExpr {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            Tree::newline(self.receive.tree()),
            Tree::child(Tree::linefeed(self.clauses.tree()), true),
            self.timeout.tree(),
            Tree::linefeed(self.end.tree()),
        ])
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ReceiveTimeout {
    after: AfterKeyword,
    millis: IntegerLikeExpr,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Item for ReceiveTimeout {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            Tree::newline(self.after.tree()),
            Tree::child(
                Tree::Unbalanced {
                    left: Box::new(self.millis.tree()),
                    delimiter: self.arrow.to_item_span(),
                    right: Box::new(self.body.tree()),
                },
                true,
            ),
        ])
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct TryExpr {
    r#try: TryKeyword,
    body: Body,
    clauses: Maybe<(OfKeyword, Clauses<CaseClause>)>,
    catch: Maybe<TryCatch>,
    after: Maybe<TryAfter>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct TryCatch {
    catch: CatchKeyword,
    clauses: Clauses<CatchClause>,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct CatchClause {
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
    guard: Maybe<Guard>,
    arrow: RightArrowSymbol,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct TryAfter {
    after: AfterKeyword,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct CatchExpr {
    catch: CatchKeyword,
    expr: Expr,
}

impl Item for CatchExpr {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![
            Tree::right_space(self.catch.tree()),
            Tree::child(self.expr.tree(), false),
        ])
    }
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
