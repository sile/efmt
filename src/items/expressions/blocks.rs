use crate::format::Format;
use crate::items::expressions::{Body, Expr, Guard, GuardCondition};
use crate::items::generics::{Clauses2, Either, Maybe, NonEmptyItems2};
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, EndKeyword, IfKeyword, OfKeyword,
    ReceiveKeyword, TryKeyword,
};
use crate::items::styles::{Block, ColumnIndent, Newline, RightSpace, Space, TrailingColumns};
use crate::items::symbols::{ColonSymbol, RightArrowSymbol};
use crate::items::tokens::{AtomToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BlockExpr {
    Case(Box<CaseExpr>),
    If(Box<IfExpr>),
    Receive(Box<ReceiveExpr>),
    Begin(Box<BeginExpr>),
    Try(Box<TryExpr>),
    Catch(Box<CatchExpr>),
}

/// `case` [Expr] `of` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` `$BODY`
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CaseExpr {
    case: Space<CaseKeyword>,
    value: TrailingColumns<Expr, 3>, // " of"
    of: Space<OfKeyword>,
    clauses: Block<Clauses2<CaseClause, 0>>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CaseClause {
    // TODO: trailing columns
    pattern: Expr,
    guard: Maybe<Guard>,
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

/// `if` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$GUARD` `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Block<Clauses2<IfClause, 0>>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct IfClause {
    condigion: TrailingColumns<GuardCondition, 3>, // " ->"
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

/// `begin` `$BODY` `end`
///
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: Block<NonEmptyItems2<Expr>>,
    end: EndKeyword,
}

/// `receive` (`$CLAUSE` `;`?)* `$TIMEOUT`? `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` `$BODY`
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $TIMEOUT: `after` [Expr] `->` `$BODY`
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ReceiveExpr {
    receive: Newline<ReceiveKeyword>,
    clauses: Maybe<Block<Clauses2<CaseClause, 0>>>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ReceiveTimeout {
    after: AfterKeyword,
    clause: Block<ReceiveTimeoutClause>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ReceiveTimeoutClause {
    timeout: TrailingColumns<Expr, 3>, // " ->"
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

/// `try` `$BODY` `$BRANCHES`? `$CATCH`? `$AFTER`? `end`
///
/// - $BODY: ([Expr] `,`?)+
/// - $BRANCHES: (`of` (`$BRANCH_CLAUSE` `;`?)+)
/// - $BRANCH_CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` `$BODY`
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $CATCH: `catch` (`$CATCH_CLAUSE` `;`?)+
/// - $CATCH_CLAUSE: `$ERROR_CLASS`? [Expr] `$STACKTRACE`? (`when` `$GUARD`)? `->` `$BODY`
/// - $ERROR_CLASS: ([AtomToken] | [VariableToken]) `:`
/// - $STACKTRACE: `:` [VariableToken]
/// - $AFTER: `after` `$BODY`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TryExpr {
    r#try: TryKeyword,
    body: Newline<Body>,
    clauses: Maybe<(OfKeyword, Block<Clauses2<CaseClause, 0>>)>,
    catch: Maybe<TryCatch>,
    after: Maybe<TryAfter>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct TryCatch {
    catch: CatchKeyword,
    clauses: Block<Clauses2<CatchClause, 0>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CatchClause {
    // TODO: trailing column
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
    guard: Maybe<Guard>,
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct TryAfter {
    after: AfterKeyword,
    body: Newline<Body>,
}

/// `catch` [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CatchExpr {
    catch: RightSpace<CatchKeyword>,
    expr: ColumnIndent<Expr>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn case_works() {
        let texts = [
            indoc::indoc! {"
            case Foo of
                1 ->
                    2
            end"},
            indoc::indoc! {"
            case foo() of
                {1, 2} ->
                    3;
                A when is_integer(A),
                       A >
                       100 ->
                    A / 10
            end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn if_works() {
        let texts = [
            indoc::indoc! {"
                if
                    true ->
                        2
                end"},
            indoc::indoc! {"
                if
                    A =:= {1, 2} ->
                        3;
                    is_integer(A),
                    A > 100 ->
                        A / 10
                end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn receive_works() {
        let texts = [
            indoc::indoc! {"
                receive
                    {A, B} ->
                        [A, B];
                    A when is_integer(A);
                           is_atom(A) ->
                        A
                end"},
            indoc::indoc! {"
                receive
                    A ->
                        A
                after
                    1000 ->
                        timeout
                end"},
            indoc::indoc! {"
                receive
                after
                    N ->
                        timeout
                end"},
            indoc::indoc! {"
                receive
                after
                    infinity ->
                        timeout
                end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn begin_works() {
        let texts = [
            indoc::indoc! {"
                begin
                    1
                end"},
            indoc::indoc! {"
                begin
                    foo(bar, Baz),
                    {[#{}]}
                end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn try_works() {
        let texts = [
            indoc::indoc! {"
                try
                    1
                after
                    2
                end"},
            indoc::indoc! {"
                try
                    1,
                    2,
                    3
                catch
                    E ->
                        E
                end"},
            indoc::indoc! {"
                try
                    X
                of
                    {_, _} ->
                        1;
                    [_, _] ->
                        2
                catch
                    _:E:Stacktrace
                      when is_atom(E) ->
                        foo
                after
                    bar
                end"},
            indoc::indoc! {"
                try
                    foo()
                catch
                    _:#foo{} = E ->
                        E
                end"},
            indoc::indoc! {"
                try
                    foo()
                catch
                    throw:_ ->
                        throw;
                    error:_ ->
                        error
                end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn catch_works() {
        let texts = [
            "catch 1",
            indoc::indoc! {"
                catch foo(bar,
                          Baz,
                          qux) +
                      3 + 4"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
