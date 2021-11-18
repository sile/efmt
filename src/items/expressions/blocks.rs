use crate::format::{self, Format};
use crate::items::expressions::{Body, Expr, GuardCondition, WithArrow, WithGuard};
use crate::items::generics::{Clauses, Either, Maybe, NonEmptyItems};
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, EndKeyword, IfKeyword, OfKeyword,
    ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::ColonSymbol;
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
#[derive(Debug, Clone, Span, Parse)]
pub struct CaseExpr {
    case: CaseKeyword,
    value: Expr,
    of: OfKeyword,
    clauses: Block<Clauses<CaseClause>>,
    end: EndKeyword,
}

impl Format for CaseExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().clear_trailing_columns(true).enter(|fmt| {
            self.case.format(fmt)?;
            fmt.write_space()?;

            fmt.subregion()
                .reset_trailing_columns(3) // " of"
                .enter(|fmt| self.value.format(fmt))?;
            fmt.write_space()?;

            self.of.format(fmt)?;
            self.clauses.format(fmt)?;
            self.end.format(fmt)?;

            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CaseClause {
    pattern: WithArrow<WithGuard<Expr>>,
    body: Body,
}

/// `if` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$GUARD` `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Block<Clauses<IfClause>>,
    end: EndKeyword,
}

impl Format for IfExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().clear_trailing_columns(true).enter(|fmt| {
            self.r#if.format(fmt)?;
            self.clauses.format(fmt)?;
            self.end.format(fmt)?;
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct IfClause {
    condigion: WithArrow<GuardCondition>,
    body: Body,
}

/// `begin` `$BODY` `end`
///
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: Block<NonEmptyItems<Expr>>,
    end: EndKeyword,
}

/// `receive` (`$CLAUSE` `;`?)* `$TIMEOUT`? `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` `$BODY`
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $TIMEOUT: `after` [Expr] `->` `$BODY`
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Block<Maybe<Clauses<CaseClause>>>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

impl Format for ReceiveExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().clear_trailing_columns(true).enter(|fmt| {
            self.receive.format(fmt)?;
            self.clauses.format(fmt)?;
            self.timeout.format(fmt)?;
            self.end.format(fmt)?;
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ReceiveTimeout {
    after: AfterKeyword,
    clause: Block<ReceiveTimeoutClause>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ReceiveTimeoutClause {
    timeout: WithArrow<Expr>,
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
#[derive(Debug, Clone, Span, Parse)]
pub struct TryExpr {
    r#try: TryKeyword,
    body: Body,
    clauses: Maybe<(OfKeyword, Block<Clauses<CaseClause>>)>,
    catch: Maybe<TryCatch>,
    after: Maybe<TryAfter>,
    end: EndKeyword,
}

impl Format for TryExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().clear_trailing_columns(true).enter(|fmt| {
            self.r#try.format(fmt)?;
            self.body.format(fmt)?;
            fmt.write_newline()?;
            self.clauses.format(fmt)?;
            self.catch.format(fmt)?;
            self.after.format(fmt)?;
            fmt.write_newline()?;
            self.end.format(fmt)?;
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct TryCatch {
    catch: CatchKeyword,
    clauses: Block<Clauses<CatchClause>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CatchClause {
    pattern: WithArrow<WithGuard<CatchPattern>>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CatchPattern {
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct TryAfter {
    after: AfterKeyword,
    body: Body,
}

/// `catch` [Expr]
#[derive(Debug, Clone, Span, Parse)]
pub struct CatchExpr {
    catch: CatchKeyword,
    expr: Expr,
}

impl Format for CatchExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.catch.format(fmt)?;
        fmt.write_space()?;
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.expr.format(fmt))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Block<T>(T);

impl<T: Format> Format for Block<T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion()
            .clear_trailing_columns(true)
            .indent_offset(4)
            .enter(|fmt| {
                fmt.write_newline()?;
                self.0.format(fmt)?;
                fmt.write_newline()?;
                Ok(())
            })
    }
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
            %---10---|%---20---|
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
