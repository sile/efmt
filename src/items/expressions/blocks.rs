use crate::format::{self, Format};
use crate::format2::{Format2, Formatter2, Indent, Newline};
use crate::items::expressions::{Body, Expr};
use crate::items::generics::{Clauses, Either, Maybe, NonEmptyItems, WithArrow, WithGuard};
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, EndKeyword, IfKeyword, OfKeyword,
    ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::{ColonSymbol, CommaSymbol, SemicolonSymbol};
use crate::items::tokens::{AtomToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
struct End(EndKeyword);

impl Format2 for End {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::Inherit, Newline::Always, |fmt| {
            self.0.format2(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
    end: End,
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

impl Format2 for CaseExpr {
    fn format2(&self, fmt: &mut Formatter2) {
        self.case.format2(fmt);
        fmt.add_space();
        self.value.format2(fmt);
        fmt.add_space();
        self.of.format2(fmt);
        self.clauses.format2(fmt);
        self.end.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct CaseClause {
    pattern: WithArrow<WithGuard<Expr, Expr>>,
    body: Body,
}

/// `if` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$GUARD` `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Block<Clauses<IfClause>>,
    end: End,
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

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct IfClause {
    condigion: WithArrow<GuardCondition>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct GuardCondition(NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>);

/// `begin` `$BODY` `end`
///
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: Body,
    end: End,
}

/// `receive` (`$CLAUSE` `;`?)* `$TIMEOUT`? `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` `$BODY`
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $TIMEOUT: `after` [Expr] `->` `$BODY`
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Block<Maybe<Clauses<CaseClause>>>,
    timeout: Maybe<ReceiveTimeout>,
    end: End,
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

impl Format2 for ReceiveTimeout {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::Inherit, Newline::Always, |fmt| {
            self.after.format2(fmt);
            self.clause.format2(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
    end: End,
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

impl Format2 for TryExpr {
    fn format2(&self, fmt: &mut Formatter2) {
        self.r#try.format2(fmt);
        self.body.format2(fmt);
        fmt.add_newline();
        self.clauses.format2(fmt);
        fmt.subregion(Indent::Inherit, Newline::Always, |fmt| {
            self.catch.format2(fmt);
        });
        fmt.subregion(Indent::Inherit, Newline::Always, |fmt| {
            self.after.format2(fmt);
        });
        self.end.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct TryCatch {
    catch: CatchKeyword,
    clauses: Block<Clauses<CatchClause>>,
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct CatchClause {
    pattern: WithArrow<WithGuard<CatchPattern, Expr>>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
struct CatchPattern {
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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

impl Format2 for CatchExpr {
    fn format2(&self, fmt: &mut Formatter2) {
        self.catch.format2(fmt);
        fmt.add_space();
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.expr.format2(fmt);
        });
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

impl<T: Format2> Format2 for Block<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::Offset(4), Newline::Always, |fmt| {
            self.0.format2(fmt);
            fmt.add_newline();
        });
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
            crate::assert_format2!(text, Expr);
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
            crate::assert_format2!(text, Expr);
        }
    }

    #[test]
    fn receive_works() {
        let texts = [
            indoc::indoc! {"
            %---10---|%---20---|
            receive
                {A, B} ->
                    [A, B];
                A when is_integer(A);
                       is_atom(A) ->
                    A
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            receive
                A ->
                    A
            after
                1000 ->
                    timeout
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            receive
            after
                N ->
                    timeout
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            receive
            after
                infinity ->
                    timeout
            end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
            crate::assert_format2!(text, Expr);
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
            %---10---|%---20---|
            begin
                foo(bar, Baz),
                {[#{}]}
            end"},
        ];
        for text in texts {
            //crate::assert_format!(text, Expr);
            crate::assert_format2!(text, Expr);
        }
    }

    #[test]
    fn try_works() {
        let texts = [
            indoc::indoc! {"
            %---10---|%---20---|
            try
                1
            after
                2
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            try
                1,
                2,
                3
            catch
                E ->
                    E
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
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
            %---10---|%---20---|
            try
                foo()
            catch
                _:#foo{} = E ->
                    E
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
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
            crate::assert_format2!(text, Expr);
        }
    }

    #[test]
    fn catch_works() {
        let texts = [
            "catch 1",
            indoc::indoc! {"
            %---10---|%---20---|
            catch foo(bar,
                      Baz,
                      qux) + 3 +
                  4"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
            crate::assert_format2!(text, Expr);
        }
    }
}
