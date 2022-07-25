use crate::format::{Format, Formatter, Indent};
use crate::items::components::{Clauses, Either, Maybe, NonEmptyItems, WithArrow, WithGuard};
use crate::items::expressions::components::Body;
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, ElseKeyword, EndKeyword, IfKeyword,
    MaybeKeyword, OfKeyword, ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::{ColonSymbol, CommaSymbol, SemicolonSymbol};
use crate::items::tokens::{AtomToken, VariableToken};
use crate::items::Expr;
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
    Maybe(Box<MaybeExpr>),
}

/// `case` [Expr] `of` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct CaseExpr {
    case: CaseKeyword,
    value: Expr,
    of: OfKeyword,
    clauses: Clauses<CaseClause>,
    end: EndKeyword,
}

impl Format for CaseExpr {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.case.format(fmt);

            fmt.with_scoped_indent(|fmt| {
                fmt.write_space();
                fmt.set_indent(fmt.column());

                self.value.format(fmt);
                fmt.write_space();
                fmt.set_indent(fmt.column());

                self.of.format(fmt);
                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();
                    self.clauses.format(fmt);
                });
            });

            fmt.write_newline();
            self.end.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CaseClause {
    pattern: WithArrow<WithGuard<Expr, Expr>>,
    body: Body,
}

/// `if` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$GUARD` `->` [Body]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Block<Clauses<IfClause>>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct IfClause {
    condigion: WithArrow<GuardCondition>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct GuardCondition(NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>);

/// `begin` [Body] `end`
///
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: Body,
    end: EndKeyword,
}

/// `receive` (`$CLAUSE` `;`?)* `$TIMEOUT`? `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $TIMEOUT: `after` [Expr] `->` [Body]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Block<Maybe<Clauses<CaseClause>>>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse)]
struct ReceiveTimeout {
    after: AfterKeyword,
    clause: Block<ReceiveTimeoutClause>,
}

impl Format for ReceiveTimeout {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::inherit(), |fmt| {
            self.after.format(fmt);
            self.clause.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ReceiveTimeoutClause {
    timeout: WithArrow<Expr>,
    body: Body,
}

/// `try` [Body] `$BRANCHES`? `$CATCH`? `$AFTER`? `end`
///
/// - $BRANCHES: (`of` (`$BRANCH_CLAUSE` `;`?)+)
/// - $BRANCH_CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $CATCH: `catch` (`$CATCH_CLAUSE` `;`?)+
/// - $CATCH_CLAUSE: `$ERROR_CLASS`? [Expr] `$STACKTRACE`? (`when` `$GUARD`)? `->` [Body]
/// - $ERROR_CLASS: ([AtomToken] | [VariableToken]) `:`
/// - $STACKTRACE: `:` [VariableToken]
/// - $AFTER: `after` [Body]
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
    fn format(&self, fmt: &mut Formatter) {
        self.r#try.format(fmt);
        if self.clauses.get().is_some() && self.body.exprs().len() == 1 {
            self.body.exprs()[0].format(fmt);
            fmt.add_space();
        } else {
            self.body.format(fmt);
            fmt.add_newline();
        }
        self.clauses.format(fmt);
        fmt.subregion(Indent::inherit(), |fmt| self.catch.format(fmt));
        fmt.subregion(Indent::inherit(), |fmt| self.after.format(fmt));
        self.end.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct TryCatch {
    catch: CatchKeyword,
    clauses: Block<Clauses<CatchClause>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct CatchClause {
    pattern: WithArrow<WithGuard<CatchPattern, Expr>>,
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
    fn format(&self, fmt: &mut Formatter) {
        self.catch.format(fmt);
        fmt.add_space();
        fmt.subregion(Indent::CurrentColumn, |fmt| self.expr.format(fmt));
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Block<T>(T);

impl<T: Format> Format for Block<T> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::Offset(4), |fmt| {
            self.0.format(fmt);
            fmt.add_newline();
        });
    }
}

/// `maybe` [Body] `$ELSE`? `end`
///
/// - $ELSE: `else` (`$CLAUSE` `;`?)+
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MaybeExpr {
    maybe: MaybeKeyword,
    body: Body,
    else_block: Maybe<ElseBlock>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse)]
struct ElseBlock {
    else_keyword: ElseKeyword,
    clauses: Block<Clauses<CaseClause>>,
}

impl Format for ElseBlock {
    fn format(&self, fmt: &mut Formatter) {
        fmt.add_newline();
        self.else_keyword.format(fmt);
        self.clauses.format(fmt);
    }
}

#[cfg(test)]
mod tests {
    use crate::items::Expr;

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
                       A > 100 ->
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
            crate::assert_format!(text, Expr);
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
            try X of
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
            try foo() of
                1 ->
                    2
            catch
                _:_ ->
                    foo
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            try
                X,
                Y
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
        }
    }

    #[test]
    fn maybe_works() {
        let texts = [
            indoc::indoc! {"
            maybe
                1
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            maybe
                ok ?= foo(bar,
                          Baz),
                {[#{}]}
            else
                foo ->
                    bar;
                {error, R}
                  when is_atom(R) ->
                    error
            end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
