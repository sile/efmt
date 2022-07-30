use crate::format::{Format, Formatter};
use crate::items::components::{Clauses, Either, Guard, Maybe, NonEmptyItems};
use crate::items::expressions::components::Body;
use crate::items::keywords::{
    AfterKeyword, BeginKeyword, CaseKeyword, CatchKeyword, ElseKeyword, EndKeyword, IfKeyword,
    MaybeKeyword, OfKeyword, ReceiveKeyword, TryKeyword,
};
use crate::items::symbols::{ColonSymbol, CommaSymbol, RightArrowSymbol, SemicolonSymbol};
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
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'case'
                fmt.set_indent(fmt.column());
                self.case.format(fmt);

                fmt.with_scoped_indent(|fmt| {
                    fmt.write_space();
                    fmt.set_indent(fmt.column());

                    // 'Expr'
                    self.value.format(fmt);
                    fmt.write_space();
                });

                // 'of'
                self.of.format(fmt);
                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();

                    // 'Clauses'
                    self.clauses.format(fmt);
                });

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct CaseClause {
    pattern: Expr,
    guard: Maybe<Guard<Expr>>,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Format for CaseClause {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            let base_ident = fmt.indent();

            // 'Pattern'
            self.pattern.format(fmt);

            // 'when'
            if let Some(guard) = self.guard.get() {
                if fmt.has_newline_until(guard) {
                    fmt.set_indent(base_ident + 2);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                guard.format(fmt);
            }

            // '->'
            let multiline = fmt.has_newline_until(&self.body.end_position());
            fmt.write_space();
            self.arrow.format(fmt);

            // 'Body'
            if multiline {
                fmt.set_indent(base_ident + 4);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }
            self.body.format(fmt);
        });
    }
}

/// `if` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `$GUARD` `->` [Body]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct IfExpr {
    r#if: IfKeyword,
    clauses: Clauses<IfClause>,
    end: EndKeyword,
}

impl Format for IfExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'if'
                fmt.set_indent(fmt.column());
                self.r#if.format(fmt);

                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();

                    // 'Clauses'
                    self.clauses.format(fmt);
                });

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct IfClause {
    condition: GuardCondition,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Format for IfClause {
    fn format(&self, fmt: &mut Formatter) {
        // 'Condition'
        self.condition.format(fmt);
        fmt.write_space();

        // '->'
        let multiline = fmt.has_newline_until(&self.body.end_position());
        self.arrow.format(fmt);

        // 'Body'
        if multiline {
            fmt.with_scoped_indent(|fmt| {
                fmt.set_indent(fmt.indent() + 4);
                fmt.write_newline();
                self.body.format(fmt);
            });
        } else {
            fmt.write_space();
            self.body.format(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct GuardCondition(NonEmptyItems<Expr, Either<CommaSymbol, SemicolonSymbol>>);

/// `begin` [Body] `end`
///
#[derive(Debug, Clone, Span, Parse)]
pub struct BeginExpr {
    begin: BeginKeyword,
    exprs: Body,
    end: EndKeyword,
}

impl Format for BeginExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'begin'
                fmt.set_indent(fmt.column());
                self.begin.format(fmt);

                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();

                    // 'Body'
                    self.exprs.format(fmt);
                });

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

/// `receive` (`$CLAUSE` `;`?)* `$TIMEOUT`? `end`
///
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $TIMEOUT: `after` [Expr] `->` [Body]
#[derive(Debug, Clone, Span, Parse)]
pub struct ReceiveExpr {
    receive: ReceiveKeyword,
    clauses: Maybe<Clauses<CaseClause>>,
    timeout: Maybe<ReceiveTimeout>,
    end: EndKeyword,
}

impl Format for ReceiveExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'receive'
                fmt.set_indent(fmt.column());
                self.receive.format(fmt);

                if self.clauses.get().is_some() {
                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();

                        // 'Clauses'
                        self.clauses.format(fmt);
                    });
                }

                // 'after'
                if self.timeout.get().is_some() {
                    fmt.write_newline();
                    self.timeout.format(fmt);
                }

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct ReceiveTimeout {
    after: AfterKeyword,
    timeout: Expr,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Format for ReceiveTimeout {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            // 'after'
            self.after.format(fmt);

            // 'Timeout' '->'
            fmt.set_indent(fmt.indent() + 4);
            fmt.write_newline();
            self.timeout.format(fmt);
            fmt.write_space();
            self.arrow.format(fmt);

            // 'Body'
            fmt.set_indent(fmt.indent() + 4);
            fmt.write_newline();
            self.body.format(fmt);
        });
    }
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
    clauses: Maybe<(OfKeyword, Clauses<CaseClause>)>,
    catch: Maybe<TryCatch>,
    after: Maybe<TryAfter>,
    end: EndKeyword,
}

impl Format for TryExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'try'
                fmt.set_indent(fmt.column());
                self.r#try.format(fmt);

                // 'Body'
                if self.clauses.get().is_some()
                    && (self.body.exprs().len() == 1 || !fmt.has_newline_until(&self.clauses))
                {
                    fmt.with_scoped_indent(|fmt| {
                        fmt.write_space();
                        fmt.set_indent(fmt.column());
                        self.body.format(fmt);
                        fmt.write_space();
                    });
                } else {
                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();
                        self.body.format(fmt);
                    });
                    if self.clauses.get().is_some() {
                        fmt.write_newline();
                    }
                }

                if let Some((of, clauses)) = self.clauses.get() {
                    // 'of'
                    of.format(fmt);

                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();

                        // 'Clauses'
                        clauses.format(fmt);
                    });
                }

                // 'catch'
                if let Some(catch) = self.catch.get() {
                    fmt.write_newline();
                    catch.catch.format(fmt);

                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();
                        catch.clauses.format(fmt);
                    });
                }

                // 'after'
                if let Some(after) = self.after.get() {
                    fmt.write_newline();
                    after.after.format(fmt);
                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();
                        after.body.format(fmt);
                    });
                }

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct TryCatch {
    catch: CatchKeyword,
    clauses: Clauses<CatchClause>,
}

#[derive(Debug, Clone, Span, Parse)]
struct CatchClause {
    pattern: CatchPattern,
    guard: Maybe<Guard<Expr>>,
    arrow: RightArrowSymbol,
    body: Body,
}

impl Format for CatchClause {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            let base_indent = fmt.indent();

            // 'Class:Patter:Stacktrace'
            self.pattern.class.format(fmt);
            self.pattern.pattern.format(fmt);
            self.pattern.stacktrace.format(fmt);

            // 'when'
            if let Some(guard) = self.guard.get() {
                if fmt.has_newline_until(guard) {
                    fmt.set_indent(fmt.indent() + 2);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                guard.format(fmt);
            }

            // '->'
            fmt.write_space();
            self.arrow.format(fmt);

            // 'Body'
            if fmt.has_newline_until(&self.body) {
                fmt.set_indent(base_indent + 4);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }
            self.body.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct CatchPattern {
    class: Maybe<(Either<AtomToken, VariableToken>, ColonSymbol)>,
    pattern: Expr,
    stacktrace: Maybe<(ColonSymbol, VariableToken)>,
}

#[derive(Debug, Clone, Span, Parse)]
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
        // 'catch'
        self.catch.format(fmt);

        // 'Expr'
        fmt.write_space();
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.expr.format(fmt);
        });
    }
}

/// `Maybe` [Body] `$ELSE`? `end`
///
/// - $ELSE: `else` (`$CLAUSE` `;`?)+
/// - $CLAUSE: `$PATTERN` (`when` `$GUARD`)? `->` [Body]
/// - $PATTERN: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct MaybeExpr {
    maybe: MaybeKeyword,
    body: Body,
    else_block: Maybe<ElseBlock>,
    end: EndKeyword,
}

impl Format for MaybeExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'maybe'
                fmt.set_indent(fmt.column());
                self.maybe.format(fmt);

                // 'Body'
                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();
                    self.body.format(fmt);
                });

                // 'else'
                if let Some(else_block) = self.else_block.get() {
                    fmt.write_newline();
                    else_block.else_keyword.format(fmt);

                    // 'Clauses'
                    fmt.with_scoped_indent(|fmt| {
                        fmt.set_indent(fmt.indent() + 4);
                        fmt.write_newline();
                        else_block.clauses.format(fmt);
                    });
                }

                // 'end'
                fmt.write_newline();
                self.end.format(fmt);
            })
        };
        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct ElseBlock {
    else_keyword: ElseKeyword,
    clauses: Clauses<CaseClause>,
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
            case Foo of 1 -> 2 end"},
            indoc::indoc! {"
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
            if true -> 2 end"},
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
            receive A -> A after 1000 -> timeout end"},
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
            indoc::indoc! {"
            begin foo(bar, Baz), {[#{}]} end"},
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
            try 1, 2, 3 catch E -> E end"},
            indoc::indoc! {"
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
            try foo() of
                1 ->
                    2
            catch
                C:_ when is_integer(C);
                         is_atom(C) ->
                    bar;
                _:_ ->
                    foo
            end"},
            indoc::indoc! {"
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
                      qux) + 3 + 4"},
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
