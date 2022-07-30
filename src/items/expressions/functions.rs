use crate::format::{Format, Formatter};
use crate::items::components::{Clauses, Maybe, Null};
use crate::items::expressions::components::FunctionClause;
use crate::items::expressions::BaseExpr;
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::symbols::{ColonSymbol, SlashSymbol};
use crate::items::tokens::VariableToken;
#[cfg(doc)]
use crate::items::Expr;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionExpr {
    Defined(Box<DefinedFunctionExpr>),
    Anonymous(Box<AnonymousFunctionExpr>),
    Named(Box<NamedFunctionExpr>),
}

/// `fun` (`$MODULE` `:`)? `$NAME` `/` `$ARITY`
///
/// - $MODULE: [Expr]
/// - $NAME: [Expr]
/// - $ARITY: [Expr]
#[derive(Debug, Clone, Span, Parse)]
pub struct DefinedFunctionExpr {
    fun: FunKeyword,
    module: Maybe<(BaseExpr, ColonSymbol)>,
    name: BaseExpr,
    slash: SlashSymbol,
    arity: BaseExpr,
}

impl Format for DefinedFunctionExpr {
    fn format(&self, fmt: &mut Formatter) {
        self.fun.format(fmt);
        fmt.write_space();
        self.module.format(fmt);
        self.name.format(fmt);
        self.slash.format(fmt);
        self.arity.format(fmt);
    }
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse)]
pub struct AnonymousFunctionExpr {
    fun: FunKeyword,
    clauses: Clauses<FunctionClause<Null, 5>>,
    end: EndKeyword,
}

impl Format for AnonymousFunctionExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'fun'
                fmt.set_indent(fmt.column());
                self.fun.format(fmt);

                // 'Clauses'
                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.column());
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

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: [VariableToken] `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse)]
pub struct NamedFunctionExpr {
    fun: FunKeyword,
    clauses: Clauses<FunctionClause<VariableToken>>,
    end: EndKeyword,
}

impl Format for NamedFunctionExpr {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                // 'fun'
                fmt.set_indent(fmt.column());
                self.fun.format(fmt);

                // 'Clauses'
                fmt.write_space();
                fmt.with_scoped_indent(|fmt| {
                    fmt.set_indent(fmt.column());
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

#[cfg(test)]
mod tests {
    use crate::items::expressions::Expr;

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
            "fun() -> hi end",
            indoc::indoc! {"
            fun(a) ->
                    a;
               (A) ->
                    A
            end"},
            indoc::indoc! {"
            fun({a, b}, C) ->
                    C;
               (A, B)
                  when is_integer(A);
                       is_atom(B) ->
                    A
            end"},
            indoc::indoc! {"
            fun(A) ->
                    foo(),
                    bar,
                    baz(A)
            end"},
            indoc::indoc! {"
            fun(a) ->
                    foo(),
                    bar,
                    baz();
               (A) ->
                    A
            end"},
            indoc::indoc! {"
            fun() -> foo() end"},
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
            fun Foo() ->
                    hello
            end"},
            indoc::indoc! {"
            fun Foo(a) ->
                    a;
                Foo(A) ->
                    A
            end"},
            indoc::indoc! {"
            fun Foo({a, b},
                    C) ->
                    C;
                Foo(A, _B)
                  when is_integer(A) ->
                    A
            end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
