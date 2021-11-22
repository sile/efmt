use crate::format::{Format, Formatter, Indent, Newline, NewlineIf};
use crate::items::expressions::{AtomLikeExpr, Body, Expr, IntegerLikeExpr};
use crate::items::generics::{Clauses, Maybe, Null, Params, WithArrow, WithGuard};
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::symbols::{ColonSymbol, SlashSymbol};
use crate::items::tokens::VariableToken;
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
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct DefinedFunctionExpr {
    fun: Fun,
    module: Maybe<(AtomLikeExpr, ColonSymbol)>,
    name: AtomLikeExpr,
    slash: SlashSymbol,
    arity: IntegerLikeExpr,
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse)]
pub struct AnonymousFunctionExpr {
    fun: Fun,
    clauses: FunctionClauses<Null>,
    end: EndKeyword,
}

impl Format for AnonymousFunctionExpr {
    fn format(&self, fmt: &mut Formatter) {
        // TODO: refactor
        if self.clauses.0.items().len() == 1 && self.clauses.0.items()[0].body.exprs().len() == 1 {
            let clause = self.clauses.0.items()[0].clone();
            let clause = FunctionClause {
                name: clause.name,
                params: clause.params,
                body: MaybeOnelineBody(clause.body),
            };
            self.fun.format(fmt);
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                clause.format(fmt);
                fmt.subregion(
                    Indent::ParentOffset(0),
                    Newline::If(NewlineIf {
                        too_long: true,
                        multi_line_parent: true,
                        ..Default::default()
                    }),
                    |fmt| {
                        self.end.format(fmt);
                    },
                );
            });
        } else {
            self.fun.format(fmt);
            self.clauses.format(fmt);
            fmt.add_newline();
            self.end.format(fmt);
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
    fun: Fun,
    clauses: FunctionClauses<VariableToken>,
    end: EndKeyword,
}

impl Format for NamedFunctionExpr {
    fn format(&self, fmt: &mut Formatter) {
        if self.clauses.0.items().len() == 1 && self.clauses.0.items()[0].body.exprs().len() == 1 {
            let clause = self.clauses.0.items()[0].clone();
            let clause = FunctionClause {
                name: clause.name,
                params: clause.params,
                body: MaybeOnelineBody(clause.body),
            };
            self.fun.format(fmt);
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                clause.format(fmt);
                fmt.subregion(
                    Indent::ParentOffset(0),
                    Newline::If(NewlineIf {
                        too_long: true,
                        multi_line_parent: true,
                        ..Default::default()
                    }),
                    |fmt| {
                        self.end.format(fmt);
                    },
                );
            });
        } else {
            self.fun.format(fmt);
            self.clauses.format(fmt);
            fmt.add_newline();
            self.end.format(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Fun(FunKeyword);

impl Format for Fun {
    fn format(&self, fmt: &mut Formatter) {
        self.0.format(fmt);
        fmt.add_space();
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct FunctionClauses<Name>(Clauses<FunctionClause<Name>>);

#[derive(Debug, Clone, Span, Parse, Format)]
pub(crate) struct FunctionClause<Name, B = Body> {
    name: Name,
    params: WithArrow<WithGuard<Params<Expr>, Expr>>,
    body: B,
}

#[derive(Debug, Clone, Span, Parse)]
struct MaybeOnelineBody(Body);

impl Format for MaybeOnelineBody {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(
            Indent::Offset(4),
            Newline::If(NewlineIf {
                too_long: true,
                multi_line_parent: true,
                ..Default::default()
            }),
            |fmt| self.0.exprs.format(fmt),
        );
    }
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
            %---10---|%---20---|
            fun (a) ->
                    a;
                (A) ->
                    A
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun ({a, b}, C) ->
                    C;
                (A, B)
                  when is_integer(A);
                       is_atom(B) ->
                    A
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun (A) ->
                    foo(),
                    bar,
                    baz(A)
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun (a) ->
                    foo(),
                    bar,
                    baz();
                (A) ->
                    A
            end"},
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
            %---10---|%---20---|
            fun Foo() ->
                    hello
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun Foo(a) ->
                    a;
                Foo(A) ->
                    A
            end"},
            indoc::indoc! {"
            %---10---|%---20---|
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
