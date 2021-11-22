use crate::format::{self, Format};
use crate::format2::{Format2, Formatter2, Indent, Newline, NewlineIf};
use crate::items::expressions::{AtomLikeExpr, Body, Expr, IntegerLikeExpr};
use crate::items::generics::{Clauses, Maybe, Null, Params, WithArrow, WithGuard};
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::symbols::{ColonSymbol, SlashSymbol};
use crate::items::tokens::VariableToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct AnonymousFunctionExpr {
    fun: Fun,
    clauses: FunctionClauses<Null>,
    end: EndKeyword,
}

impl Format2 for AnonymousFunctionExpr {
    fn format2(&self, fmt: &mut Formatter2) {
        // TODO: refactor
        if self.clauses.0.items().len() == 1 && self.clauses.0.items()[0].body.exprs().len() == 1 {
            let clause = self.clauses.0.items()[0].clone();
            let clause = FunctionClause {
                name: clause.name,
                params: clause.params,
                body: MaybeOnelineBody(clause.body),
            };
            self.fun.format2(fmt);
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                clause.format2(fmt);
                fmt.subregion(
                    Indent::ParentOffset(0),
                    Newline::If(NewlineIf {
                        too_long: true,
                        multi_line_parent: true,
                        ..Default::default()
                    }),
                    |fmt| {
                        self.end.format2(fmt);
                    },
                );
            });
        } else {
            self.fun.format2(fmt);
            self.clauses.format2(fmt);
            fmt.add_newline();
            self.end.format2(fmt);
        }
    }
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: [VariableToken] `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionExpr {
    fun: Fun,
    clauses: FunctionClauses<VariableToken>,
    end: EndKeyword,
}

impl Format2 for NamedFunctionExpr {
    fn format2(&self, fmt: &mut Formatter2) {
        if self.clauses.0.items().len() == 1 && self.clauses.0.items()[0].body.exprs().len() == 1 {
            let clause = self.clauses.0.items()[0].clone();
            let clause = FunctionClause {
                name: clause.name,
                params: clause.params,
                body: MaybeOnelineBody(clause.body),
            };
            self.fun.format2(fmt);
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                clause.format2(fmt);
                fmt.subregion(
                    Indent::ParentOffset(0),
                    Newline::If(NewlineIf {
                        too_long: true,
                        multi_line_parent: true,
                        ..Default::default()
                    }),
                    |fmt| {
                        self.end.format2(fmt);
                    },
                );
            });
        } else {
            self.fun.format2(fmt);
            self.clauses.format2(fmt);
            fmt.add_newline();
            self.end.format2(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Fun(FunKeyword);

impl Format2 for Fun {
    fn format2(&self, fmt: &mut Formatter2) {
        self.0.format2(fmt);
        fmt.add_space();
    }
}

impl Format for Fun {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.0.format(fmt)?;
        fmt.write_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
struct FunctionClauses<Name>(Clauses<FunctionClause<Name>>);

impl<Name: Format> Format for FunctionClauses<Name> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.0.items().len() == 1 && self.0.items()[0].body.exprs().len() == 1 {
            let ok = fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .trailing_columns(4) // "end"
                .check_trailing_columns(true)
                .enter(|fmt| {
                    let clause = &self.0.items()[0];
                    clause.name.format(fmt)?;
                    clause.params.format(fmt)?;
                    fmt.write_space()?;
                    clause.body.exprs()[0].format(fmt)?;
                    fmt.write_space()?;
                    Ok(())
                })
                .is_ok();
            if ok {
                return Ok(());
            }
        }

        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.0.format(fmt)?;
            fmt.write_newline()?;
            Ok(())
        })
    }
}

// TODO
// impl<Name: Format2 + Clone> Format2 for FunctionClauses<Name> {
//     fn format2(&self, fmt: &mut Formatter2) {
//         if self.0.items().len() == 1 && self.0.items()[0].body.exprs().len() == 1 {
//             self.0
//                 .clone()
//                 .map(|clause| FunctionClause {
//                     name: clause.name,
//                     params: clause.params,
//                     body: MaybeOnelineBody(clause.body),
//                 })
//                 .format2(fmt);
//         } else {
//             self.0.format2(fmt);
//             //fmt.add_newline(self.0.start_position());
//         }
//         // TODO: remove
//         // fmt.subregion(
//         //     Indent::Inherit,
//         //     Newline::If(NewlineIf {
//         //         multi_line_parent: true,
//         //         ..Default::default()
//         //     }),
//         //     |_| {},
//         // );
//     }
// }

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub(crate) struct FunctionClause<Name, B = Body> {
    name: Name,
    params: WithArrow<WithGuard<Params<Expr>, Expr>>,
    body: B,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct MaybeOnelineBody(Body);

impl Format2 for MaybeOnelineBody {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(
            Indent::Offset(4),
            Newline::If(NewlineIf {
                too_long: true,
                multi_line_parent: true,
                ..Default::default()
            }),
            |fmt| self.0.exprs.format2(fmt),
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
            crate::assert_format2!(text, Expr);
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
            crate::assert_format2!(text, Expr);
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
            //crate::assert_format!(text, Expr);
            crate::assert_format2!(text, Expr);
        }
    }
}
