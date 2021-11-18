use crate::format::{self, Format};
use crate::items::expressions::{AtomLikeExpr, Body, Expr, Guard, IntegerLikeExpr};
use crate::items::generics::{Maybe, NonEmptyItems2, Null, Params};
use crate::items::keywords::{EndKeyword, FunKeyword};
use crate::items::styles::{Newline, RightSpace, Space};
use crate::items::symbols::{ColonSymbol, RightArrowSymbol, SemicolonSymbol, SlashSymbol};
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
    fun: RightSpace<FunKeyword>,
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
    fun: RightSpace<FunKeyword>,
    clauses: FunctionClauses<Null>,
    end: EndKeyword,
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: [VariableToken] `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionExpr {
    fun: RightSpace<FunKeyword>,
    clauses: FunctionClauses<VariableToken>,
    end: EndKeyword,
}

#[derive(Debug, Clone, Span, Parse)]
struct FunctionClauses<Name>(NonEmptyItems2<FunctionClause<Name>, Newline<SemicolonSymbol>>);

impl<Name: Format> Format for FunctionClauses<Name> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.0.items().len() == 1 && self.0.items()[0].body.exprs().len() == 1 {
            let ok = fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .trailing_columns2(4) // "end"
                .check_trailing_columns(true)
                .enter(|fmt| {
                    let clause = &self.0.items()[0];
                    clause.name.format(fmt)?;
                    clause.params_and_guard.format(fmt)?;
                    clause.arrow.format(fmt)?;
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

#[derive(Debug, Clone, Span, Parse, Format)]
pub(crate) struct FunctionClause<Name> {
    name: Name,
    params_and_guard: ParamsAndGuard,
    arrow: Space<RightArrowSymbol>,
    body: Body,
}

#[derive(Debug, Clone, Span, Parse)]
struct ParamsAndGuard {
    params: Params<Expr>,
    guard: Maybe<Guard>,
}

impl Format for ParamsAndGuard {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.guard.get().is_none() {
            fmt.subregion()
                .trailing_columns2(3) // " ->"
                .enter(|fmt| self.params.format(fmt))?;
        } else {
            self.params.format(fmt)?;
            fmt.subregion()
                .trailing_columns2(3) // " ->"
                .enter(|fmt| self.guard.format(fmt))?;
        }
        Ok(())
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
