use crate::format::{Format, Formatter, Indent, Newline};
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
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct DefinedFunctionExpr {
    fun: Fun,
    module: Maybe<(BaseExpr, ColonSymbol)>,
    name: BaseExpr,
    slash: SlashSymbol,
    arity: BaseExpr,
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct AnonymousFunctionExpr {
    fun: Fun,
    clauses_and_end: FunctionClausesAndEnd<Null>,
}

/// `fun` (`$CLAUSE` `;`?)+ `end`
///
/// - $CLAUSE: [VariableToken] `(` ([Expr] `,`?)* `)` (when `$GUARD`)? `->` `$BODY`
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct NamedFunctionExpr {
    fun: Fun,
    clauses_and_end: FunctionClausesAndEnd<VariableToken>,
}

#[derive(Debug, Clone, Span, Parse)]
struct Fun(FunKeyword);

impl Format for Fun {
    fn format(&self, fmt: &mut Formatter) {
        self.0.format(fmt);
        fmt.add_space();
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct FunctionClausesAndEnd<Name> {
    clauses: Clauses<FunctionClause<Name>>,
    end: EndKeyword,
}

impl<Name: Format> Format for FunctionClausesAndEnd<Name> {
    fn format(&self, fmt: &mut Formatter) {
        if self.clauses.items().len() == 1 && self.clauses.items()[0].body().exprs().len() == 1 {
            let clause = &self.clauses.items()[0];
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                clause.format_maybe_one_line_body(fmt);
                fmt.add_space();
                fmt.subregion(
                    Indent::ParentOffset(0),
                    Newline::IfTooLongOrMultiLineParent,
                    |fmt| self.end.format(fmt),
                );
            });
        } else {
            self.clauses.format(fmt);
            fmt.add_newline();
            self.end.format(fmt);
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
            indoc::indoc! {"
            %---10---|%---20---|
            fun () -> foo() end"},
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
