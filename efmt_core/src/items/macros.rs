use super::components::Guard;
use crate::format::{Format, Formatter};
use crate::items::Expr;
use crate::items::components::{Args, Either, Maybe};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, OpenParenSymbol, QuestionSymbol,
};
use crate::items::tokens::{AtomToken, LexicalToken, StringToken, VariableToken};
use crate::parse::{self, Parse, ResumeParse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};
use std::collections::HashMap;

/// `?` `$NAME` (`(` (`$ARG` `,`?)* `)`)?
///
/// - $NAME: [AtomToken] | [VariableToken]
/// - $ARG: [LexicalToken]+
#[derive(Debug, Clone, Span, Parse)]
pub struct Macro {
    question: QuestionSymbol,
    name: MacroName,
    args: Maybe<Args<MacroArg>>,
}

impl ResumeParse<(QuestionSymbol, MacroName, bool)> for Macro {
    fn resume_parse(
        ts: &mut TokenStream,
        (question, name, has_args): (QuestionSymbol, MacroName, bool),
    ) -> parse::Result<Self> {
        if has_args {
            Ok(Self {
                question,
                name,
                args: Maybe::some(ts.parse()?),
            })
        } else {
            Ok(Self {
                question,
                name,
                args: Maybe::parse_none(ts)?,
            })
        }
    }
}

impl Format for Macro {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.question.format(fmt);
            self.name.format(fmt);
            self.args.format(fmt);
        });
    }
}

impl Macro {
    pub fn macro_name(&self) -> &MacroName {
        &self.name
    }

    pub fn arity(&self) -> Option<usize> {
        self.args.get().map(|x| x.get().len())
    }

    pub fn args(&self) -> impl Iterator<Item = &MacroArg> {
        self.args.get().into_iter().flat_map(|x| x.get().iter())
    }

    pub fn expand(
        &self,
        variables: Option<Vec<String>>,
        replacement: Vec<LexicalToken>,
    ) -> Vec<LexicalToken> {
        let args = if let (Some(vars), Some(vals)) = (&variables, self.args.get().map(|x| x.get()))
        {
            vars.iter()
                .map(|x| &x[..])
                .zip(vals.iter())
                .collect::<HashMap<_, _>>()
        } else {
            HashMap::new()
        };

        let mut do_stringify = false;
        let mut tokens = Vec::new();
        for token in replacement {
            match token {
                LexicalToken::Variable(_) if do_stringify => {
                    let dummy =
                        StringToken::new("EFMT_DUMMY", self.start_position(), self.end_position());
                    tokens.push(dummy.into());
                }
                LexicalToken::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].tokens().iter().cloned());
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::DoubleQuestion => {
                    do_stringify = true;
                    continue;
                }
                mut token => {
                    token.set_span(self);
                    tokens.push(token);
                }
            }
            do_stringify = false;
        }
        tokens
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MacroName(Either<AtomToken, VariableToken>);

impl MacroName {
    pub fn value(&self) -> &str {
        match &self.0 {
            Either::A(x) => x.value(),
            Either::B(x) => x.value(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MacroReplacement {
    tokens: Vec<LexicalToken>,
    start_position: Position,
}

impl MacroReplacement {
    pub fn tokens(&self) -> &[LexicalToken] {
        &self.tokens
    }
}

impl Span for MacroReplacement {
    fn start_position(&self) -> Position {
        self.tokens
            .first()
            .map_or(self.start_position, |token| token.start_position())
    }

    fn end_position(&self) -> Position {
        self.tokens
            .last()
            .map_or(self.start_position, |token| token.end_position())
    }
}

impl Parse for MacroReplacement {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        ts.with_macro_expand_disabled(|ts| {
            let start_position = ts.next_token_start_position()?;
            let mut tokens = Vec::new();
            while ts.peek::<(CloseParenSymbol, DotSymbol)>().is_none() {
                tokens.push(ts.parse()?);
            }
            Ok(Self {
                tokens,
                start_position,
            })
        })
    }
}

impl Format for MacroReplacement {
    fn format(&self, fmt: &mut Formatter) {
        if let Ok(expr) = fmt
            .token_stream_mut()
            .parse_tokens::<Expr>(self.tokens.clone())
            && expr.end_position() == self.end_position()
        {
            expr.format(fmt);
            return;
        }

        fmt.write_span(self);
    }
}

#[derive(Debug, Clone)]
pub struct MacroArg {
    tokens: Vec<LexicalToken>,
}

impl MacroArg {
    pub fn tokens(&self) -> &[LexicalToken] {
        &self.tokens
    }

    pub fn parse_expr(&self, ts: &mut TokenStream) -> Option<Expr> {
        ts.parse_tokens::<Expr>(self.tokens.clone()).ok()
    }
}

impl Span for MacroArg {
    fn start_position(&self) -> Position {
        self.tokens[0].start_position()
    }

    fn end_position(&self) -> Position {
        self.tokens[self.tokens.len() - 1].end_position()
    }
}

impl Parse for MacroArg {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        if ts.peek::<Either<CommaSymbol, CloseParenSymbol>>().is_some() {
            let token = ts.parse()?;
            return Err(parse::Error::unexpected_token(ts, token));
        }

        let mut tokens = Vec::new();
        let mut level = 0;
        while tokens.is_empty()
            || level > 0
            || ts.peek::<Either<CommaSymbol, CloseParenSymbol>>().is_none()
        {
            let token: LexicalToken = ts.parse()?;
            match &token {
                LexicalToken::Symbol(x) => match x.value() {
                    Symbol::OpenParen
                    | Symbol::OpenBrace
                    | Symbol::OpenSquare
                    | Symbol::DoubleLeftAngle => {
                        level += 1;
                    }
                    Symbol::CloseParen
                    | Symbol::CloseBrace
                    | Symbol::CloseSquare
                    | Symbol::DoubleRightAngle => {
                        if level == 0 {
                            return Err(parse::Error::unexpected_token(ts, token));
                        }
                        level -= 1;
                    }
                    _ => {}
                },
                LexicalToken::Keyword(x) => match x.value() {
                    Keyword::Begin
                    | Keyword::Try
                    | Keyword::Case
                    | Keyword::If
                    | Keyword::Receive => {
                        level += 1;
                    }
                    Keyword::Fun => {
                        if ts.peek::<OpenParenSymbol>().is_some()
                            || ts.peek::<(LexicalToken, OpenParenSymbol)>().is_some()
                        {
                            level += 1;
                        }
                    }
                    Keyword::End => {
                        if level == 0 {
                            return Err(parse::Error::unexpected_token(ts, token));
                        }
                        level -= 1;
                    }
                    _ => {}
                },
                _ => {}
            }
            tokens.push(token);
        }

        Ok(Self { tokens })
    }
}

impl Format for MacroArg {
    fn format(&self, fmt: &mut Formatter) {
        if let Ok(arg) = fmt
            .token_stream_mut()
            .parse_tokens::<(Expr, Maybe<Guard<Expr>>)>(self.tokens.clone())
            && arg.end_position() == self.end_position()
        {
            fmt.with_scoped_indent(|fmt| {
                arg.0.format(fmt);
                if let Some(guard) = arg.1.get() {
                    if fmt.has_newline_until(guard) {
                        fmt.set_indent(fmt.indent() + 2);
                        fmt.write_newline();
                    } else {
                        fmt.write_space();
                    }
                    guard.format(fmt);
                }
            });
            return;
        }

        fmt.write_span(self);
    }
}

#[cfg(test)]
mod tests {
    use crate::items::module::Module;

    #[test]
    fn macro_without_args_works() {
        let texts = [
            indoc::indoc! {"
            -define(FOO, bar).


            baz() ->
                ?FOO.
            "},
            indoc::indoc! {"
            -define(FOO, bar).


            baz() ->
                ?FOO().
            "},
            indoc::indoc! {"
            -define(FOO, bar).
            -define(FOO(),
                    bar bar).


            baz() ->
                ?FOO.
            "},
            indoc::indoc! {"
            baz() ->
                ?UNKNOWN.
            "},
            indoc::indoc! {"
            -define(INC, 1 +).


            inc(A) ->
                ?INC A.
            "},
            indoc::indoc! {"
            -define(FOO_OPEN,
                    foo().

            -define(FOO_CLOSE,
                    )).


            foo(A) ->
                ?FOO_OPEN A?FOO_CLOSE.
            "},
            indoc::indoc! {"
            -define(EMPTY, ).


            ?EMPTY hello?EMPTY() ->
                ?EMPTY?EMPTY world.
            ?EMPTY"},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_with_args_works() {
        let texts = [
            indoc::indoc! {"
            -define(FOO(), foo).


            foo() ->
                ?FOO().
            "},
            indoc::indoc! {"
            -define(FOO(Bar),
                    {Bar, Baz}).


            qux() ->
                ?FOO(quux).
            "},
            indoc::indoc! {"
            -define(FOO(Bar,
                        Baz),
                    {Bar, Baz}).


            qux() ->
                ?FOO(begin
                         foo,
                         bar,
                         baz
                     end,
                     hello).
            "},
            indoc::indoc! {"
            -define(FOO(A), A).


            qux() ->
                [?FOO(begin
                          foo,
                          bar,
                          baz
                      end),
                 1,
                 2].
            "},
            indoc::indoc! {"
            -define(foo, foo).
            -define(bar(A), A).
            -define(baz(A), A).


            main() ->
                ?baz(?bar(?foo)).
            "},
            indoc::indoc! {"
            -define(Foo(A),
                    ??A).


            bar() ->
                ?Foo(10).
            "},
            indoc::indoc! {"
            -define(FOO(), foo).
            -define(FOO,
                    foo foo).


            foo() ->
                ?FOO() + 10.
            "},
            indoc::indoc! {"
            -define(FOO,
                    begin
                        1,
                        2
                    end).


            foo() ->
                ?FOO.
            "},
            indoc::indoc! {"
            -define(FOO(X), X).
            -define(BAR(),
                    ?FOO(baz),).
            "},
            indoc::indoc! {"
            foo() ->
                ?FOO(
                  a, b, c).
            "},
            indoc::indoc! {"
            foo() ->
                ?FOO(
                  a,
                  b,
                  c).
            "},
            indoc::indoc! {"
            foo() ->
                ?FOO(a,
                     b,
                     c).
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn weird_macro_works() {
        let texts = [
            indoc::indoc! {"
            -define(foo, [],[).
            -define(bar(A), A).


            main() ->
                ?bar(?foo) c].
            "},
            indoc::indoc! {"
            -define(foo, [],).
            -define(bar(A), A).


            main() ->
                ?bar(?foo)
                [c].
            "},
            indoc::indoc! {"
            -define(a,
                    [1, 2, 3], [).
            -define(b(A), A).


            main() ->
                ?b(?a a),
                        c].
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_and_comment_works() {
        let texts = [
            indoc::indoc! {"
            -define(FOO(A), A).


            qux() ->
                ?FOO(\"aaa\"
                     \"bbb\"  % comment
                     \"ccc\").
            "},
            indoc::indoc! {"
            -define(FOO, foo).


            qux(A) ->
                case A of
                    a ->
                        ?FOO
                        %% comment
                end.
            "},
            indoc::indoc! {"
            -define(a(X), X X).


            foo() ->
                1 ?a(?a(+1)).
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_lazy_expand_works() {
        let texts = [
            indoc::indoc! {"
            -define(BAR,
                    ?FOO
                        1
                    end).

            -define(FOO, begin).


            baz() ->
                ?BAR.
            "},
            indoc::indoc! {"
            -define(FOO,
                    ?BAR(1)).

            -define(BAR(X), X).


            foo() ->
                [?FOO].
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn circular_macro_works() {
        let texts = [
            indoc::indoc! {"
            -define(a, ?b).
            -define(b, ?a).


            foo() ->
                ?a == ?b.
            "},
            indoc::indoc! {"
            -define(a, ?a).
            "},
            indoc::indoc! {"
            -define(a, ?a + ?a).
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_and_binary_op() {
        let texts = [indoc::indoc! {"
            -define(FOO,
                    case bar of
                        baz ->
                            ok
                    end).


            foo() ->
                ok = ?FOO.
            "}];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn assert_match() {
        let texts = [
            indoc::indoc! {"
            foo() ->
                ?assertMatch(ok when true, Value),
                ?assertNotMatch([A, B, C]
                                  when is_binary(B) andalso
                                       is_integer(C),
                                Value),
                ok.
            "},
            indoc::indoc! {"
            foo() -> fun() -> ?assertMatch(ok, Value) end.
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }
}
