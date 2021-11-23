use crate::format::{Format, Formatter};
use crate::items::components::{Args, Either, Maybe};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, OpenParenSymbol, QuestionSymbol,
};
use crate::items::tokens::{AtomToken, LexicalToken, StringToken, VariableToken};
use crate::items::Expr;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};
use std::collections::HashMap;

/// `?` `$NAME` (`(` (`$ARG` `,`?)* `)`)?
///
/// - $NAME: [AtomToken] | [VariableToken]
/// - $ARG: [LexicalToken]+
#[derive(Debug, Clone, Span, Format)]
pub struct Macro {
    question: QuestionSymbol,
    name: MacroName,
    args: Maybe<MacroArgs>,
}

impl Macro {
    // TODO: resume_parse
    pub(crate) fn parse(
        ts: &mut TokenStream,
        question: QuestionSymbol,
        name: MacroName,
        has_args: bool,
    ) -> parse::Result<Self> {
        if has_args {
            Ok(Self {
                question,
                name,
                args: Maybe::from_item(ts.parse()?),
            })
        } else {
            Ok(Self {
                question,
                name,
                args: Maybe::none(ts)?,
            })
        }
    }

    pub fn arity(&self) -> Option<usize> {
        self.args.get().map(|x| x.get().len())
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
                LexicalToken::Variable(x) if do_stringify => {
                    let dummy =
                        StringToken::new("EFMT_DUMMY", x.start_position(), x.end_position());
                    tokens.push(dummy.into());
                }
                LexicalToken::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].tokens().iter().cloned());
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::DoubleQuestion => {
                    do_stringify = true;
                    continue;
                }
                token => {
                    tokens.push(token);
                }
            }
            do_stringify = false;
        }
        // TODO: delete(?)
        tokens.iter_mut().for_each(|token| token.set_span(self));
        tokens
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub(crate) struct MacroName(Either<AtomToken, VariableToken>);

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
    expr: Option<Expr>, // The expression representation of `tokens` (for formatting)
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
        ts.enter_macro_replacement(|ts| {
            let start_position = ts.current_whitespace_token()?.end_position();
            let expr = ts
                .peek::<(Expr, (CloseParenSymbol, DotSymbol))>()
                .map(|(expr, _)| expr);
            let mut tokens = Vec::new();
            while ts.peek::<(CloseParenSymbol, DotSymbol)>().is_none() {
                tokens.push(ts.parse()?);
            }
            Ok(Self {
                tokens,
                expr,
                start_position,
            })
        })
    }
}

impl Format for MacroReplacement {
    fn format(&self, fmt: &mut Formatter) {
        if let Some(expr) = &self.expr {
            expr.format(fmt);
        } else {
            fmt.add_span(self);
        }
    }
}

// TODO: Use `Null`
#[derive(Debug, Clone)]
struct Empty;

impl Span for Empty {
    fn start_position(&self) -> Position {
        unreachable!()
    }

    fn end_position(&self) -> Position {
        unreachable!()
    }
}

impl Parse for Empty {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let token = ts.parse()?;
        Err(parse::Error::unexpected_token(ts, token))
    }
}

impl Format for Empty {
    fn format(&self, _: &mut Formatter) {
        unreachable!();
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
enum MacroArgs {
    Empty(Args<Empty>),
    Args(Args<MacroArg>),
}

impl MacroArgs {
    fn get(&self) -> &[MacroArg] {
        match self {
            Self::Empty(_) => &[],
            Self::Args(x) => x.get(),
        }
    }
}

#[derive(Debug, Clone)]
struct MacroArg {
    tokens: Vec<LexicalToken>,

    // TODO: Support macros in a macro arg
    expr: Option<Expr>, // The expression representation of `tokens` (for formatting)
}

impl MacroArg {
    pub fn tokens(&self) -> &[LexicalToken] {
        &self.tokens
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
        let mut expr = ts.peek::<Expr>();

        #[derive(Debug, Default, PartialEq, Eq)]
        struct Level {
            paren: usize,
            brace: usize,
            square: usize,
            bits: usize,
            block: usize,
        }

        impl Level {
            fn is_toplevel(&self) -> bool {
                *self == Self::default()
            }
        }

        let mut tokens = Vec::new();
        let mut level = Level::default();
        while tokens.is_empty()
            || !level.is_toplevel()
            || ts.peek::<Either<CommaSymbol, CloseParenSymbol>>().is_none()
        {
            let token: LexicalToken = ts.parse()?;
            match &token {
                LexicalToken::Symbol(x) => match x.value() {
                    Symbol::OpenParen => {
                        level.paren += 1;
                    }
                    Symbol::CloseParen => {
                        if level.paren == 0 {
                            todo!("{:?}", ts.current_position());
                        }
                        level.paren -= 1;
                    }
                    Symbol::OpenBrace => {
                        level.brace += 1;
                    }
                    Symbol::CloseBrace => {
                        if level.brace == 0 {
                            todo!("{:?}", ts.current_position());
                        }
                        level.brace -= 1;
                    }
                    Symbol::OpenSquare => {
                        level.square += 1;
                    }
                    Symbol::CloseSquare => {
                        if level.square == 0 {
                            todo!("{:?}", ts.current_position());
                        }

                        level.square -= 1;
                    }
                    Symbol::DoubleLeftAngle => {
                        level.bits += 1;
                    }
                    Symbol::DoubleRightAngle => {
                        if level.bits == 0 {
                            todo!("{:?}", ts.current_position());
                        }
                        level.bits -= 1;
                    }
                    _ => {}
                },
                LexicalToken::Keyword(x) => match x.value() {
                    Keyword::Begin
                    | Keyword::Try
                    | Keyword::Case
                    | Keyword::If
                    | Keyword::Receive => {
                        level.block += 1;
                    }
                    Keyword::Fun => {
                        if ts.peek::<OpenParenSymbol>().is_some()
                            || ts.peek::<(LexicalToken, OpenParenSymbol)>().is_some()
                        {
                            level.block += 1;
                        }
                    }
                    Keyword::End => {
                        if level.block == 0 {
                            todo!("{:?}", ts.current_position());
                        }
                        level.block -= 1;
                    }
                    _ => {}
                },
                _ => {}
            }
            tokens.push(token);
        }

        let is_expr = expr.as_ref().map_or(false, |x| {
            x.end_position() == tokens[tokens.len() - 1].end_position()
        });
        if !is_expr {
            expr = None;
        }

        Ok(Self { tokens, expr })
    }
}

impl Format for MacroArg {
    fn format(&self, fmt: &mut Formatter) {
        if let Some(expr) = &self.expr {
            expr.format(fmt);
        } else {
            fmt.add_span(self);
        }
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
            %---10---|%---20---|
            -define(FOO(), foo).
            foo() ->
                ?FOO().
            "},
            indoc::indoc! {"
            %---10---|%---20---|
            -define(FOO(Bar),
                    {Bar, Baz}).
            qux() ->
                ?FOO(quux).
            "},
            indoc::indoc! {"
            %---10---|%---20---|
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
            %---10---|%---20---|
            -define(FOO(), foo).
            -define(FOO,
                    foo foo).
            foo() ->
                ?FOO() + 10.
            "},
            indoc::indoc! {"
            %---10---|%---20---|
            -define(FOO,
                    begin
                        1,
                        2
                    end).
            foo() ->
                ?FOO.
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
                ?bar(?foo)c].
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
                ?b(?a a), c].
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
            %---10---|%---20---|
            -define(FOO(A), A).
            qux() ->
                ?FOO(\"aaa\"
                     \"bbb\"  % comment
                     \"ccc\").
            "},
            indoc::indoc! {"
            %---10---|%---20---|
            -define(FOO, foo).
            qux(A) ->
                case A of
                    a ->
                        ?FOO
                %% comment
                end.
            "},
            indoc::indoc! {"
            %---10---|%---20---|
            -define(a(X), X X).
            foo() ->
                1 ?a(?a(+1)).
            "}, // TODO
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_lazy_expand_works() {
        let texts = [
            indoc::indoc! {"
            %---10---|%---20---|
            -define(BAR,
                    ?FOO 1 end).
            -define(FOO, begin).
            baz() ->
                ?BAR.
            "},
            indoc::indoc! {"
            %---10---|%---20---|
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
        let texts = [indoc::indoc! {"
            %---10---|%---20---|
            -define(a, ?b).
            -define(b, ?a).
            foo() ->
                ?a == ?b.
            "}];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }
}
