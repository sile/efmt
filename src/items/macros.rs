use crate::format::{self, Format, Formatter};
use crate::items::expressions::Expr;
use crate::items::generics::{Either, Items, Maybe, Parenthesized};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, OpenParenSymbol, QuestionSymbol,
};
use crate::items::tokens::{AtomToken, StringToken, Token, VariableToken};
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};
use std::collections::HashMap;

#[derive(Debug, Clone, Span, Format)]
pub struct Macro {
    question: QuestionSymbol,
    name: MacroName,
    args: Maybe<Parenthesized<Items<MacroArg>>>,
}

impl Macro {
    pub fn parse(
        ts: &mut TokenStream,
        question: QuestionSymbol,
        name: MacroName,
        arity: Option<usize>,
    ) -> parse::Result<Self> {
        if let Some(_arity) = arity {
            Ok(Self {
                question,
                name,
                args: ts.parse()?,
            })
        } else {
            Ok(Self {
                question,
                name,
                args: Maybe::none(ts)?,
            })
        }
    }

    pub fn expand(&self, variables: Option<Vec<String>>, replacement: Vec<Token>) -> Vec<Token> {
        let args = if let (Some(vars), Some(vals)) =
            (&variables, self.args.get().map(|x| x.get().get()))
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
                Token::Variable(x) if do_stringify => {
                    let dummy =
                        StringToken::new("EFMT_DUMMY", x.start_position(), x.end_position());
                    tokens.push(dummy.into());
                }
                Token::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].tokens().iter().cloned());
                }
                Token::Symbol(x) if x.value() == Symbol::DoubleQuestion => {
                    do_stringify = true;
                    continue;
                }
                token => {
                    tokens.push(token);
                }
            }
            do_stringify = false;
        }
        tokens.iter_mut().for_each(|token| token.set_span(self));
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
pub struct MacroReplacement {
    tokens: Vec<Token>,
    expr: Option<Expr>, // The expression representation of `tokens` (for formatting)
    start_position: Position,
}

impl MacroReplacement {
    pub fn tokens(&self) -> &[Token] {
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
    }
}

impl Format for MacroReplacement {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if let Some(expr) = &self.expr {
            expr.format(fmt)
        } else {
            fmt.write_text(self)
        }
    }
}

#[derive(Debug, Clone)]
pub struct MacroArg {
    tokens: Vec<Token>,
    expr: Option<Expr>, // The expression representation of `tokens` (for formatting)
}

impl MacroArg {
    pub fn tokens(&self) -> &[Token] {
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
            || ts
                .peek::<Either<CommaSymbol, CloseParenSymbol>>()
                .filter(|t| !ts.macros().contains_key(&t.start_position()))
                .is_none()
        {
            let token: Token = ts.parse()?;

            let is_macro_expanded = ts.macros().contains_key(&token.start_position());
            if is_macro_expanded {
                tokens.push(token);
                continue;
            }

            match &token {
                Token::Symbol(x) => match x.value() {
                    Symbol::OpenParen => {
                        level.paren += 1;
                    }
                    Symbol::CloseParen => {
                        if level.paren == 0 {
                            todo!()
                        }
                        level.paren -= 1;
                    }
                    Symbol::OpenBrace => {
                        level.brace += 1;
                    }
                    Symbol::CloseBrace => {
                        if level.brace == 0 {
                            todo!();
                        }
                        level.brace -= 1;
                    }
                    Symbol::OpenSquare => {
                        level.square += 1;
                    }
                    Symbol::CloseSquare => {
                        if level.square == 0 {
                            todo!();
                        }

                        level.square -= 1;
                    }
                    Symbol::DoubleLeftAngle => {
                        level.bits += 1;
                    }
                    Symbol::DoubleRightAngle => {
                        if level.bits == 0 {
                            todo!();
                        }
                        level.bits -= 1;
                    }
                    _ => {}
                },
                Token::Keyword(x) => match x.value() {
                    Keyword::Begin | Keyword::Try | Keyword::Case | Keyword::If => {
                        level.block += 1;
                    }
                    Keyword::Fun => {
                        if ts.peek::<OpenParenSymbol>().is_some()
                            || ts.peek::<(Token, OpenParenSymbol)>().is_some()
                        {
                            level.block += 1;
                        }
                    }
                    Keyword::End => {
                        if level.block == 0 {
                            todo!();
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
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if let Some(expr) = &self.expr {
            expr.format(fmt)
        } else {
            fmt.write_text(self)
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
            -define(INC, 1 +).
            inc(A) ->
                ?INC A.
            "},
            indoc::indoc! {"
            -define(FOO_OPEN, foo().
            -define(FOO_CLOSE, )).
            foo(A) ->
                ?FOO_OPEN A ?FOO_CLOSE.
            "},
            indoc::indoc! {"
            -define(EMPTY, ).

            ?EMPTY hello ?EMPTY () ->
                ?EMPTY ?EMPTY world.
            ?EMPTY"},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn macro_with_args_works() {
        let texts = [
            // TODO: Should have a newline after ',' if the line is too long.
            indoc::indoc! {"
            -define(FOO(Bar), {Bar,
                               Baz}).
            qux() ->
                ?FOO(quux).
            "},
            indoc::indoc! {"
            -define(FOO(Bar,
                        Baz), {Bar,
                               Baz}).
            qux() ->
                ?FOO(begin
                         foo,
                         bar,
                         baz
                     end,
                     hello).
            "},
            indoc::indoc! {"
            -define(FOO(Bar), Bar).

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
            -define(Foo(A), ??A).
            bar() ->
                ?Foo(10).
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
                ?bar(?foo) [c].
            "},
            indoc::indoc! {"
            -define(a, [1, 2, 3], [).
            -define(b(A), A).

            main() ->
                ?b(?a a), c].
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }
}
