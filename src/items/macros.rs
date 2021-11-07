use crate::format::{self, Format, Formatter};
use crate::items::generics::{Either, Items, Maybe, Parenthesized};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, OpenParenSymbol, QuestionSymbol,
};
use crate::items::tokens::{AtomToken, Token, VariableToken};
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};
use erl_tokenize::values::{Keyword, Symbol};
use std::collections::HashMap;

#[derive(Debug, Clone, Span, Format)]
pub struct Macro {
    question: QuestionSymbol,
    name: MacroName,
    args: Maybe<Parenthesized<Items<MacroArg, CommaSymbol>>>,
}

impl Macro {
    pub fn parse(
        parser: &mut Parser,
        question: QuestionSymbol,
        name: MacroName,
        arity: Option<usize>,
    ) -> parse::Result<Self> {
        if let Some(_arity) = arity {
            Ok(Self {
                question,
                name,
                args: parser.parse()?,
            })
        } else {
            Ok(Self {
                question,
                name,
                args: Maybe::none(parser)?,
            })
        }
    }

    pub fn expand(
        &self,
        variables: Option<Vec<VariableToken>>,
        replacement: Vec<Token>,
    ) -> Vec<Token> {
        let args = if let (Some(vars), Some(vals)) =
            (&variables, self.args.get().map(|x| x.get().get()))
        {
            vars.iter()
                .map(|x| x.value())
                .zip(vals.iter())
                .collect::<HashMap<_, _>>()
        } else {
            HashMap::new()
        };

        let mut tokens = Vec::new();
        for token in replacement {
            match token {
                Token::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].tokens().iter().cloned());
                }
                token => {
                    tokens.push(token);
                }
            }
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
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start_position = parser.next_token_start_position()?;
        let mut tokens = Vec::new();
        while !parser.peek::<(CloseParenSymbol, DotSymbol)>() {
            tokens.push(parser.parse()?);
        }
        Ok(Self {
            tokens,
            start_position,
        })
    }
}

impl Format for MacroReplacement {
    // TODO: try parse
    // TODO: consider comment (by formatter)
    fn format(&self, _fmt: &mut Formatter) -> format::Result<()> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct MacroArg {
    tokens: Vec<Token>,
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
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
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
            || !parser.peek::<Either<CommaSymbol, CloseParenSymbol>>()
        {
            let token: Token = parser.parse()?;

            let is_macro_expanded = parser.macros().contains_key(&token.start_position());
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
                        if parser.peek::<OpenParenSymbol>()
                            || parser.peek::<(Token, OpenParenSymbol)>()
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
        Ok(Self { tokens })
    }
}

impl Format for MacroArg {
    // TODO: try parse
    // TODO: consider comment (by formatter)
    fn format(&self, _fmt: &mut Formatter) -> format::Result<()> {
        todo!()
    }
}
