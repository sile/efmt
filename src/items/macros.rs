use crate::format::{self, Format, Formatter};
use crate::items::generics::Either;
use crate::items::symbols::{CloseParenSymbol, DotSymbol};
use crate::items::tokens::{AtomToken, Token, VariableToken};
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};
use erl_tokenize::values::Symbol;
use std::io::Write;

#[derive(Debug, Clone)]
pub struct Macro {}

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
            match parser.parse()? {
                Token::Symbol(x) if x.value() == Symbol::Dot => {
                    return Err(parse::Error::unexpected_token(parser, x.into()));
                }
                token => {
                    tokens.push(token);
                }
            }
        }

        Ok(Self {
            tokens,
            start_position,
        })
    }
}

impl Format for MacroReplacement {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        // TODO: try parse and format
        fmt.noformat(self)
    }
}
