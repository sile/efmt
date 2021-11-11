use crate::items::tokens::Token;
use crate::lex::{self};
use crate::span::{Position, Span as _};

pub use efmt_derive::Parse;

pub use crate::lex::Lexer;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("{message}")]
    UnexpectedToken { token: Token, message: String },

    #[error(transparent)]
    LexError(#[from] lex::Error),
}

impl Error {
    pub fn unexpected_token(lexer: &mut Lexer, token: Token) -> Self {
        let message = lexer.generate_error_place(&token);
        Self::UnexpectedToken { token, message }
    }

    // TODO
    pub fn position(&self) -> Option<Position> {
        if let Self::UnexpectedToken { token, .. } = self {
            Some(token.start_position())
        } else {
            None
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(lexer: &mut Lexer) -> Result<Self>;
}

impl Parse for Token {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let token = lexer.read_token()?.ok_or(Error::UnexpectedEof)?;
        Ok(token)
    }
}

impl<A: Parse> Parse for Box<A> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        lexer.parse().map(Box::new)
    }
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        Ok((lexer.parse()?, lexer.parse()?))
    }
}

#[cfg(test)]
pub fn parse_text<T: Parse>(text: &str) -> anyhow::Result<T> {
    let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
    let mut lexer = Lexer::new(tokenizer);
    let item = lexer.parse()?;
    anyhow::ensure!(lexer.is_eof()?, "there are unconsumed tokens");
    Ok(item)
}
