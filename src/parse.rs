use crate::items::tokens::Token;
use crate::lex::{self};
use crate::span::{Position, Span as _};

pub use efmt_derive::Parse;

pub use crate::lex::TokenStream;

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
    pub fn unexpected_token(ts: &mut TokenStream, token: Token) -> Self {
        let message = ts.generate_error_place(&token);
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
    fn parse(ts: &mut TokenStream) -> Result<Self>;
}

impl Parse for Token {
    fn parse(ts: &mut TokenStream) -> Result<Self> {
        let token = ts.read_token()?.ok_or(Error::UnexpectedEof)?;
        Ok(token)
    }
}

impl<A: Parse> Parse for Box<A> {
    fn parse(ts: &mut TokenStream) -> Result<Self> {
        ts.parse().map(Box::new)
    }
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    fn parse(ts: &mut TokenStream) -> Result<Self> {
        Ok((ts.parse()?, ts.parse()?))
    }
}

#[cfg(test)]
pub fn parse_text<T: Parse>(text: &str) -> anyhow::Result<T> {
    let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
    let mut ts = TokenStream::new(tokenizer);
    let item = ts.parse()?;
    anyhow::ensure!(ts.is_eof()?, "there are unconsumed tokens");
    Ok(item)
}
