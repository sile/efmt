use crate::items::tokens::Token;
use crate::span::{Position, Span as _};
use std::path::PathBuf;

pub use self::token_stream::TokenStream;
pub use efmt_derive::Parse;

mod token_stream;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("{message}")]
    UnexpectedToken { token: Token, message: String },

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

impl Error {
    pub fn unexpected_token(ts: &mut TokenStream, token: Token) -> Self {
        let message = generate_error_place(ts.text(), ts.filepath(), &token);
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
        let token = ts.next().transpose()?.ok_or(Error::UnexpectedEof)?;
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
    let mut ts = TokenStream::new(tokenizer, Default::default());
    let item = ts.parse()?;
    anyhow::ensure!(ts.is_eof()?, "there are unconsumed tokens");
    Ok(item)
}

// TODO
fn generate_error_place(text: &str, filepath: Option<PathBuf>, unexpected_token: &Token) -> String {
    use std::fmt::Write;

    let line = unexpected_token.start_position().line();
    let column = unexpected_token.start_position().column();
    let file = filepath
        .and_then(|x| x.to_str().map(|x| x.to_owned()))
        .unwrap_or_else(|| "<anonymous>".to_owned());
    let line_string = get_line_string(text, unexpected_token);

    let mut m = String::new();
    writeln!(&mut m).unwrap();
    writeln!(&mut m, "--> {}:{}:{}", file, line, column).unwrap();
    writeln!(&mut m, "{} | {}", line, line_string).unwrap();
    writeln!(
        &mut m,
        "{:line_width$} | {:>token_column$} unexpected token",
        "",
        "^",
        line_width = line.to_string().len(),
        token_column = column
    )
    .unwrap();
    m
}

fn get_line_string<'a>(text: &'a str, token: &Token) -> &'a str {
    let offset = token.start_position().offset();
    let line_start = (&text[..offset]).rfind('\n').unwrap_or(0);
    let line_end = (&text[offset..])
        .find('\n')
        .map(|x| x + offset)
        .unwrap_or_else(|| text.len());
    (&text[line_start..line_end]).trim_matches(char::is_control)
}
