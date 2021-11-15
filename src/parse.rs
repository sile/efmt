use crate::items::tokens::Token;
use crate::span::{Position, Span as _};
use std::path::PathBuf;

pub use self::token_stream::{TokenStream, TokenStreamOptions};
pub use efmt_derive::Parse;

mod token_stream;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof { position: Position },

    #[error("{message}")]
    UnexpectedToken { position: Position, message: String },

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

impl Error {
    pub fn unexpected_token(ts: &TokenStream, token: Token) -> Self {
        let message = Self::generate_unexpected_token_place(ts.text(), ts.filepath(), &token);
        Self::UnexpectedToken {
            position: token.start_position(),
            message,
        }
    }

    pub fn position(&self) -> Position {
        match self {
            Self::UnexpectedEof { position } => *position,
            Self::UnexpectedToken { position, .. } => *position,
            Self::TokenizeError(x) => x.position().clone().into(),
        }
    }

    fn generate_unexpected_token_place(
        text: &str,
        filepath: Option<PathBuf>,
        unexpected_token: &Token,
    ) -> String {
        let line = unexpected_token.start_position().line();
        let column = unexpected_token.start_position().column();
        let file = filepath
            .and_then(|x| x.to_str().map(|x| x.to_owned()))
            .unwrap_or_else(|| "<anonymous>".to_owned());
        let line_string = Self::get_line_string(text, unexpected_token);

        let mut m = String::new();
        m.push_str(&format!("\n--> {}:{}:{}\n", file, line, column));
        m.push_str(&format!("{} | {}\n", line, line_string));
        m.push_str(&format!(
            "{:line_width$} | {:>token_column$} unexpected token",
            "",
            "^",
            line_width = line.to_string().len(),
            token_column = column
        ));
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
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(ts: &mut TokenStream) -> Result<Self>;
}

impl<T: Parse> Parse for Box<T> {
    fn parse(ts: &mut TokenStream) -> Result<Self> {
        ts.parse().map(Box::new)
    }
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    fn parse(ts: &mut TokenStream) -> Result<Self> {
        Ok((ts.parse()?, ts.parse()?))
    }
}

pub trait ResumeParse<A>: Parse {
    fn resume_parse(ts: &mut TokenStream, args: A) -> Result<Self>;
}

impl<T, A> ResumeParse<A> for Box<T>
where
    T: ResumeParse<A>,
{
    fn resume_parse(ts: &mut TokenStream, args: A) -> Result<Self> {
        ts.resume_parse(args).map(Box::new)
    }
}
