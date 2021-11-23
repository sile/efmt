use crate::items::tokens::LexicalToken;
use crate::span::{Position, Span as _};
use std::path::PathBuf;
use std::sync::Arc;

pub use self::token_stream::{TokenStream, TokenStreamOptions};

/// A procedural macro to derive [Parse].
pub use efmt_derive::Parse;

mod token_stream;

/// Possible errors.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Unexpected EOF.
    #[error("Parse failed: Unexpected EOF")]
    UnexpectedEof { position: Position },

    /// Unexpected token.
    #[error("Parse failed: {}", unexpected_token_error_message(.position, .text, .path))]
    UnexpectedToken {
        position: Position,
        text: Arc<String>,
        path: Option<Arc<PathBuf>>,
    },

    /// Error during tokenization.
    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

impl Error {
    pub(crate) fn unexpected_token(ts: &TokenStream, token: LexicalToken) -> Self {
        Self::UnexpectedToken {
            position: token.start_position(),
            text: ts.text(),
            path: ts.filepath(),
        }
    }

    pub(crate) fn position(&self) -> Position {
        match self {
            Self::UnexpectedEof { position } => *position,
            Self::UnexpectedToken { position, .. } => *position,
            Self::TokenizeError(x) => x.position().clone().into(),
        }
    }
}

// TODO:
fn unexpected_token_error_message(
    position: &Position,
    text: &Arc<String>,
    path: &Option<Arc<PathBuf>>,
) -> String {
    let line = position.line();
    let column = position.column();
    let file = path
        .as_ref()
        .and_then(|x| x.to_str().map(|x| x.to_owned()))
        .unwrap_or_else(|| "<anonymous>".to_owned());
    let line_string = get_line_string(text, position);

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

fn get_line_string<'a>(text: &'a str, position: &Position) -> &'a str {
    let offset = position.offset();
    let line_start = (&text[..offset]).rfind('\n').unwrap_or(0);
    let line_end = (&text[offset..])
        .find('\n')
        .map(|x| x + offset)
        .unwrap_or_else(|| text.len());
    (&text[line_start..line_end]).trim_matches(char::is_control)
}

/// A specialized [Result][std::result::Result] type for this module.
pub type Result<T> = std::result::Result<T, Error>;

/// This trait allows parsing an item from a token stream.
pub trait Parse: Sized {
    /// Parse an item from the given token stream.
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

/// This trait allows resuming to parse an item from a token stream.
pub trait ResumeParse<A>: Parse {
    /// Resume to parse an item from the given token stream.
    ///
    /// The second argument is the item that has already been parsed from the stream.
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
