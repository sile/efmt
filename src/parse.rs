use crate::items::tokens::LexicalToken;
use crate::span::{Position, Span as _};
use std::path::PathBuf;
use std::sync::Arc;

pub use self::include::IncludeOptions;
pub use self::token_stream::TokenStream;

/// A procedural macro to derive [Parse].
pub use efmt_derive::Parse;

pub(crate) mod include;
pub(crate) mod token_stream;

/// Possible errors.
#[derive(Debug, Clone, thiserror::Error)]
pub enum Error {
    /// Unexpected EOF.
    #[error("Parse failed:{}", Self::unexpected_eof_message(.position, .text, .path))]
    UnexpectedEof {
        position: Position,
        text: Arc<String>,
        path: Option<Arc<PathBuf>>,
    },

    /// Unexpected token.
    #[error("Parse failed:{}", Self::unexpected_token_message(.position, .text, .path))]
    UnexpectedToken {
        position: Position,
        text: Arc<String>,
        path: Option<Arc<PathBuf>>,
    },

    /// Error during tokenization.
    #[error("Tokenize failed:{}", Self::tokenize_error_message(.source, .text))]
    TokenizeError {
        source: erl_tokenize::Error,
        text: Arc<String>,
    },
}

impl Error {
    pub(crate) fn unexpected_token(ts: &TokenStream, token: LexicalToken) -> Self {
        Self::UnexpectedToken {
            position: token.start_position(),
            text: ts.text(),
            path: ts.filepath(),
        }
    }

    pub(crate) fn unexpected_eof(ts: &TokenStream) -> Self {
        Self::UnexpectedEof {
            position: ts.prev_token_end_position(),
            text: ts.text(),
            path: ts.filepath(),
        }
    }

    pub(crate) fn tokenize_error(ts: &TokenStream, source: erl_tokenize::Error) -> Self {
        Self::TokenizeError {
            source,
            text: ts.text(),
        }
    }

    pub(crate) fn position(&self) -> Position {
        match self {
            Self::UnexpectedEof { position, .. } => *position,
            Self::UnexpectedToken { position, .. } => *position,
            Self::TokenizeError { source, .. } => source.position().clone().into(),
        }
    }

    fn tokenize_error_message(source: &erl_tokenize::Error, text: &Arc<String>) -> String {
        let source_message = source.to_string();
        let source_message_end = source_message
            .find(" (")
            .unwrap_or_else(|| source_message.len());
        crate::error::generate_error_message(
            text,
            source.position().filepath(),
            source.position().clone().into(),
            &source_message[..source_message_end],
        )
    }

    fn unexpected_eof_message(
        position: &Position,
        text: &Arc<String>,
        path: &Option<Arc<PathBuf>>,
    ) -> String {
        crate::error::generate_error_message(
            text,
            path.as_ref().map(|x| &**x),
            *position,
            "unexpected EOF",
        )
    }

    fn unexpected_token_message(
        position: &Position,
        text: &Arc<String>,
        path: &Option<Arc<PathBuf>>,
    ) -> String {
        crate::error::generate_error_message(
            text,
            path.as_ref().map(|x| &**x),
            *position,
            "unexpected token",
        )
    }
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

#[cfg(test)]
mod tests {
    use crate::items::Module;

    #[test]
    fn unexpected_token_message_works() {
        let text = indoc::indoc! {"
        foo() ->
            [a, b | #c].
        "};
        let err = crate::format_text::<Module>(text).err().unwrap();
        similar_asserts::assert_str_eq!(
            err.to_string(),
            indoc::indoc! {"
        Parse failed:
        --> <unknown>:2:15
        2 |     [a, b | #c].
          |               ^ unexpected token"}
        );
    }

    #[test]
    fn unexpected_token_message_with_macro_works() {
        let text = indoc::indoc! {"
        -define(ID(A), A).
        foo() ->
            ?ID([a, b | #c]).
        "};
        let err = crate::format_text::<Module>(text).err().unwrap();
        similar_asserts::assert_str_eq!(
            err.to_string(),
            indoc::indoc! {"
        Parse failed:
        --> <unknown>:3:19
        3 |     ?ID([a, b | #c]).
          |                   ^ unexpected token"}
        );
    }

    #[test]
    fn unexpected_eof_message_works() {
        let text = indoc::indoc! {"
        foo() ->
            hello
        "};
        let err = crate::format_text::<Module>(text).err().unwrap();
        similar_asserts::assert_str_eq!(
            err.to_string(),
            indoc::indoc! {"
        Parse failed:
        --> <unknown>:2:10
        2 |     hello
          |          ^ unexpected EOF"}
        );
    }

    #[test]
    fn tokenize_error_message_works() {
        let text = indoc::indoc! {r#"
        foo() ->
            "hello
        "#};
        let err = crate::format_text::<Module>(text).err().unwrap();
        similar_asserts::assert_str_eq!(
            err.to_string(),
            indoc::indoc! {r#"
        Tokenize failed:
        --> <unknown>:2:5
        2 |     "hello
          |     ^ no closing quotation"#}
        );
    }
}
