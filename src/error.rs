#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
