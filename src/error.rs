// TODO: rename to ParseError (or move into `crate::parser` module)
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected EOF")]
    UnexpectedEof,

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
