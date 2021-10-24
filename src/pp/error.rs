use erl_tokenize::Position;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("macro {name:?} is not defined ({position:?})")]
    UndefinedMacro { name: String, position: Position },

    #[error("badly formed argument ({position:?})")]
    MalformedMacroArg { position: Position },

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}
