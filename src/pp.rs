mod error;
mod macros;
mod preprocessor;

pub use self::error::Error;
pub use self::macros::MacroCall;
pub use self::macros::MacroDefine; // TODO: remove
pub use self::preprocessor::{PreprocessedText, Preprocessor};

pub type Result<T> = std::result::Result<T, Error>;
