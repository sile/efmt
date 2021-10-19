pub mod ast;
pub mod commands;
pub mod lexer;
pub mod parser;

mod error;

pub use self::error::Error;

pub type Result<T> = std::result::Result<T, Error>;
