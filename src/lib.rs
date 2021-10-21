pub mod ast;
pub mod commands;
pub mod expect;
pub mod lexer;
pub mod parser;
pub mod pp;

mod error;

pub use self::error::Error;
pub use self::expect::Expect;
pub use self::lexer::{Lexer, Region};
pub use self::parser::Parse;

pub type Result<T> = std::result::Result<T, Error>;
