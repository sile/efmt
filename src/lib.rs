pub mod ast;
pub mod commands;
pub mod lexer;
pub mod parser;

pub type Result<T> = anyhow::Result<T>;
