extern crate erl_parse;
extern crate erl_pp;
extern crate erl_tokenize;
#[macro_use]
extern crate trackable;

pub use error::{Error, ErrorKind};

pub mod formatter;

mod error;

pub type Result<T> = ::std::result::Result<T, Error>;
