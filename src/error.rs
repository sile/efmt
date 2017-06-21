use std;
use erl_parse;
use trackable::error::TrackableError;
use trackable::error::{ErrorKind as TrackableErrorKind, ErrorKindExt};

#[derive(Debug, Clone)]
pub enum ErrorKind {
    Other,
}
impl TrackableErrorKind for ErrorKind {}

#[derive(Debug, Clone)]
pub struct Error(TrackableError<ErrorKind>);
derive_traits_for_trackable_error_newtype!(Error, ErrorKind);
impl From<std::io::Error> for Error {
    fn from(f: std::io::Error) -> Self {
        ErrorKind::Other.cause(f).into()
    }
}
impl From<erl_parse::Error> for Error {
    fn from(f: erl_parse::Error) -> Self {
        ErrorKind::Other.takes_over(f).into()
    }
}
