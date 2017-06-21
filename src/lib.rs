extern crate erl_parse;
extern crate erl_pp;
extern crate erl_tokenize;
#[macro_use]
extern crate trackable;

pub use error::{Error, ErrorKind};

mod error;

pub type Result<T> = ::std::result::Result<T, Error>;

pub fn parse_erl_file<P: AsRef<std::path::Path>>(file: P) -> Result<erl_parse::cst::ModuleDecl> {
    use std::fs::File;
    use std::io::Read;

    let mut code = String::new();
    let mut file = track!(File::open(file).map_err(Error::from))?;
    track!(file.read_to_string(&mut code).map_err(Error::from))?;

    let lexer = erl_tokenize::Lexer::new(code);
    let preprocessor = erl_pp::Preprocessor::new(lexer);
    let reader = erl_parse::TokenReader::new(preprocessor);
    let mut parser = erl_parse::Parser::new(reader);
    let module = track!(parser.parse())?;
    Ok(module)
}
