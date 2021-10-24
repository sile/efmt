//pub mod ast;
pub mod commands;
pub mod expect;
//pub mod formatter;
//pub mod lexer;
//pub mod parser;
pub mod pp;
pub mod token;

mod error;

pub use self::error::Error;
pub use self::expect::Expect;
// pub use self::lexer::{Lexer, Region};
// pub use self::parser::{Parse, ResumeParse};

pub type Result<T> = std::result::Result<T, Error>;

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Read as _;
    use std::path::PathBuf;

    pub fn load_before_and_after_files(before_path: PathBuf) -> (String, String) {
        let before_name = before_path.file_stem().unwrap().to_str().unwrap();
        let after_name = before_name.replace("_before", "_after");
        let after_path = before_path
            .parent()
            .unwrap()
            .join(format!("{}.erl", after_name));

        let mut before = String::new();
        let mut after = String::new();
        File::open(before_path)
            .unwrap()
            .read_to_string(&mut before)
            .unwrap();
        File::open(after_path)
            .unwrap()
            .read_to_string(&mut after)
            .unwrap();
        (before, after)
    }
}
