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
    use std::path::PathBuf;

    pub fn load_before_and_after_files(before_path: PathBuf) -> (String, String) {
        let before_name = before_path.file_stem().unwrap().to_str().unwrap();
        let after_name = before_name.replace("_before", "_after");
        let after_path = before_path
            .parent()
            .unwrap()
            .join(format!("{}.erl", after_name));

        let before = std::fs::read_to_string(&before_path)
            .expect(&format!("failed to read {:?}", before_path));
        let after = std::fs::read_to_string(&after_path)
            .expect(&format!("failed to read {:?}", after_path));
        (before, after)
    }
}
