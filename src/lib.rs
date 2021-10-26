pub mod cst;
//pub mod ast;
pub mod commands;
pub mod expect;
//pub mod formatter;
//pub mod lexer;
//pub mod parser;
pub mod format;
pub mod parse;
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
    use crate::format::{Format, Formatter};
    use crate::parse::{Parse, TokenReader};
    use crate::pp::Preprocessor;
    use erl_tokenize::Tokenizer;
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

    pub fn test_parse_and_format<T: Parse + Format>(testname: &str) {
        let (text, expected) = load_testdata(testname);
        let tokenizer = Tokenizer::new(text);
        let pp = Preprocessor::new(tokenizer);
        let preprocessed = pp
            .preprocess()
            .expect(&format!("[{}] cannot preprocess", testname));

        let mut buf = Vec::new();
        {
            let mut fmt = Formatter::new(&mut buf, preprocessed.clone());
            let cst = T::parse(&mut TokenReader::new(preprocessed.tokens))
                .expect(&format!("[{}] cannot parse", testname));
            fmt.format(&cst)
                .expect(&format!("[{}] cannot format", testname));
        }
        assert_eq!(
            String::from_utf8_lossy(&buf),
            expected,
            "[{}] unexpected formatted code",
            testname
        );
    }

    fn load_testdata(testname: &str) -> (String, String) {
        let before_path = format!("testdata/{}-before.erl", testname);
        let after_path = format!("testdata/{}-after.erl", testname);
        let before =
            std::fs::read_to_string(&before_path).expect(&format!("cannot read {:?}", before_path));
        let after =
            std::fs::read_to_string(&after_path).expect(&format!("cannot read {:?}", after_path));
        (before, after)
    }
}
