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
    use anyhow::Context;
    use erl_tokenize::Tokenizer;
    use std::path::PathBuf;

    // TODO: delete
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

    #[inline]
    pub fn test_parse_and_format<T: Parse + Format>(testname: &str) -> anyhow::Result<()> {
        let (text, expected) = load_testdata(testname).with_context(|| "cannot load testdata")?;
        let tokenizer = Tokenizer::new(text);
        let pp = Preprocessor::new(tokenizer);
        let preprocessed = pp.preprocess().with_context(|| "cannot preprocess")?;

        let mut buf = Vec::new();
        {
            let mut fmt = Formatter::new(&mut buf, preprocessed.clone());
            let cst = T::parse(&mut TokenReader::new(preprocessed.tokens))
                .with_context(|| "cannot parse")?;
            fmt.format(&cst).with_context(|| "cannot format")?;
        }
        let formatted = String::from_utf8_lossy(&buf);
        anyhow::ensure!(
            formatted == expected.trim(),
            "unexpected formatted code.\n[ACTUAL]\n{}\n[EXPECTED]\n{}",
            formatted,
            expected
        );
        Ok(())
    }

    fn load_testdata(testname: &str) -> anyhow::Result<(String, String)> {
        let before_path = format!("testdata/{}-before.erl", testname);
        let after_path = format!("testdata/{}-after.erl", testname);
        let before = std::fs::read_to_string(&before_path)
            .with_context(|| format!("cannot read {:?}", before_path))?;
        let after = std::fs::read_to_string(&after_path)
            .with_context(|| format!("cannot read {:?}", after_path))?;
        Ok((before, after))
    }
}
