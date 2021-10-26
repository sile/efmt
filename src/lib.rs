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

    pub fn test_parse_and_format<T: Parse + Format>(testname: &str) -> anyhow::Result<()> {
        let (text_path, text, expected_path, expected) =
            load_testdata(testname).with_context(|| "cannot load testdata")?;
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
            "unexpected formatted code.\n[ACTUAL] {}\n{}\n\n[EXPECTED] {}\n{}",
            text_path,
            formatted,
            expected_path,
            expected
        );
        Ok(())
    }

    pub fn load_testdata(testname: &str) -> anyhow::Result<(String, String, String, String)> {
        let before_path = format!("testdata/{}-before.erl", testname);
        let after_path = format!("testdata/{}-after.erl", testname);
        let before = std::fs::read_to_string(&before_path)
            .with_context(|| format!("cannot read {:?}", before_path))?;
        let after = std::fs::read_to_string(&after_path)
            .with_context(|| format!("cannot read {:?}", after_path))?;
        Ok((before_path, before, after_path, after))
    }
}
