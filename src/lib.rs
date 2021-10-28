// pub mod commands;
pub mod cst;
pub mod format;
pub mod lex;
pub mod parse;
pub mod token;
pub mod tokenize;

#[cfg(test)]
mod tests {
    use crate::format::{Format, Formatter};
    use crate::lex::Lexer;
    use crate::parse::{Parse, Parser};
    use crate::tokenize::Tokenizer;
    use anyhow::Context;

    pub fn test_parse_and_format<T: Parse + Format>(testname: &str) -> anyhow::Result<()> {
        let (text_path, text, expected_path, expected) =
            load_testdata(testname).with_context(|| "cannot load testdata")?;
        let mut lexer = Lexer::new(Tokenizer::new(text.clone()));
        let mut parser = Parser::new(&mut lexer);
        let item = parser.parse::<T>().with_context(|| "cannot parse")?;

        let mut buf = Vec::new();
        let mut fmt = Formatter::new(&mut buf, lexer.finish());
        fmt.format(&item).with_context(|| "cannot format")?;

        let formatted = String::from_utf8_lossy(&buf);
        anyhow::ensure!(
            formatted == expected.trim(),
            "unexpected formatted code.\n[SOURCE] {}\n{}\n\n[FORMATTED] \n{}\n\n[EXPECTED] {}\n{}",
            text_path,
            text,
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
