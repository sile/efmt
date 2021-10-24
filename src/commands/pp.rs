use crate::pp::Preprocessor;
use erl_tokenize::Tokenizer;
use std::fs::File;
use std::io::Read as _;
use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt)]
#[structopt(rename_all = "kebab-case")]
pub struct PreprocessOpt {
    pub source_code_path: PathBuf,
}

impl PreprocessOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        let mut buf = String::new();
        File::open(&self.source_code_path)?.read_to_string(&mut buf)?;

        let tokenizer = Tokenizer::new(buf);
        let pp = Preprocessor::new(tokenizer);
        let preprocessed = pp.preprocess()?;

        let mut last_line = 0;
        for (i, token) in preprocessed.tokens.iter().enumerate() {
            let position = preprocessed.actual_position(i);
            if last_line != position.line() {
                println!();
                if last_line + 1 < position.line() {
                    println!();
                }
                last_line = position.line();
                for _ in 1..position.column() {
                    print!(" ");
                }
                print!("{}", token.text());
            } else {
                print!(" {}", token.text());
            }
        }
        println!();

        Ok(())
    }
}
