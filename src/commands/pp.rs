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

        let mut tokenizer = Tokenizer::new(buf);
        tokenizer.set_filepath(&self.source_code_path);
        let pp = Preprocessor::new(tokenizer);
        let preprocessed = pp.preprocess()?;
        print!("{}", preprocessed);
        Ok(())
    }
}
