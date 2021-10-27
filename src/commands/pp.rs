// use crate::pp::Preprocessor;
// use std::fs::File;
// use std::io::Read as _;
use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt)]
pub struct PreprocessOpt {
    pub source_code_path: PathBuf,
}

impl PreprocessOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        todo!()
        // let mut buf = String::new();
        // File::open(&self.source_code_path)?.read_to_string(&mut buf)?;
        // let pp = Preprocessor::new(buf);
        // let preprocessed = pp.preprocess()?;
        // print!("{}", preprocessed);
        // Ok(())
    }
}
