use crate::parser::Parser;
use std::fs::File;
use std::io::Read as _;
use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt)]
#[structopt(rename_all = "kebab-case")]
pub struct ParseOpt {
    pub source_code_path: PathBuf,
}

impl ParseOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        let mut buf = String::new();
        File::open(&self.source_code_path)?.read_to_string(&mut buf)?;
        let parser = Parser::new(buf);
        for ast in parser {
            println!("{:?}", ast?);
        }
        Ok(())
    }
}
