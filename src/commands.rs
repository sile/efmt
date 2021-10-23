use crate::formatter::{Context, Formatter};
use crate::parser::Parser;
use crate::pp::Preprocessor;
use erl_tokenize::Tokenizer;
use std::fs::File;
use std::io::Read as _;
use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt)]
pub struct FormatOpt {
    pub source_code_path: PathBuf,
}

impl FormatOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        let mut buf = String::new();
        File::open(&self.source_code_path)?.read_to_string(&mut buf)?;

        let pp = Preprocessor::new(Tokenizer::new(buf));
        let preprocessed = pp.preprocess()?;
        let parser = Parser::new(preprocessed.tokens);
        let context = Context::new(preprocessed.comments, preprocessed.macro_calls);
        for ast in parser {
            let stdout = std::io::stdout();
            let formatter = Formatter::new(ast?, context.clone(), stdout.lock());
            formatter.format()?;
        }
        Ok(())
    }
}

#[derive(Debug, structopt::StructOpt)]
#[structopt(rename_all = "kebab-case")]
pub struct ParseOpt {
    pub source_code_path: PathBuf,
}

impl ParseOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        let mut buf = String::new();
        File::open(&self.source_code_path)?.read_to_string(&mut buf)?;

        let pp = Preprocessor::new(Tokenizer::new(buf));
        let preprocessed = pp.preprocess()?;
        let parser = Parser::new(preprocessed.tokens);
        for ast in parser {
            println!("{:?}", ast?);
        }
        Ok(())
    }
}

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
        for token in pp.preprocess()?.tokens {
            println!("{:?}", token);
        }
        Ok(())
    }
}
