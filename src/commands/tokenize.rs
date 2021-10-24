use erl_tokenize::{Lexer, PositionRange};
use std::fs::File;
use std::io::Read as _;
use std::path::PathBuf;

#[derive(Debug, structopt::StructOpt)]
pub struct TokenizeOpt {
    source_code_path: PathBuf,
}

impl TokenizeOpt {
    pub fn run(&self) -> anyhow::Result<()> {
        let mut buf = String::new();
        File::open(&self.source_code_path)?.read_to_string(&mut buf)?;

        let mut lexer = Lexer::new(buf);
        lexer.set_filepath(&self.source_code_path);

        let mut last_line = 0;
        for token in lexer {
            let token = token?;
            if last_line != token.start_position().line() {
                println!();
                if last_line + 1 < token.start_position().line() {
                    println!();
                }
                last_line = token.start_position().line();
                for _ in 1..token.start_position().column() {
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
