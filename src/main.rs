use efmt::format::Formatter;
use efmt::items::forms::Form;
use efmt::lex::Lexer;
use efmt::parse::Parser;
use erl_tokenize::Tokenizer;
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opt = Opt::from_args();

    let text = std::fs::read_to_string(&opt.file)?;
    let mut tokenizer = Tokenizer::new(text);
    tokenizer.set_filepath(opt.file);

    let mut lexer = Lexer::new(tokenizer);
    let mut parser = Parser::new(&mut lexer);
    let mut forms = Vec::new();
    while !parser.is_eof()? {
        let form: Form = parser.parse()?;
        forms.push(form);
    }

    let stdout = std::io::stdout();
    let formatter = Formatter::new(
        stdout.lock(),
        parser.text().to_owned(),
        parser.comments().clone(),
        parser.macros().clone(),
    );
    formatter.format_module(&forms)?;

    Ok(())
}
