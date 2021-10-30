use efmt::cst::root::RootItems;
use efmt::format::Formatter;
use efmt::lex::Lexer;
use efmt::parse::Parser;
use efmt::tokenize::Tokenizer;
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
    tokenizer.set_filepath(&opt.file);

    let mut lexer = Lexer::new(tokenizer);
    let mut parser = Parser::new(&mut lexer);
    let items: RootItems = parser.parse()?;

    let stdout = std::io::stdout();
    let mut fmt = Formatter::new(stdout.lock(), lexer.finish());
    fmt.format(&items)?;
    fmt.finish()?;

    Ok(())
}
