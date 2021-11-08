pub mod format;
pub mod items;
pub mod lex;
pub mod parse;
pub mod span;

pub fn format_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<String> {
    let text = std::fs::read_to_string(&path)?;
    let mut tokenizer = erl_tokenize::Tokenizer::new(text);
    tokenizer.set_filepath(path);
    format(tokenizer)
}

pub fn format_text(text: &str) -> anyhow::Result<String> {
    format(erl_tokenize::Tokenizer::new(text.to_owned()))
}

fn format(tokenizer: erl_tokenize::Tokenizer<String>) -> anyhow::Result<String> {
    let mut lexer = crate::lex::Lexer::new(tokenizer);
    let mut parser = crate::parse::Parser::new(&mut lexer);
    let module: crate::items::module::Module = parser.parse()?;
    let mut formatter = crate::format::Formatter::new(
        parser.text().to_owned(),
        parser.comments().clone(),
        parser.macros().clone(),
    );
    formatter.format_item(&module)?;
    Ok(formatter.finish())
}
