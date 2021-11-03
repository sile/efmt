pub mod format;
pub mod items;
pub mod lex;
pub mod parse;
pub mod span;

// TODO: remove?
pub fn format_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<String> {
    use crate::items::forms::Form;
    use crate::lex::Lexer;
    use crate::parse::Parser;
    use erl_tokenize::Tokenizer;

    let text = std::fs::read_to_string(&path)?;
    let mut tokenizer = Tokenizer::new(text);
    tokenizer.set_filepath(path);

    let mut lexer = Lexer::new(tokenizer);
    let mut parser = Parser::new(&mut lexer);
    let mut forms: Vec<Form> = Vec::new();
    while !parser.is_eof()? {
        forms.push(parser.parse()?);
    }

    Ok(String::new())
}
