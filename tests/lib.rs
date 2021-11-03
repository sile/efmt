use efmt::items::forms::Form;
use efmt::lex::Lexer;
use efmt::parse::Parser;
use erl_tokenize::Tokenizer;

#[test]
fn parse_works() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("tests/testdata/")? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().map_or(true, |ext| ext != "erl") {
            continue;
        }

        dbg!(&path);
        let text = std::fs::read_to_string(&path)?;
        let mut tokenizer = Tokenizer::new(text);
        tokenizer.set_filepath(path);

        let mut lexer = Lexer::new(tokenizer);
        let mut parser = Parser::new(&mut lexer);
        while !parser.is_eof()? {
            let _: Form = parser.parse()?;
        }
    }
    Ok(())
}
