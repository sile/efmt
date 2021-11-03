use efmt::format::Formatter;
use efmt::items::forms::Form;
use efmt::lex::Lexer;
use efmt::parse::Parser;
use erl_tokenize::Tokenizer;

// TODO
// #[test]
// fn parse_works() -> anyhow::Result<()> {
//     for entry in std::fs::read_dir("tests/testdata/")? {
//         let entry = entry?;
//         let path = entry.path();
//         if path.extension().map_or(true, |ext| ext != "erl") {
//             continue;
//         }

//         dbg!(&path);
//         let text = std::fs::read_to_string(&path)?;
//         let mut tokenizer = Tokenizer::new(text);
//         tokenizer.set_filepath(path);

//         let mut lexer = Lexer::new(tokenizer);
//         let mut parser = Parser::new(&mut lexer);
//         while !parser.is_eof()? {
//             let _: Form = parser.parse()?;
//         }
//     }
//     Ok(())
// }

#[test]
fn format_works() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("tests/testdata/")? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().map_or(true, |ext| ext != "erl") {
            continue;
        }

        let mut formatted_path = path.clone();
        formatted_path.set_extension("erl.formatted");
        if !formatted_path.exists() {
            continue;
        }

        dbg!(&path);
        let text = std::fs::read_to_string(&path)?;
        let mut tokenizer = Tokenizer::new(text);
        tokenizer.set_filepath(path);

        let mut lexer = Lexer::new(tokenizer);
        let mut parser = Parser::new(&mut lexer);
        let mut forms = Vec::new();
        while !parser.is_eof()? {
            let form: Form = parser.parse()?;
            forms.push(form);
        }

        let expected = std::fs::read_to_string(&formatted_path)?;
        let mut formatted_text = Vec::new();
        {
            let mut formatter = Formatter::new(
                &mut formatted_text,
                parser.text().to_owned(),
                parser.comments().clone(),
                parser.macros().clone(),
            );
            formatter.format_module(&forms)?;
        }
        let formatted_text = String::from_utf8(formatted_text)?;
        if formatted_text != expected {
            println!();
            println!("[Actual]\n{}\n", formatted_text);
            println!("[Expected]\n{}\n", expected);
            assert!(false);
        }
    }
    Ok(())
}
