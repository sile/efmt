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

        // TODO: Assume `*.erl` files are already well formatted.
        let mut formatted_path = path.clone();
        formatted_path.set_extension("erl.formatted");
        if !formatted_path.exists() {
            continue;
        }

        dbg!(&path);
        let formatted_text = efmt::format_file(&path)?;
        let expected = std::fs::read_to_string(&formatted_path)?;
        if formatted_text != expected {
            println!();
            println!("[Actual]\n{}\n", formatted_text);
            println!("[Expected]\n{}\n", expected);
            assert!(false);
        }
    }
    Ok(())
}
