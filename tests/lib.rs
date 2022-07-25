use efmt::items::Module;

#[test]
fn format_works() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("tests/testdata/")? {
        let entry = entry?;
        let path = entry.path();
        if path.extension().map_or(true, |ext| ext != "erl") {
            continue;
        }
        let formatted = efmt::Options::new().format_file::<Module, _>(&path)?;
        let expected = std::fs::read_to_string(&path)?;
        similar_asserts::assert_str_eq!(formatted, expected, "target={:?}", path);
    }
    Ok(())
}
