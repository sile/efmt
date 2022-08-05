use efmt::items::ModuleOrConfig;

#[test]
fn format_works() -> anyhow::Result<()> {
    for entry in std::fs::read_dir("tests/testdata/")? {
        let entry = entry?;
        let path = entry.path();
        if path
            .extension()
            .map_or(true, |ext| ext != "erl" && ext != "src")
        {
            continue;
        }
        let formatted = efmt::Options::new().format_file::<ModuleOrConfig, _>(&path)?;
        let expected = std::fs::read_to_string(&path)?;
        similar_asserts::assert_eq!(formatted, expected, "target={:?}", path);
    }
    Ok(())
}
