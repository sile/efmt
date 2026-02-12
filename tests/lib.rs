use efmt_core::items::ModuleOrConfig;

type Result<T> = efmt::Result<T>;

#[test]
fn format_works() -> crate::Result<()> {
    for entry in std::fs::read_dir("tests/testdata/")? {
        let entry = entry?;
        let path = entry.path();
        if path
            .extension()
            .map_or(true, |ext| ext != "erl" && ext != "src" && ext != "escript")
        {
            continue;
        }
        let formatted = efmt::Options::new().format_file::<ModuleOrConfig, _>(&path)?;
        let expected = std::fs::read_to_string(&path)?;
        similar_asserts::assert_eq!(formatted, expected, "target={:?}", path);
    }
    Ok(())
}
