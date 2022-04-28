use similar::TextDiff;
use std::path::Path;

pub fn text_diff<P: AsRef<Path>>(original: &str, formatted: &str, file: P) -> String {
    let diff = TextDiff::from_lines(original, formatted);
    if let Some(file) = file.as_ref().to_str() {
        diff.unified_diff()
            .header(&format!("a/{file}"), &format!("b/{file}"))
            .to_string()
    } else {
        diff.unified_diff().to_string()
    }
}
