use colored::Colorize;
use similar::{ChangeTag, TextDiff};
use std::path::Path;

pub fn text_color_diff<P: AsRef<Path>>(original: &str, formatted: &str, file: P) {
    let diff = TextDiff::from_lines(original, formatted);
    let file = file.as_ref().display();

    println!("--- {}", format!("a/{file}").bold());
    println!("+++ {}", format!("b/{file}").bold());
    for hunk in diff.unified_diff().iter_hunks() {
        println!("{}", hunk.header().to_string().cyan());
        for change in hunk.iter_changes() {
            match change.tag() {
                ChangeTag::Equal => print!("{}", format!(" {}", change.to_string())),
                ChangeTag::Delete => print!("{}", format!("-{}", change.to_string()).red()),
                ChangeTag::Insert => print!("{}", format!("+{}", change.to_string()).green()),
            }
        }
    }
    println!();
}

pub fn text_diff<P: AsRef<Path>>(original: &str, formatted: &str, file: P) {
    let diff = TextDiff::from_lines(original, formatted);
    let file = file.as_ref().display();
    println!(
        "{}",
        diff.unified_diff()
            .header(&format!("a/{file}"), &format!("b/{file}"))
            .to_string()
    );
}
