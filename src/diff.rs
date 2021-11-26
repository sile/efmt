use console::{style, Style};
use similar::{ChangeTag, TextDiff};

pub fn text_diff(original: &str, formatted: &str) -> String {
    let mut output = String::new();
    let diff = TextDiff::from_lines(original, formatted);
    for (idx, group) in diff.grouped_ops(3).iter().enumerate() {
        if idx > 0 {
            output += &format!("{:-^1$}\n", "-", 80);
        }
        for op in group {
            for change in diff.iter_inline_changes(op) {
                let (sign, s) = match change.tag() {
                    ChangeTag::Delete => ("-", Style::new().red()),
                    ChangeTag::Insert => ("+", Style::new().green()),
                    ChangeTag::Equal => (" ", Style::new().dim()),
                };
                output += &format!(
                    "{}{} |{}",
                    style(Line(change.old_index())).dim(),
                    style(Line(change.new_index())).dim(),
                    s.apply_to(sign).bold(),
                );
                for (emphasized, value) in change.iter_strings_lossy() {
                    if emphasized {
                        output += &format!("{}", s.apply_to(value).underlined().on_black());
                    } else {
                        output += &format!("{}", s.apply_to(value));
                    }
                }
                if change.missing_newline() {
                    output.push('\n');
                }
            }
        }
    }
    output
}

struct Line(Option<usize>);

impl std::fmt::Display for Line {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self.0 {
            None => write!(f, "    "),
            Some(idx) => write!(f, "{:<4}", idx + 1),
        }
    }
}
