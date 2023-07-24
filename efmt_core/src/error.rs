use crate::span::Position;
use std::fmt::Write as _;
use std::path::Path;

pub fn generate_error_message<P: AsRef<Path>>(
    text: &str,
    path: Option<P>,
    position: Position,
    reason: &str,
) -> String {
    let line = position.line();
    let column = position.column();
    let file = path
        .as_ref()
        .and_then(|x| x.as_ref().to_str())
        .unwrap_or("<unknown>");
    let line_string = get_line_string(text, position);

    let mut m = String::new();
    writeln!(m, "\n--> {file}:{line}:{column}").expect("unreachable");
    writeln!(m, "{line} | {line_string}").expect("unreachable");
    write!(
        m,
        "{:line_width$} | {:>token_column$} {reason}",
        "",
        "^",
        line_width = line.to_string().len(),
        token_column = column
    )
    .expect("unreachable");
    m
}

fn get_line_string(text: &str, position: Position) -> &str {
    let offset = position.offset();
    let line_start = text[..offset].rfind('\n').unwrap_or(0);
    let line_end = text[offset..]
        .find('\n')
        .map(|x| x + offset)
        .unwrap_or_else(|| text.len());
    text[line_start..line_end].trim_matches(char::is_control)
}
