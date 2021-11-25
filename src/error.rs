use crate::span::Position;
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
        .unwrap_or_else(|| "<unknown>");
    let line_string = get_line_string(text, position);

    let mut m = String::new();
    m.push_str(&format!("\n--> {}:{}:{}\n", file, line, column));
    m.push_str(&format!("{} | {}\n", line, line_string));
    m.push_str(&format!(
        "{:line_width$} | {:>token_column$} {}",
        "",
        "^",
        reason,
        line_width = line.to_string().len(),
        token_column = column
    ));
    m
}

fn get_line_string(text: &str, position: Position) -> &str {
    let offset = position.offset();
    let line_start = (&text[..offset]).rfind('\n').unwrap_or(0);
    let line_end = (&text[offset..])
        .find('\n')
        .map(|x| x + offset)
        .unwrap_or_else(|| text.len());
    (&text[line_start..line_end]).trim_matches(char::is_control)
}
