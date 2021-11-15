pub mod erl;
pub mod format;
pub mod items;
pub mod parse;
pub mod span;

#[cfg(test)]
#[macro_export]
macro_rules! assert_format {
    ($text:expr, $item_type:ident) => {{
        use crate::format::FormatOptions;
        let formatted = FormatOptions::new()
            .max_columns(20)
            .format_text::<$item_type>($text)
            .unwrap_or_else(|e| format!("parse or format failed: {}", e));
        let expected = $text;
        similar_asserts::assert_str_eq!(formatted, expected);
    }};
}
