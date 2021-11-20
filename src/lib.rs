pub mod erl;
pub mod format;
pub mod format2;
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

#[cfg(test)]
#[macro_export]
macro_rules! assert_format2 {
    ($text:expr, $item_type:ident) => {{
        use crate::format2::Format2 as _;

        let tokenizer = erl_tokenize::Tokenizer::new($text.to_owned());

        let mut ts = crate::parse::TokenStream::new(tokenizer, Default::default());
        let item: $item_type = ts.parse().expect("cannot parse");
        let mut formatter = crate::format2::Formatter2::new(
            ts.text().to_owned(),
            ts.macros().clone(),
            ts.comments().clone(),
        );
        item.format2(&mut formatter);
        let formatted = formatter.format(20);

        let expected = $text;
        similar_asserts::assert_str_eq!(formatted, expected);
    }};
}
