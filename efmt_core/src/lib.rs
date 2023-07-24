pub mod error;
pub mod format;
pub mod items;
pub mod parse;
pub mod span;

pub fn format_text<T: crate::parse::Parse + crate::format::Format>(
    text: &str,
) -> crate::parse::Result<String> {
    let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
    let mut ts = crate::parse::TokenStream::new(tokenizer);
    let item: T = ts.parse()?;
    let mut formatter = crate::format::Formatter::new(ts);
    item.format(&mut formatter);
    let formatted_text = formatter.finish();
    Ok(formatted_text)
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_format {
    ($text:expr, $item_type:ty) => {{
        let formatted = crate::format_text::<$item_type>(&$text).unwrap();
        let expected = $text;
        similar_asserts::assert_eq!(formatted, expected);
    }};

    ($text:expr, $expected:expr, $item_type:ty) => {{
        let formatted = crate::format_text::<$item_type>(&$text).unwrap();
        similar_asserts::assert_eq!(formatted, $expected);
    }};
}
