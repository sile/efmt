pub mod format;
pub mod items;
pub mod parse;
pub mod span;

pub(crate) mod erl;

#[cfg(test)]
#[macro_export]
macro_rules! assert_format {
    ($text:expr, $item_type:ident) => {{
        use crate::format::Format as _;

        let tokenizer = erl_tokenize::Tokenizer::new($text.to_owned());

        let mut ts = crate::parse::TokenStream::new(tokenizer, Default::default());
        let item: $item_type = ts.parse().expect("cannot parse");
        let mut formatter = crate::format::Formatter::new(ts);
        item.format(&mut formatter);
        let formatted = formatter.format(20);

        let expected = $text;
        similar_asserts::assert_str_eq!(formatted, expected);
    }};
}
