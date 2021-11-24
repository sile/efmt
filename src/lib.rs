use crate::format::{Format, Formatter};
use crate::parse::{Parse, TokenStream, TokenStreamOptions};
use std::path::Path;

pub mod format;
pub mod items;
pub mod parse;
pub mod span;

pub(crate) mod erl;

/// Formats an Erlang file with the default options.
pub fn format_file<T: Parse + Format, P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    Options::new().format_file::<T, P>(path)
}

/// Formats an Erlang text with the default options.
pub fn format_text<T: Parse + Format>(text: &str) -> anyhow::Result<String> {
    Options::new().format_text::<T>(text)
}

/// Options to format an item.
#[derive(Debug, Clone)]
pub struct Options {
    max_columns: usize,
    token_stream: TokenStreamOptions,
}

impl Default for Options {
    fn default() -> Self {
        Self {
            max_columns: Self::DEFAULT_MAX_COLUMNS,
            token_stream: Default::default(),
        }
    }
}

impl Options {
    /// The default max column number.
    pub const DEFAULT_MAX_COLUMNS: usize = 120;

    /// Makes an [Options] instance with the default settings.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn max_columns(mut self, n: usize) -> Self {
        self.max_columns = n;
        self
    }

    pub fn include_dirs<P: AsRef<Path>>(mut self, dirs: Vec<P>) -> Self {
        self.token_stream = self
            .token_stream
            .include_dirs(dirs.into_iter().map(|x| x.as_ref().to_path_buf()).collect());
        self
    }

    pub fn include_cache_dir<P: AsRef<Path>>(mut self, dir: P) -> Self {
        self.token_stream = self
            .token_stream
            .include_cache_dir(dir.as_ref().to_path_buf());
        self
    }

    pub fn format_file<T: Parse + Format, P: AsRef<Path>>(self, path: P) -> anyhow::Result<String> {
        let text = std::fs::read_to_string(&path)?;
        let mut tokenizer = erl_tokenize::Tokenizer::new(text);
        tokenizer.set_filepath(path);
        self.format::<T>(tokenizer)
    }

    pub fn format_text<T: Parse + Format>(self, text: &str) -> anyhow::Result<String> {
        let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
        self.format::<T>(tokenizer)
    }

    fn format<T: Parse + Format>(
        self,
        tokenizer: erl_tokenize::Tokenizer<String>,
    ) -> anyhow::Result<String> {
        let mut ts = TokenStream::new(tokenizer, self.token_stream);
        let item: T = ts.parse()?;
        let mut formatter = Formatter::new(ts);
        item.format(&mut formatter);
        let formatted_text = formatter.format(self.max_columns);
        Ok(formatted_text)
    }
}

#[cfg(test)]
#[macro_export]
macro_rules! assert_format {
    ($text:expr, $item_type:ident) => {{
        let formatted = crate::Options::new()
            .max_columns(20)
            .format_text::<$item_type>(&$text)
            .unwrap();
        let expected = $text;
        similar_asserts::assert_str_eq!(formatted, expected);
    }};
}
