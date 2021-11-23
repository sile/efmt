use crate::parse::{Parse, TokenStream, TokenStreamOptions};
use crate::span::Span;
use std::path::Path;

mod formatter;
pub mod region; // s/region/writer/

pub use self::formatter::{Formatter, Indent, Newline, NewlineIf};
pub use efmt_derive::Format;

pub trait Format: Span {
    fn format(&self, fmt: &mut Formatter);
}

impl<T: Format> Format for Box<T> {
    fn format(&self, fmt: &mut Formatter) {
        (**self).format(fmt);
    }
}

impl<A: Format, B: Format> Format for (A, B) {
    fn format(&self, fmt: &mut Formatter) {
        self.0.format(fmt);
        self.1.format(fmt);
    }
}

#[derive(Debug, Clone)]
pub struct FormatOptions {
    max_columns: usize,
    token_stream: TokenStreamOptions,
}

impl Default for FormatOptions {
    fn default() -> Self {
        Self {
            max_columns: Self::DEFAULT_MAX_COLUMNS,
            token_stream: Default::default(),
        }
    }
}

impl FormatOptions {
    pub const DEFAULT_MAX_COLUMNS: usize = 120;

    pub fn new() -> Self {
        Self::default()
    }

    pub fn max_columns(mut self, n: usize) -> Self {
        self.max_columns = n;
        self
    }

    pub fn token_stream(mut self, options: TokenStreamOptions) -> Self {
        self.token_stream = options;
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
