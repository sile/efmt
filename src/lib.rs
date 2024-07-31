use efmt_core::format::{Format, Formatter};
use efmt_core::parse::{Parse, TokenStream, Tokenizer};
use std::path::Path;

pub mod diff;
pub mod files;

/// Formats an Erlang file with the default options.
pub fn format_file<T: Parse + Format, P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    Options::new().format_file::<T, P>(path)
}

/// Formats an Erlang text with the default options.
pub fn format_text<T: Parse + Format>(text: &str) -> anyhow::Result<String> {
    Options::new().format_text::<T>(text)
}

/// Options to format an item.
#[derive(Debug, Clone, Default)]
pub struct Options {
    default_off: bool,
}

impl Options {
    /// Makes an [Options] instance with the default settings.
    pub fn new() -> Self {
        Self::default()
    }

    pub fn default_off(mut self) -> Self {
        self.default_off = true;
        self
    }

    pub fn format_file<T: Parse + Format, P: AsRef<Path>>(self, path: P) -> anyhow::Result<String> {
        let text = std::fs::read_to_string(&path)?;
        let mut tokenizer = Tokenizer::new(text);
        tokenizer.set_filepath(path);
        self.format::<T>(tokenizer)
    }

    pub fn format_text<T: Parse + Format>(self, text: &str) -> anyhow::Result<String> {
        let tokenizer = Tokenizer::new(text.to_owned());
        self.format::<T>(tokenizer)
    }

    fn format<T: Parse + Format>(self, tokenizer: Tokenizer) -> anyhow::Result<String> {
        let mut ts = TokenStream::new(tokenizer);
        let item: T = ts.parse()?;
        let mut formatter = Formatter::new(ts);
        if self.default_off {
            formatter.skip_formatting();
        }
        item.format(&mut formatter);
        let formatted_text = formatter.finish();
        Ok(formatted_text)
    }
}
