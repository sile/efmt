use crate::parse::{Parse, TokenStreamOptions};
use std::path::Path;

mod formatter;
pub mod region;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    LineTooLong,

    #[error("unexpected multi-line")]
    MultiLine,
}

pub type Result<T> = std::result::Result<T, Error>;

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

    pub fn format_file<T: Parse, P: AsRef<Path>>(self, path: P) -> anyhow::Result<String> {
        // let text = std::fs::read_to_string(&path)?;
        // let mut tokenizer = erl_tokenize::Tokenizer::new(text);
        // tokenizer.set_filepath(path);
        // self.format::<T>(tokenizer)
        todo!()
    }

    pub fn format_text<T: Parse>(self, text: &str) -> anyhow::Result<String> {
        // let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
        // self.format::<T>(tokenizer)
        todo!()
    }

    // fn format<T: Parse + Format>(
    //     self,
    //     tokenizer: erl_tokenize::Tokenizer<String>,
    // ) -> anyhow::Result<String> {
    //     let mut ts = TokenStream::new(tokenizer, self.token_stream);
    //     let item: T = ts.parse()?;
    //     let mut formatter = Formatter::new(
    //         ts.text().to_owned(),
    //         ts.comments().clone(),
    //         ts.macros().clone(),
    //         self.max_columns,
    //     );
    //     item.format(&mut formatter)?;
    //     let formatted_text = formatter.finish()?;
    //     Ok(formatted_text)
    // }
}
