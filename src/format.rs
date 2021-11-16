use self::region::RegionConfig;
use crate::parse::{Parse, TokenStream, TokenStreamOptions};
use crate::span::Span;
use std::path::{Path, PathBuf};

pub use self::formatter::Formatter;
pub use efmt_derive::Format;

mod formatter;
mod region;

pub trait Format: Span {
    fn format(&self, fmt: &mut Formatter) -> Result<()>;

    fn should_be_packed(&self) -> bool {
        false
    }
}

impl<T: Format> Format for Box<T> {
    fn format(&self, fmt: &mut Formatter) -> Result<()> {
        (**self).format(fmt)
    }

    fn should_be_packed(&self) -> bool {
        (**self).should_be_packed()
    }
}

impl<A: Format, B: Format> Format for (A, B) {
    fn format(&self, fmt: &mut Formatter) -> Result<()> {
        self.0.format(fmt)?;
        self.1.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    LineTooLong,

    #[error("unexpected multi-line")]
    MultiLine,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct RegionOptions<'a> {
    fmt: &'a mut Formatter,
    config: RegionConfig,
}

impl<'a> RegionOptions<'a> {
    pub fn new(fmt: &'a mut Formatter) -> Self {
        let config = fmt.region_config().clone();
        Self { fmt, config }
    }

    pub fn indent_offset(mut self, offset: usize) -> Self {
        self.config.indent += offset;
        self
    }

    pub fn current_column_as_indent(mut self) -> Self {
        self.config.indent = self.fmt.current_column();
        self
    }

    pub fn trailing_columns(mut self, n: usize) -> Self {
        self.config.max_columns = self.config.max_columns.saturating_sub(n);
        self
    }

    pub fn forbid_multi_line(mut self) -> Self {
        self.config.allow_multi_line = false;
        self
    }

    pub fn forbid_too_long_line(mut self) -> Self {
        self.config.allow_too_long_line = false;
        self
    }

    pub fn enter<F>(self, f: F) -> Result<()>
    where
        F: FnOnce(&mut Formatter) -> Result<()>,
    {
        self.fmt.with_subregion(self.config, f)
    }

    pub fn enter_with_newline<F>(self, f: F) -> Result<()>
    where
        F: FnOnce(&mut Formatter) -> Result<()>,
    {
        self.fmt.with_subregion(self.config, |fmt| {
            fmt.write_newline()?;
            f(fmt)
        })
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

    // TODO: delete
    pub fn include_dirs(mut self, dirs: Vec<PathBuf>) -> Self {
        self.token_stream = self.token_stream.include_dirs(dirs);
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
        let mut formatter = Formatter::new(
            ts.text().to_owned(),
            ts.comments().clone(),
            ts.macros().clone(),
            self.max_columns,
        );
        item.format(&mut formatter)?;
        let formatted_text = formatter.finish()?;
        Ok(formatted_text)
    }
}
