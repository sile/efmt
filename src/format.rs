use self::region::RegionConfig;
use crate::parse::{Parse, TokenStream, TokenStreamOptions};
use crate::span::Span;
use std::path::Path;

pub use self::formatter::Formatter;
pub use efmt_derive::Format;

mod formatter;
pub mod region;

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

pub trait FormatWithStyle<Style>: Format {
    fn format_with_style(&self, fmt: &mut Formatter, style: &Style) -> Result<()>;
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    LineTooLong,

    #[error("unexpected multi-line")]
    MultiLine,

    #[error("todo")]
    MultiLineParent,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct RegionOptions<'a> {
    fmt: &'a mut Formatter,
    config: RegionConfig,
    check_trailing_columns: bool,
}

impl<'a> RegionOptions<'a> {
    pub fn new(fmt: &'a mut Formatter) -> Self {
        let config = fmt.region_config().clone();
        Self {
            fmt,
            config,
            check_trailing_columns: false,
        }
    }

    pub fn check_trailing_columns(mut self, b: bool) -> Self {
        self.check_trailing_columns = b;
        self
    }

    pub fn parent_indent(mut self) -> Self {
        self.config.indent = self.fmt.parent_indent();
        self
    }

    pub fn indent_offset(mut self, offset: usize) -> Self {
        self.config.indent += offset;
        self
    }

    pub fn current_column_as_indent(mut self) -> Self {
        self.config.indent = self.fmt.current_column();
        self
    }

    pub fn reset_trailing_columns(mut self, n: usize) -> Self {
        self.config.trailing_columns = n;
        self
    }

    pub fn clear_trailing_columns(mut self, b: bool) -> Self {
        if b {
            self.config.trailing_columns = 0;
        }
        self
    }

    // TODO: s/.../add_.../
    pub fn trailing_columns(mut self, n: usize) -> Self {
        self.config.trailing_columns += n;
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
        self.fmt.with_subregion(self.config, |fmt| {
            f(fmt)?;
            if self.check_trailing_columns {
                fmt.check_trailing_columns()?;
            }
            Ok(())
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
