use self::region::{RegionConfig, RegionWriter};
use crate::items::macros::Macro;
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};
use std::collections::BTreeMap;

pub use efmt_derive::Format;

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
    MaxColumnsExceeded,

    #[error("unexpected multiline")]
    Multiline, // TODO: s/Multiline/ForbiddenMultiLine
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct RegionOptions<'a> {
    fmt: &'a mut Formatter,
    config: RegionConfig,
}

impl<'a> RegionOptions<'a> {
    pub fn new(fmt: &'a mut Formatter) -> Self {
        let config = fmt.writer.config().clone();
        Self { fmt, config }
    }

    pub fn indent_offset(mut self, offset: usize) -> Self {
        self.config.indent += offset;
        self
    }

    pub fn current_column_as_indent(mut self) -> Self {
        self.config.indent = if self.fmt.writer.last_char() == '\n' {
            self.fmt.writer.config().indent
        } else {
            self.fmt.current_column()
        };
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
        self.fmt.writer.start_subregion(self.config);
        let result = f(self.fmt);
        if result.is_ok() {
            self.fmt.writer.commit_subregion();
        } else {
            self.fmt.writer.abort_subregion();
        }
        result
    }
}

#[derive(Debug)]
pub struct Formatter {
    writer: RegionWriter,
    text: String,
    macros: BTreeMap<Position, Macro>,
    comments: BTreeMap<Position, CommentToken>,
}

impl Formatter {
    pub fn new<T>(
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
        options: &crate::FormatOptions<T>,
    ) -> Self {
        Self {
            writer: RegionWriter::new(options.max_columns),
            text,
            macros,
            comments,
        }
    }

    pub fn finish(mut self) -> String {
        let eof = crate::items::module::Eof {
            position: Position::new(usize::MAX, usize::MAX, usize::MAX),
        };
        self.write_comments_and_macros(&eof, None)
            .expect("TODO: error handling");

        self.writer.formatted_text().to_owned()
    }

    pub fn max_columns(&self) -> usize {
        self.writer.config().max_columns
    }

    pub fn current_column(&self) -> usize {
        self.writer.current_column()
    }

    pub fn indent(&self) -> usize {
        self.writer.config().indent
    }

    pub fn write_newline(&mut self) -> Result<()> {
        self.writer.write_newline()
    }

    pub fn write_blank(&mut self) -> Result<()> {
        self.writer.write_blank()
    }

    pub fn is_multi_line_allowed(&self) -> bool {
        self.writer.config().allow_multi_line
    }

    pub fn write_text(&mut self, item: &impl Span) -> Result<()> {
        self.write_comments_and_macros(item, Some(CommentKind::Trailing))?;
        self.write_comments_and_macros(item, Some(CommentKind::Post))?;
        self.writer.write_item(&self.text, item)?;
        Ok(())
    }

    fn write_comment(&mut self, item: &CommentToken) -> Result<()> {
        self.writer.write_comment(&self.text, item)?;
        Ok(())
    }

    pub fn subregion(&mut self) -> RegionOptions {
        RegionOptions::new(self)
    }

    fn write_macro(&mut self, item: &Macro) -> Result<()> {
        // TODO: Use `item.format()` to format the args.
        //       (But be careful to prevent infinite recursive call of the format method)
        self.writer.write_item(&self.text, item)?;

        if !item.has_args() && self.text.as_bytes()[item.end_position().offset()] == b' ' {
            self.write_blank()?;
        }
        Ok(())
    }

    fn write_comments_and_macros(
        &mut self,
        next_item: &impl Span,
        allowed_comment_kind: Option<CommentKind>,
    ) -> Result<()> {
        let item_start = next_item.start_position();
        loop {
            let comment_start = self.next_comment_position();
            let macro_start = self.next_macro_position();
            if comment_start.map_or(true, |p| item_start < p)
                && macro_start.map_or(true, |p| item_start < p)
            {
                break;
            }

            if comment_start.map_or(false, |c| macro_start.map_or(true, |m| c < m)) {
                let comment = self.comments[&comment_start.unwrap()].clone();
                if allowed_comment_kind.map_or(true, |k| k == comment.kind()) {
                    self.write_comment(&comment)?;
                } else {
                    break;
                }
            } else {
                let macro_call = self.macros[&macro_start.unwrap()].clone();
                self.write_macro(&macro_call)?;
            }
        }
        Ok(())
    }

    fn next_comment_position(&self) -> Option<Position> {
        self.comments
            .range(self.next_position()..)
            .map(|x| x.0)
            .copied()
            .next()
    }

    fn next_macro_position(&self) -> Option<Position> {
        self.macros
            .range(self.next_position()..)
            .map(|x| x.0)
            .copied()
            .next()
    }

    fn next_position(&self) -> Position {
        self.writer.next_position()
    }
}
