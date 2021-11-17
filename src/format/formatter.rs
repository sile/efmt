use crate::format::region::{RegionConfig, RegionWriter};
use crate::format::{Error, Format as _, RegionOptions, Result};
use crate::items::macros::Macro;
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};
use std::collections::{BTreeMap, HashSet};

#[derive(Debug)]
pub struct Formatter {
    writer: RegionWriter,
    text: String,
    macros: BTreeMap<Position, Macro>,
    comments: BTreeMap<Position, CommentToken>,
    formatting_macros: HashSet<Position>,
}

impl Formatter {
    pub fn new(
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
        max_columns: usize,
    ) -> Self {
        Self {
            writer: RegionWriter::new(max_columns),
            text,
            macros,
            comments,
            formatting_macros: HashSet::new(),
        }
    }

    pub fn finish(mut self) -> Result<String> {
        let eof = Position::new(usize::MAX, usize::MAX, usize::MAX);
        self.write_comments_and_macros(&eof, None)?;
        Ok(self.writer.formatted_text().to_owned())
    }

    pub fn check_trailing_columns(&self) -> Result<()> {
        let too_long = self.current_column() + self.region_config().trailing_columns
            > self.region_config().max_columns;
        if too_long && !self.region_config().allow_too_long_line {
            return Err(Error::LineTooLong);
        }
        Ok(())
    }

    pub fn current_column(&self) -> usize {
        if self.writer.last_char() == '\n' {
            self.writer.config().indent
        } else {
            self.writer.current_column()
        }
    }

    pub fn current_relative_column(&self) -> usize {
        self.current_column() - self.writer.config().indent
    }

    pub fn parent_indent(&self) -> usize {
        self.writer.parent_indent()
    }

    pub fn region_config(&self) -> &RegionConfig {
        self.writer.config()
    }

    pub fn last_char(&self) -> char {
        self.writer.last_char()
    }

    pub fn is_multi_line_allowed(&self) -> bool {
        self.writer.config().allow_multi_line
    }

    pub fn subregion(&mut self) -> RegionOptions {
        RegionOptions::new(self)
    }

    pub fn write_newline(&mut self) -> Result<()> {
        self.writer.write_newline()?;
        Ok(())
    }

    pub fn write_space(&mut self) -> Result<()> {
        self.writer.write_space()?;
        Ok(())
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

    pub(super) fn with_subregion<F>(&mut self, config: RegionConfig, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.writer.start_subregion(config.clone());
        let result = f(self);
        if result.is_ok() {
            self.writer.commit_subregion();
        } else {
            self.writer.abort_subregion();
        }

        // Bug check.
        match result {
            Err(Error::LineTooLong) => {
                assert!(!config.allow_too_long_line);
            }
            Err(Error::MultiLine) => {
                assert!(!config.allow_multi_line);
            }
            Ok(()) => {}
        }

        result
    }

    fn write_macro(&mut self, item: &Macro) -> Result<()> {
        assert!(!self.formatting_macros.contains(&item.start_position()));

        self.formatting_macros.insert(item.start_position());
        let result = (|| {
            let start_offset = item.start_position().offset();
            if matches!(
                self.text.as_bytes().get(start_offset.saturating_sub(1)),
                Some(b' ')
            ) {
                self.write_space()?;
            }

            item.format(self)?;

            let end_offset = item.end_position().offset();
            if matches!(self.text.as_bytes().get(end_offset), Some(b' ' | b'\n')) {
                self.write_space()?;
            }
            Ok(())
        })();
        self.formatting_macros.remove(&item.start_position());
        result
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
            .filter(|x| !self.formatting_macros.contains(x))
            .copied()
            .next()
    }

    fn next_position(&self) -> Position {
        self.writer.next_position()
    }
}
