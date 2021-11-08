use crate::items::macros::Macro;
use crate::items::tokens::CommentToken;
use crate::span::{Position, Span};
use std::collections::BTreeMap;

pub use self::transaction::{Transaction, TransactionConfig};
pub use efmt_derive::Format;

mod transaction;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MultilineMode {
    Forbid,
    Allow,
    Recommend,
}

impl Default for MultilineMode {
    fn default() -> Self {
        Self::Allow
    }
}

impl MultilineMode {
    pub fn is_recommended(self) -> bool {
        matches!(self, MultilineMode::Recommend)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Whitespace {
    Blank,
    Newline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IndentMode {
    CurrentIndent,
    CurrentColumn,
    Offset(usize),
}

impl Default for IndentMode {
    fn default() -> Self {
        Self::CurrentIndent
    }
}

#[derive(Debug, Clone, Default)]
pub struct RegionOptions {
    newline: bool,
    multiline_mode: MultilineMode,
    indent: IndentMode,
    trailing_item_size: usize,
    noretry: bool,
}

impl RegionOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn noretry(mut self) -> Self {
        self.noretry = true;
        self
    }

    pub fn newline(mut self) -> Self {
        self.newline = true;
        self
    }

    pub fn allow_multiline(mut self) -> Self {
        self.multiline_mode = MultilineMode::Allow;
        self
    }

    pub fn recommend_multiline(mut self) -> Self {
        self.multiline_mode = MultilineMode::Recommend;
        self
    }

    pub fn forbid_multiline(mut self) -> Self {
        self.multiline_mode = MultilineMode::Forbid;
        self
    }

    pub fn indent(mut self, x: IndentMode) -> Self {
        self.indent = x;
        self
    }

    pub fn trailing_item_size(mut self, n: usize) -> Self {
        self.trailing_item_size = n;
        self
    }

    fn to_transaction_config(&self, fmt: &Formatter) -> TransactionConfig {
        TransactionConfig {
            indent: self.indent,
            max_columns: fmt
                .max_columns()
                .checked_sub(self.trailing_item_size)
                .expect("TODO"),
            multiline_mode: self.multiline_mode,
        }
    }
}

pub trait Format: Span {
    fn format(&self, fmt: &mut Formatter) -> Result<()>;

    // TODO: rename (should packed?)
    fn is_primitive(&self) -> bool {
        false
    }
}

impl<A: Format, B: Format> Format for (A, B) {
    fn format(&self, fmt: &mut Formatter) -> Result<()> {
        fmt.format_item(&self.0)?;
        fmt.format_item(&self.1)?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    MaxColumnsExceeded,

    #[error("unexpected multiline")]
    Multiline,

    #[error(transparent)]
    Int(#[from] std::num::ParseIntError),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Formatter {
    transaction: Transaction,
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
            transaction: Transaction::root(options.max_columns),
            text,
            macros,
            comments,
        }
    }

    pub fn finish(self) -> String {
        assert!(self.transaction.parent().is_none());
        self.transaction.formatted_text().to_owned()
    }

    // pub fn indent(&self) -> usize {
    //     self.transaction.config().indent
    // }

    pub fn max_columns(&self) -> usize {
        self.transaction.config().max_columns
    }

    pub fn current_column(&self) -> usize {
        self.transaction.current_column()
    }

    pub fn needs_newline(&mut self) {
        self.transaction.needs_whitespace(Whitespace::Newline);
    }

    pub fn needs_space(&mut self) {
        self.transaction.needs_whitespace(Whitespace::Blank);
    }

    pub fn format_item(&mut self, item: &impl Format) -> Result<()> {
        self.write_comments(item)?;
        item.format(self)
    }

    pub fn write_text(&mut self, item: &impl Span) -> Result<()> {
        // TODO: handle empty macros
        self.write_comments(item)?;
        self.transaction.write_item(&self.text, item)?;
        Ok(())
    }

    pub fn multiline_mode(&self) -> MultilineMode {
        self.transaction.config().multiline_mode
    }

    pub fn with_subregion<F>(&mut self, options: RegionOptions, f: F) -> Result<()>
    where
        F: Fn(&mut Self) -> Result<()>,
    {
        let config = options.to_transaction_config(self);
        let result = self.with_transaction(config, |this| {
            if options.newline {
                this.needs_newline();
            }
            f(this)
        });

        if !options.noretry && matches!(result, Err(Error::MaxColumnsExceeded)) {
            // TODO: This assertion can be violated if `optoins.noentry = true`.
            assert!(!options.multiline_mode.is_recommended());
            self.with_subregion(options.recommend_multiline(), f)
        } else {
            result
        }
    }

    fn write_comment(&mut self, item: &impl Span) -> Result<()> {
        self.transaction.write_comment(&self.text, item)?;
        Ok(())
    }

    fn write_comments(&mut self, next_item: &impl Span) -> Result<()> {
        while let Some(comment_start) = self.next_comment_position() {
            if next_item.start_position() < comment_start {
                break;
            }

            let comment = self.comments[&comment_start].clone();
            self.write_comment(&comment)?;
        }
        Ok(())
    }

    fn next_comment_position(&self) -> Option<Position> {
        self.comments
            .range(self.next_position()..)
            .map(|x| x.0.clone())
            .next()
    }

    fn next_position(&self) -> Position {
        self.transaction.next_position()
    }

    fn with_transaction<F>(&mut self, config: TransactionConfig, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.transaction.start_new_transaction(config);
        let result = f(self);
        if result.is_ok() {
            self.transaction.commit();
        } else {
            self.transaction.abort();
        }
        result
    }
}
