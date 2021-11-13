use crate::items::macros::Macro;
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};
use std::collections::BTreeMap;

pub use self::transaction::{Transaction, TransactionConfig};
pub use efmt_derive::Format;

mod transaction;

pub trait Format: Span {
    fn format(&self, fmt: &mut Formatter) -> Result<()>;

    // TODO: rename (should packed?)
    fn is_primitive(&self) -> bool {
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
        fmt.format_item(&self.0)?;
        fmt.format_item(&self.1)?;
        Ok(())
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    MaxColumnsExceeded,

    #[error("unexpected multiline: {position:?}")]
    Multiline { position: Position },
}

pub type Result<T> = std::result::Result<T, Error>;

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

// TODO: delete
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

    pub fn finish(mut self) -> String {
        assert!(self.transaction.parent().is_none());

        let eof = crate::items::module::Eof {
            position: Position::new(usize::MAX, usize::MAX, usize::MAX),
        };
        self.write_comments_and_macros(&eof, None)
            .expect("TODO: error handling");

        self.transaction.finish().expect("TODO: error handling")
    }

    pub fn max_columns(&self) -> usize {
        self.transaction.config().max_columns
    }

    pub fn current_column(&self) -> usize {
        self.transaction.current_column()
    }

    // TODO: s/needs/write/
    pub fn needs_newline(&mut self) -> Result<()> {
        self.transaction.needs_whitespace(Whitespace::Newline)
    }

    // TODO: s/needs/write/
    pub fn needs_space(&mut self) -> Result<()> {
        self.transaction.needs_whitespace(Whitespace::Blank)
    }

    pub fn format_item(&mut self, item: &impl Format) -> Result<()> {
        self.write_comments_and_macros(item, Some(CommentKind::Trailing))?;
        item.format(self)
    }

    pub fn write_text(&mut self, item: &impl Span) -> Result<()> {
        self.write_comments_and_macros(item, Some(CommentKind::Post))?;
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
                this.needs_newline()?;
            }
            f(this)
        });

        match result {
            Err(Error::MaxColumnsExceeded) if !options.noretry => {
                // TODO: This assertion can be violated if `optoins.noentry = true`.
                assert!(!options.multiline_mode.is_recommended());
                self.with_subregion(options.recommend_multiline(), f)
            }
            Err(Error::Multiline { .. }) if options.multiline_mode == MultilineMode::Forbid => {
                assert!(!options.multiline_mode.is_recommended());
                self.with_subregion(options.recommend_multiline(), f)
            }
            _ => result,
        }
    }

    fn write_comment(&mut self, item: &CommentToken) -> Result<()> {
        self.transaction.write_comment(&self.text, item)?;
        Ok(())
    }

    fn write_macro(&mut self, item: &Macro) -> Result<()> {
        // TODO: Use `item.format()` to format the args.
        //       (But be careful to prevent infinite recursive call of the format method)
        self.transaction.write_item(&self.text, item)?;

        if !item.has_args() && self.text.as_bytes()[item.end_position().offset()] == b' ' {
            self.needs_space()?;
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
