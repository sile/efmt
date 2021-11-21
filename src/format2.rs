use crate::format::region::{RegionConfig, RegionWriter};
use crate::format::{Error, Result};
use crate::items::macros::Macro;
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};
use std::collections::BTreeMap;
use std::sync::Arc;

pub use efmt_derive::Format2;

pub trait Format2: Span {
    fn format2(&self, fmt: &mut Formatter2);
}

impl<T: Format2> Format2 for Box<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        (**self).format2(fmt);
    }
}

impl<A: Format2, B: Format2> Format2 for (A, B) {
    fn format2(&self, fmt: &mut Formatter2) {
        self.0.format2(fmt);
        self.1.format2(fmt);
    }
}

#[derive(Debug)]
pub struct Formatter2 {
    item: Item,
    text: Arc<String>,
    macros: Arc<BTreeMap<Position, Macro>>,
    comments: Arc<BTreeMap<Position, CommentToken>>,
    next_position: Position,
}

impl Formatter2 {
    pub fn new(
        text: String,
        macros: BTreeMap<Position, Macro>,
        comments: BTreeMap<Position, CommentToken>,
    ) -> Self {
        Self {
            text: Arc::new(text),
            macros: Arc::new(macros),
            comments: Arc::new(comments),
            item: Item::new(),
            next_position: Position::new(0, 0, 0),
        }
    }

    // TODO: s/add_item/add_text/
    pub fn add_text(&mut self, text: &impl Span) {
        self.add_macros_and_comments(text.start_position());
        self.item.add_item(text);

        assert!(self.next_position <= text.end_position());
        self.next_position = text.end_position();
    }

    fn add_macros_and_comments(&mut self, next_position: Position) {
        // TODO: handle macro
        loop {
            let next_comment_start = self.next_comment_start();
            if next_position < next_comment_start {
                break;
            }
            let comment = self.comments[&next_comment_start].clone();
            self.add_comment(&comment);
        }
    }

    fn add_comment(&mut self, comment: &CommentToken) {
        match comment.kind() {
            CommentKind::Trailing => {
                self.item.add_space(2);
            }
            CommentKind::Post => {
                self.add_newline();
            }
        }
        self.item.add_item(comment);
        self.add_newline();

        assert!(self.next_position <= comment.end_position());
        self.next_position = comment.end_position();
    }

    fn next_comment_start(&self) -> Position {
        self.comments
            .range(self.next_position..)
            .next()
            .map(|(k, _)| *k)
            .unwrap_or_else(|| Position::new(usize::MAX, usize::MAX, usize::MAX))
    }

    pub fn add_space(&mut self) {
        self.item.add_space(1);
    }

    pub fn add_newline(&mut self) {
        self.item.add_newline(1);
    }

    pub fn subregion<F>(&mut self, indent: Indent, newline: Newline, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let parent = std::mem::replace(
            &mut self.item,
            Item::Region {
                indent,
                newline,
                items: Vec::new(),
            },
        );
        f(self);
        let child = std::mem::replace(&mut self.item, parent);
        self.item.add_region(child);
    }

    pub fn format(mut self, max_columns: usize) -> String {
        let item = std::mem::replace(&mut self.item, Item::new());
        ItemToString::new(self, max_columns).to_string(&item)
    }
}

#[derive(Debug)]
struct ItemToString {
    writer: RegionWriter,
    max_columns: usize,
    fmt: Formatter2,
}

impl ItemToString {
    fn new(fmt: Formatter2, max_columns: usize) -> Self {
        Self {
            writer: RegionWriter::new(max_columns),
            max_columns,
            fmt,
        }
    }

    fn to_string(mut self, item: &Item) -> String {
        self.format_item(item).expect("TODO: bug");
        self.writer.formatted_text().to_owned()
    }

    fn format_item(&mut self, item: &Item) -> Result<()> {
        match item {
            Item::Span {
                start_position,
                end_position,
            } => self.format_text(*start_position, *end_position)?,
            Item::Space(n) => self.format_space(*n)?,
            Item::Newline(n) => self.format_newline(*n)?,
            Item::Region {
                indent,
                newline,
                items,
            } => self.format_region(indent, newline, items)?,
        }
        Ok(())
    }

    fn format_items(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            self.format_item(item)?;
        }
        Ok(())
    }

    fn format_space(&mut self, n: usize) -> Result<()> {
        assert_eq!(n, 1);
        self.writer.write_space()
    }

    fn format_newline(&mut self, n: usize) -> Result<()> {
        assert_eq!(n, 1);
        self.writer.write_newline()
    }

    fn format_text(&mut self, start_position: Position, end_position: Position) -> Result<()> {
        self.writer
            .write_item(&self.fmt.text, &(start_position, end_position))
    }

    fn format_region(&mut self, indent: &Indent, newline: &Newline, items: &[Item]) -> Result<()> {
        let indent = match indent {
            Indent::Inherit => self.writer.config().indent,
            Indent::Offset(n) => self.writer.config().indent + n,
            Indent::ParentOffset(n) => self.writer.parent_indent() + n,
            Indent::CurrentColumn => {
                if self.writer.current_column() == 0 {
                    self.writer.config().indent
                } else {
                    self.writer.current_column()
                }
            }
        };
        let mut needs_newline = false;
        let mut allow_multi_line = true;
        let mut allow_too_long_line = true;
        let mut check_multi_line_parent = false;
        match newline {
            Newline::Always => {
                needs_newline = true;
            }
            Newline::Never => {}
            Newline::If(cond) => {
                allow_multi_line = !cond.multi_line;
                allow_too_long_line = !cond.too_long;
                if cond.multi_line_parent {
                    if self.writer.config().multi_line_mode {
                        needs_newline = true;
                    } else {
                        check_multi_line_parent = true;
                        allow_multi_line = false;
                    }
                }
            }
        };

        let config = RegionConfig {
            max_columns: self.max_columns,
            indent,
            trailing_columns: 0, // TODO: DELETE
            allow_multi_line,
            allow_too_long_line,
            multi_line_mode: false,
        };
        self.writer.start_subregion(config);
        if needs_newline {
            self.writer.write_newline()?;
        }
        let mut result = self.format_items(items);
        if result.is_ok() {
            self.writer.commit_subregion();
        } else {
            self.writer.abort_subregion();

            let (retry, needs_newline, multi_line_mode) = match &result {
                Err(_) if check_multi_line_parent => {
                    return Err(Error::MultiLineParent);
                }
                Err(Error::MultiLine) if !allow_multi_line => (true, true, false),
                Err(Error::LineTooLong) if !allow_too_long_line => (true, true, false),
                Err(Error::MultiLineParent) => (true, needs_newline, true),
                _ => (false, false, false),
            };
            if retry {
                let config = RegionConfig {
                    max_columns: self.max_columns,
                    indent,
                    trailing_columns: 0, // TODO: DELETE
                    allow_multi_line: true,
                    allow_too_long_line: true,
                    multi_line_mode,
                };
                self.writer.start_subregion(config);
                if needs_newline {
                    self.writer.write_newline()?;
                }
                result = self.format_items(items);
                if result.is_ok() {
                    self.writer.commit_subregion();
                } else {
                    self.writer.abort_subregion();
                }
            }
        }
        result
    }
}

#[derive(Debug)]
pub enum Item {
    Span {
        start_position: Position,
        end_position: Position,
    },
    Space(usize),
    Newline(usize),
    Region {
        indent: Indent,
        newline: Newline,
        items: Vec<Item>,
    },
}

impl Item {
    fn new() -> Self {
        Self::Region {
            indent: Indent::CurrentColumn,
            newline: Newline::Never,
            items: Vec::new(),
        }
    }

    fn add_item(&mut self, item: &impl Span) {
        if let Self::Region { items, .. } = self {
            if let Some(last @ Self::Region { .. }) = items.last_mut() {
                last.add_item(item);
            } else {
                items.push(Self::Span {
                    start_position: item.start_position(),
                    end_position: item.end_position(),
                });
            }
        } else {
            unreachable!();
        }
    }

    fn add_region(&mut self, region: Item) {
        if let Self::Region { items, .. } = self {
            if let Some(last @ Self::Region { .. }) = items.last_mut() {
                last.add_region(region);
            } else {
                items.push(region);
            }
        } else {
            unreachable!();
        }
    }

    fn add_space(&mut self, n: usize) {
        if let Self::Region { items, .. } = self {
            items.push(Self::Space(n));
        } else {
            unreachable!();
        }
    }

    fn add_newline(&mut self, n: usize) {
        if let Self::Region { items, .. } = self {
            items.push(Self::Newline(n));
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Indent {
    CurrentColumn,
    Inherit,
    Offset(usize),
    ParentOffset(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Newline {
    Always,
    Never,
    If(NewlineIf),
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NewlineIf {
    pub too_long: bool,
    pub multi_line: bool,
    pub multi_line_parent: bool,
}
