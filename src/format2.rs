use crate::items::macros::Macro;
use crate::items::tokens::CommentToken;
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
    items: Vec<Item>,
    text: Arc<String>,
    macros: Arc<BTreeMap<Position, Macro>>,
    comments: Arc<BTreeMap<Position, CommentToken>>,
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
            items: Vec::new(),
        }
    }

    pub fn add_item(&mut self, item: &impl Span) {
        // TODO: handle macro
        self.items.push(Item::Span {
            start_position: item.start_position(),
            end_position: item.end_position(),
        });
    }

    pub fn add_space(&mut self) {
        self.items.push(Item::Space);
    }

    pub fn add_newline(&mut self) {
        self.items.push(Item::Newline);
    }

    pub fn subregion<F>(&mut self, indent: Indent, newline: Newline, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let mut fmt = Self {
            items: Vec::new(),
            text: Arc::clone(&self.text),
            macros: Arc::clone(&self.macros),
            comments: Arc::clone(&self.comments),
        };
        f(&mut fmt);
        self.items.push(Item::Region {
            indent,
            newline,
            items: fmt.items,
        });
    }

    pub fn format(mut self, max_columns: usize) -> String {
        let items = std::mem::replace(&mut self.items, Vec::new());
        ItemToString::new(self, max_columns).to_string(&items)
    }
}

#[derive(Debug)]
struct ItemToString {
    buf: String,
    max_columns: usize,
    fmt: Formatter2,
}

impl ItemToString {
    fn new(fmt: Formatter2, max_columns: usize) -> Self {
        Self {
            buf: String::new(),
            max_columns,
            fmt,
        }
    }

    fn to_string(mut self, items: &[Item]) -> String {
        self.format_items(items);
        self.buf
    }

    fn format_items(&mut self, items: &[Item]) {
        for item in items {
            match item {
                Item::Span {
                    start_position,
                    end_position,
                } => self.format_item(*start_position, *end_position),
                Item::Space => self.format_space(),
                Item::Newline => self.format_newline(),
                Item::Region {
                    indent,
                    newline,
                    items,
                } => self.format_region(indent, newline, items),
            }
        }
    }

    fn format_space(&mut self) {
        if self.buf.chars().last() != Some(' ') {
            self.buf.push(' ');
        }
    }

    fn format_newline(&mut self) {
        if !matches!(self.buf.chars().last(), None | Some('\n')) {
            if self.buf.chars().last() == Some(' ') {
                self.buf.pop();
            }
            self.buf.push('\n');
        }
    }

    fn format_item(&mut self, start_position: Position, end_position: Position) {
        let start = start_position.offset();
        let end = end_position.offset();
        self.buf.push_str(&self.fmt.text[start..end]);
    }

    fn format_region(&mut self, indent: &Indent, newline: &Newline, items: &[Item]) {
        // TODO: indent and newline handlings
        self.format_items(items);
    }
}

#[derive(Debug)]
pub enum Item {
    Span {
        start_position: Position,
        end_position: Position,
    },
    Space,
    Newline,
    //NewlineIfMultiLineRegion,
    Region {
        indent: Indent,
        newline: Newline,
        items: Vec<Item>,
    },
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
    IfTooLong,
    IfMultiLine,
    IfMultiLineParent,
    Or(Vec<Self>),
}
