use crate::items::forms::Form;
use crate::items::macros::Macro;
use crate::items::tokens::CommentToken;
use crate::span::{Position, Span};
use std::collections::BTreeMap;

pub use efmt_derive::Format;

// TODO: delete
// mod writers;

#[derive(Debug, Clone, Default)]
pub struct ChildOptions {
    newline: bool,
    multiline_mode: bool,
    forbid_multiline: bool,
    base: usize,
}

impl ChildOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn base(mut self, n: usize) -> Self {
        self.base = n;
        self
    }

    pub fn forbid_multiline(mut self) -> Self {
        self.forbid_multiline = true;
        self
    }

    pub fn newline(mut self) -> Self {
        self.newline = true;
        self
    }

    pub fn multiline_mode(mut self) -> Self {
        self.multiline_mode = true;
        self
    }

    fn apply(&self, transaction: &mut Transaction) {
        assert!(transaction.buf.is_empty());
        if self.newline {
            transaction.needs_newline();
            transaction.indent = transaction
                .ancestor_indents
                .iter()
                .rev()
                .nth(self.base)
                .copied()
                .expect("TODO")
                + 4;
        }
        if self.multiline_mode {
            transaction.enable_multiline_mode();
        }
        if self.forbid_multiline {
            transaction.forbid_multiline = true;
        }
    }
}

pub trait Format: Span {
    fn format(&self, fmt: &mut Formatter) -> Result<()>;

    // TODO:
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
    transactions: Vec<Transaction>,
    text: String,
    macros: BTreeMap<Position, Macro>,
    comments: BTreeMap<Position, CommentToken>,
}

impl Formatter {
    pub fn new(
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
    ) -> Self {
        Self {
            transactions: vec![Transaction::new()],
            text,
            macros,
            comments,
        }
    }

    pub fn max_columns(&self) -> usize {
        self.transaction().max_columns
    }

    pub fn current_columns(&self) -> usize {
        self.transaction().current_columns
    }

    fn transaction_mut(&mut self) -> &mut Transaction {
        self.transactions.last_mut().expect("bug")
    }

    fn transaction(&self) -> &Transaction {
        self.transactions.last().expect("bug")
    }

    // TODO: remove
    pub fn format_either_child_item(
        &mut self,
        item: &impl Format,
        options0: ChildOptions,
        options1: ChildOptions,
    ) -> Result<()> {
        let original = self.transaction().clone();
        let mut t0 = None;
        let mut t1 = None;
        let result0 = self.format_child_item_with_options(item, options0);
        if result0.is_ok() {
            t0 = Some(std::mem::replace(self.transaction_mut(), original.clone()));
        }
        let result1 = self.format_child_item_with_options(item, options1);
        if result1.is_ok() {
            t1 = Some(std::mem::replace(self.transaction_mut(), original.clone()));
        }
        match (result0.map(|_| t0.unwrap()), result1.map(|_| t1.unwrap())) {
            (Err(e), Err(_)) => Err(e),
            (Ok(transaction), Err(_)) => {
                *self.transaction_mut() = transaction;
                Ok(())
            }
            (Err(_), Ok(transaction)) => {
                *self.transaction_mut() = transaction;
                Ok(())
            }
            (Ok(t0), Ok(t1)) => {
                let max_columns = self.transaction().max_columns;
                let t0_columns = t0.current_max_columns;
                let t1_columns = t1.current_max_columns;

                if t0_columns <= max_columns && t1_columns <= max_columns {
                    let t0_lines = t0.current_lines + t0.needs_newline as usize;
                    let t1_lines = t1.current_lines + t1.needs_newline as usize;
                    if t0_lines < t1_lines {
                        *self.transaction_mut() = t0;
                    } else {
                        *self.transaction_mut() = t1;
                    }
                } else if t0_columns > max_columns && t1_columns <= max_columns {
                    *self.transaction_mut() = t1;
                } else if t0_columns <= max_columns && t1_columns > max_columns {
                    *self.transaction_mut() = t0;
                } else {
                    if t0_columns < t1_columns {
                        *self.transaction_mut() = t0;
                    } else {
                        *self.transaction_mut() = t1;
                    }
                }

                Ok(())
            }
        }
    }

    pub fn multiline_mode(&self) -> bool {
        self.transaction().multiline_mode
    }

    pub fn needs_newline(&mut self) {
        self.transaction_mut().needs_newline();
    }

    pub fn needs_space(&mut self) {
        self.transaction_mut().needs_space();
    }

    pub fn enable_multiline_mode(&mut self) {
        self.transaction_mut().enable_multiline_mode();
    }

    pub fn format_module(mut self, forms: &[Form]) -> Result<String> {
        for form in forms {
            self.format_child_item(form)?;
            self.needs_newline();
        }

        // TODO: handle remaining comments and empty macros
        assert_eq!(self.transactions.len(), 1);
        Ok(self.transactions.pop().unwrap().buf)
    }

    pub fn format_item(&mut self, item: &impl Format) -> Result<()> {
        self.write_comments(item)?;
        item.format(self)
    }

    pub fn format_child_item_with_options(
        &mut self,
        item: &impl Format,
        options: ChildOptions,
    ) -> Result<()> {
        let mut result = self.with_transaction(|this| {
            // TODO: handle comments and macros
            options.apply(this.transaction_mut());
            item.format(this)?;
            Ok(())
        });

        // TODO: improve this huristic
        for i in 1..=4 {
            match result {
                Err(Error::MaxColumnsExceeded | Error::Multiline) => {
                    result = self.with_transaction(|this| {
                        options.apply(this.transaction_mut());
                        this.transaction_mut().shorten_max_columns(i);
                        item.format(this)
                    })
                }
                _ => {
                    break;
                }
            }
        }

        match result {
            Err(Error::MaxColumnsExceeded | Error::Multiline) => {
                result = self.with_transaction(|this| {
                    options.apply(this.transaction_mut());
                    this.enable_multiline_mode();
                    item.format(this)
                })
            }
            _ => {}
        }

        result
    }

    pub fn format_child_item(&mut self, item: &impl Format) -> Result<()> {
        self.format_child_item_with_options(item, Default::default())
    }

    pub fn write_text(&mut self, item: &impl Span) -> Result<()> {
        // TODO: handle empty macros

        let transaction = self.transactions.last_mut().expect("bug");
        transaction.write_text(&self.text, item)?;
        Ok(())
    }

    pub fn write_comment(&mut self, item: &impl Span) -> Result<()> {
        let transaction = self.transactions.last_mut().expect("bug");
        transaction.write_comment(&self.text, item)?;
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
        self.transactions.last().unwrap().next_text_position
    }

    fn with_transaction<F>(&mut self, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        let child = self.transaction_mut().create_child();
        self.transactions.push(child);
        let result = f(self);
        let child = self.transactions.pop().expect("bug");
        if result.is_ok() {
            child.commit(self.transaction_mut());
        }
        result
    }
}

#[derive(Debug, Clone)]
pub struct Transaction {
    indent: usize,
    ancestor_indents: Vec<usize>,
    max_columns: usize,
    next_text_position: Position,
    current_columns: usize,
    current_max_columns: usize, // TODO: rename
    current_lines: usize,
    needs_space: bool,
    needs_newline: bool,
    multiline_mode: bool,
    forbid_multiline: bool,
    buf: String,
    parent_last_char: Option<char>,
}

impl Transaction {
    pub fn new() -> Self {
        Self {
            next_text_position: Position::new(0, 0, 0),
            indent: 0,
            ancestor_indents: Vec::new(),
            max_columns: 50, // TODO: Make configurable
            needs_space: false,
            needs_newline: false,
            multiline_mode: false,
            forbid_multiline: false,
            current_columns: 0,
            current_max_columns: 0,
            current_lines: 0,
            buf: String::new(),
            parent_last_char: None,
        }
    }

    pub fn create_child(&self) -> Self {
        Self {
            next_text_position: self.next_text_position,
            indent: if self.needs_newline {
                self.indent
            } else {
                self.current_columns
            },
            ancestor_indents: self
                .ancestor_indents
                .iter()
                .copied()
                .chain(std::iter::once(self.indent))
                .collect(),
            current_columns: self.current_columns,
            current_max_columns: 0,
            current_lines: self.current_lines,
            max_columns: self.max_columns,
            needs_space: self.needs_space,
            needs_newline: self.needs_newline,
            multiline_mode: false,
            forbid_multiline: self.forbid_multiline,
            buf: String::new(),
            parent_last_char: self.last_char(),
        }
    }

    fn last_char(&self) -> Option<char> {
        self.buf.chars().last().or(self.parent_last_char)
    }

    fn write_newline(&mut self) -> Result<()> {
        if self.needs_newline {
            if self.last_char().map_or(false, |c| c != '\n') {
                self.write_text0("\n")?;
            }
            self.needs_newline = false;
            self.needs_space = false;
        }
        Ok(())
    }

    fn write_space(&mut self) -> Result<()> {
        if self.needs_space {
            if self.last_char().map_or(false, |c| c != ' ') {
                self.write_text0(" ")?;
            }
            self.needs_space = false;
        }
        Ok(())
    }

    pub fn write_comment(&mut self, text: &str, item: &impl Span) -> Result<()> {
        let start = item.start_position().offset();
        let end = std::cmp::max(start, item.end_position().offset()); // TODO: remove
        let text = &text[start..end];

        self.write_newline()?;
        if self.current_columns > 0 {
            self.write_text0("  ")?;
        }
        if self.next_text_position.line() + 1 < item.start_position().line() {
            self.write_text0("\n")?;
            self.needs_space = false;
            self.needs_newline = false;
        }

        self.buf.push_str(text);
        self.current_columns += text.len();
        self.next_text_position = item.end_position();

        self.needs_newline();
        Ok(())
    }

    pub fn write_text(&mut self, text: &str, item: &impl Span) -> Result<()> {
        let start = item.start_position().offset();
        let end = std::cmp::max(start, item.end_position().offset()); // TODO: remove
        let text = &text[start..end];

        self.write_newline()?;
        self.write_space()?;
        if self.next_text_position.line() + 1 < item.start_position().line() {
            self.write_text0("\n")?;
            self.needs_space = false;
            self.needs_newline = false;
        }

        self.write_text0(text)?;

        self.next_text_position = item.end_position();
        Ok(())
    }

    // TODO: rename
    fn write_text0(&mut self, text: &str) -> Result<()> {
        for c in text.chars() {
            if c == '\n' {
                if self.forbid_multiline && !self.buf.chars().all(|c| c == '\n') {
                    return Err(Error::Multiline);
                }

                self.current_columns = 0;
                self.current_lines += 1;
            } else if !c.is_control() {
                if self.current_columns >= self.max_columns {
                    if !self.multiline_mode {
                        return Err(Error::MaxColumnsExceeded);
                    } else {
                        // TODO: Emit warning log
                    }
                }

                if self.current_columns < self.indent {
                    for _ in self.current_columns..self.indent {
                        self.buf.push(' ');
                    }
                    self.current_columns = self.indent;
                }
                self.current_columns += 1;
                self.current_max_columns =
                    std::cmp::max(self.current_max_columns, self.current_columns);
            }

            self.buf.push(c);
        }

        Ok(())
    }

    pub fn next_text_position(&self) -> Position {
        self.next_text_position
    }

    pub fn shorten_max_columns(&mut self, n: usize) {
        self.max_columns -= n;
    }

    pub fn enable_multiline_mode(&mut self) {
        self.multiline_mode = true;
        if self.needs_space {
            self.needs_newline();
        }
    }

    pub fn needs_newline(&mut self) {
        self.needs_newline = true;
    }

    pub fn needs_space(&mut self) {
        self.needs_space = true;
    }

    pub fn commit(self, parent: &mut Self) {
        parent.next_text_position = self.next_text_position;
        parent.current_columns = self.current_columns;
        parent.current_max_columns =
            std::cmp::max(parent.current_max_columns, self.current_max_columns);
        parent.current_lines = self.current_lines;
        parent.needs_space = self.needs_space;
        parent.needs_newline = self.needs_newline;
        parent.buf.push_str(&self.buf);
    }
}
