use crate::items::forms::Form;
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
    Force,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Whitespace {
    Blank,
    Newline,
}

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
