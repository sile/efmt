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

impl Default for MultilineMode {
    fn default() -> Self {
        Self::Allow
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Whitespace {
    Blank,
    Newline,
}

#[derive(Debug, Clone, Default)]
pub struct GroupOptions {
    newline: bool,
    multiline_mode: MultilineMode,
    indent: Option<usize>,
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
        todo!()
        // assert!(transaction.buf.is_empty());
        // if self.newline {
        //     transaction.needs_newline();
        //     transaction.indent = transaction
        //         .ancestor_indents
        //         .iter()
        //         .rev()
        //         .nth(self.base)
        //         .copied()
        //         .expect("TODO")
        //         + 4;
        // }
        // if self.multiline_mode {
        //     transaction.enable_multiline_mode();
        // }
        // if self.forbid_multiline {
        //     transaction.forbid_multiline = true;
        // }
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
    pub fn new(
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
    ) -> Self {
        let max_columns = 50; // TODO
        Self {
            transaction: Transaction::root(max_columns),
            text,
            macros,
            comments,
        }
    }

    pub fn max_columns(&self) -> usize {
        todo!()
        //self.transaction().max_columns
    }

    pub fn current_columns(&self) -> usize {
        todo!()
        // self.transaction().current_columns
    }

    pub fn multiline_mode(&self) -> bool {
        todo!()
        //self.transaction().multiline_mode
    }

    pub fn needs_newline(&mut self) {
        todo!()
        //self.transaction_mut().needs_newline();
    }

    pub fn needs_space(&mut self) {
        todo!()
        //self.transaction_mut().needs_space();
    }

    pub fn enable_multiline_mode(&mut self) {
        todo!()
        //self.transaction_mut().enable_multiline_mode();
    }

    pub fn format_module(mut self, forms: &[Form]) -> Result<String> {
        todo!()
        // for form in forms {
        //     self.format_child_item(form)?;
        //     self.needs_newline();
        // }

        // // TODO: handle remaining comments and empty macros
        // assert_eq!(self.transactions.len(), 1);
        // Ok(self.transactions.pop().unwrap().buf)
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
        todo!()
        // let mut result = self.with_transaction(|this| {
        //     // TODO: handle comments and macros
        //     options.apply(this.transaction_mut());
        //     item.format(this)?;
        //     Ok(())
        // });

        // // TODO: improve this huristic
        // for i in 1..=4 {
        //     match result {
        //         Err(Error::MaxColumnsExceeded | Error::Multiline) => {
        //             result = self.with_transaction(|this| {
        //                 options.apply(this.transaction_mut());
        //                 this.transaction_mut().shorten_max_columns(i);
        //                 item.format(this)
        //             })
        //         }
        //         _ => {
        //             break;
        //         }
        //     }
        // }

        // match result {
        //     Err(Error::MaxColumnsExceeded | Error::Multiline) => {
        //         result = self.with_transaction(|this| {
        //             options.apply(this.transaction_mut());
        //             this.enable_multiline_mode();
        //             item.format(this)
        //         })
        //     }
        //     _ => {}
        // }

        // result
    }

    pub fn format_child_item(&mut self, item: &impl Format) -> Result<()> {
        self.format_child_item_with_options(item, Default::default())
    }

    pub fn write_text(&mut self, item: &impl Span) -> Result<()> {
        todo!()
        // TODO: handle empty macros

        // let transaction = self.transactions.last_mut().expect("bug");
        // transaction.write_text(&self.text, item)?;
        // Ok(())
    }

    pub fn write_comment(&mut self, item: &impl Span) -> Result<()> {
        // let transaction = self.transactions.last_mut().expect("bug");
        // transaction.write_comment(&self.text, item)?;
        // Ok(())
        todo!()
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
        todo!()
        //self.transactions.last().unwrap().next_text_position
    }

    pub fn with_subgroup<F>(&mut self, options: GroupOptions, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        todo!()
    }

    pub fn with_transaction<F>(&mut self, config: TransactionConfig, f: F) -> Result<()>
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
