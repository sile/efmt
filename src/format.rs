use crate::items::forms::Form;
use crate::items::macros::Macro;
use crate::items::tokens::CommentToken;
use crate::span::{Position, Span};
use std::collections::BTreeMap;
use std::io::Write;

pub use efmt_derive::Item;

mod writers;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    MaxColumnsExceeded,

    #[error(transparent)]
    Io(#[from] std::io::Error),

    #[error(transparent)]
    Int(#[from] std::num::ParseIntError),
}

pub type Result<T> = std::result::Result<T, Error>;

impl<A: Item, B: Item> Item for (A, B) {
    fn tree(&self) -> Tree {
        Tree::Compound(vec![self.0.tree(), self.1.tree()])
    }
}

#[derive(Debug, Clone)]
pub struct ItemSpan {
    start_position: Position,
    end_position: Position,
}

impl ItemSpan {
    pub fn new(start_position: Position, end_position: Position) -> Self {
        Self {
            start_position,
            end_position,
        }
    }
}

impl Span for ItemSpan {
    fn start_position(&self) -> Position {
        self.start_position
    }

    fn end_position(&self) -> Position {
        self.end_position
    }
}

#[derive(Debug, Clone)]
pub enum Tree {
    Atomic(Vec<ItemSpan>), // or Pattern
    Compound(Vec<Tree>),
    Child {
        tree: Box<Tree>,
        maybe_newline: bool,
    }, // increment indent
    IndentOffset {
        offset: usize,
        tree: Box<Tree>,
    },
    // TODO: remove(? maybe only `Coumpound` is sufficient)
    Unbalanced {
        left: Box<Tree>,
        delimiter: ItemSpan,
        right: Box<Tree>,
    },
    Elements {
        // TODO: rename? (children or something)
        trees: Vec<Tree>,
        delimiters: Vec<ItemSpan>,
        packed: bool,
    },
    BinaryOp {
        left: Box<Tree>,
        delimiter: ItemSpan,
        right: Box<Tree>,
    },
    Space {
        tree: Box<Tree>,
        left: bool,
        right: bool,
    },
    Linefeed(Box<Tree>),
    Newline(Box<Tree>),
    SideEffect(Box<Tree>), // TODO: remove
    Block(Box<Tree>),
    None,
}

impl Tree {
    pub fn child(tree: Self, maybe_newline: bool) -> Self {
        Self::Child {
            tree: Box::new(tree),
            maybe_newline,
        }
    }

    pub fn space(tree: Self) -> Self {
        Self::Space {
            tree: Box::new(tree),
            left: true,
            right: true,
        }
    }

    pub fn right_space(tree: Self) -> Self {
        Self::Space {
            tree: Box::new(tree),
            left: false,
            right: true,
        }
    }

    pub fn linefeed(tree: Self) -> Self {
        Self::Linefeed(Box::new(tree))
    }

    pub fn newline(tree: Self) -> Self {
        Self::Newline(Box::new(tree))
    }

    // TODO: rename
    pub fn is_pattern(&self) -> bool {
        match self {
            Self::Atomic(_) => true,
            Self::Compound(x) => x.iter().all(|x| x.is_pattern()),
            Self::Child { tree, .. } => tree.is_pattern(),
            Self::SideEffect(_) => false,
            Self::Unbalanced { .. } => false, // TODO
            Self::Elements { trees, .. } => trees.iter().all(|x| x.is_pattern()),
            Self::BinaryOp { left, right, .. } => left.is_pattern() && right.is_pattern(),
            Self::IndentOffset { tree, .. } => tree.is_pattern(),
            Self::Space { tree, .. } => tree.is_pattern(),
            Self::Newline(_) => false,
            Self::Linefeed(_) => false,
            Self::Block(_) => false,
            Self::None => true,
        }
    }

    pub fn next_position(&self) -> Option<Position> {
        match self {
            Self::Atomic(x) => x.first().map(|x| x.start_position()),
            Self::Compound(x) => x.first().and_then(|x| x.next_position()),
            Self::Child { tree, .. } => tree.next_position(),
            Self::SideEffect(x) => x.next_position(),
            Self::IndentOffset { tree, .. } => tree.next_position(),
            Self::Unbalanced { left, .. } => left.next_position(),
            Self::Elements { trees, .. } => trees.first().and_then(|x| x.next_position()),
            Self::BinaryOp { left, .. } => left.next_position(),
            Self::Space { tree, .. } => tree.next_position(),
            Self::Newline(x) => x.next_position(),
            Self::Linefeed(x) => x.next_position(),
            Self::Block(x) => x.next_position(),
            Self::None => None,
        }
    }
}

// TODO: rename and remove `Span` requirement
pub trait Item: Span {
    fn tree(&self) -> Tree;
}

// TODO: remove?
impl<T: Item + ?Sized> Item for &T {
    fn tree(&self) -> Tree {
        (**self).tree()
    }
}

#[derive(Debug)]
pub struct Formatter<W> {
    writer: self::writers::Writer<W>,
    state: FormatterState,
    text: String,
    macros: BTreeMap<Position, Macro>,
    comments: BTreeMap<Position, CommentToken>,
}

impl<W: Write> Formatter<W> {
    pub fn new(
        writer: W,
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
    ) -> Self {
        Self {
            writer: self::writers::Writer::new(writer),
            state: FormatterState::new(),
            text,
            macros,
            comments,
        }
    }

    pub fn format_tree(&mut self, tree: &Tree) -> Result<()> {
        //let state = self.state.clone();
        self.writer.start_transaction();
        if let Err(e) = self.format_tree0(tree) {
            self.writer.rollback();
            return Err(e);
        }

        if self.writer.max_columns_exceeded() && !self.state.newline_mode {
            self.writer.rollback();
            //self.state = state;
            return Err(Error::MaxColumnsExceeded);
        }
        self.writer.commit()?;
        Ok(())
    }

    fn format_tree0(&mut self, tree: &Tree) -> Result<()> {
        match tree {
            Tree::Atomic(x) => {
                for x in x {
                    self.write_text(x)?;
                }
            }
            Tree::Compound(x) => {
                for x in x {
                    self.format_tree(x)?;
                }
            }
            Tree::Child {
                tree,
                maybe_newline,
            } => {
                let mut newline_mode = self.state.newline_mode;
                self.state.newline_mode = false;
                self.state.indent_level += 4; // TODO: increment only if in newline mode
                let state = self.state.clone();
                match self.format_tree(tree) {
                    Err(Error::MaxColumnsExceeded) => {
                        eprintln!("Try re-formatting in newline_mode");
                        self.state = state;
                        self.state.newline_mode = true;
                        newline_mode = true;
                        dbg!("foo");
                        self.format_tree(tree)?;
                        dbg!("bar");
                    }
                    Err(e) => return Err(e),
                    _ => {}
                }

                if self.state.newline_mode && *maybe_newline {
                    self.needs_newline()?;
                }
                self.state.indent_level -= 4;
                self.state.newline_mode = newline_mode;
            }
            Tree::SideEffect(x) => {
                self.format_tree(x)?;
            }
            Tree::Unbalanced {
                left,
                delimiter,
                right,
            } => {
                // TODO:
                self.format_tree(left)?;
                self.needs_space()?;
                self.write_text(delimiter)?; // TODO
                self.needs_space()?;
                self.format_tree(right)?;
            }
            Tree::Elements {
                trees,
                delimiters,
                packed,
            } => {
                // TODO:
                let needs_newline = self.state.newline_mode;
                if needs_newline {
                    self.needs_newline()?;
                }
                if let Some(x) = trees.get(0) {
                    // TODO: save the current indent and use that for the following items
                    self.format_tree(x)?;
                    for (delimiter, tree) in delimiters.iter().zip(trees.iter().skip(1)) {
                        if *packed && self.writer.columns() == self.writer.max_columns() {
                            // TODO
                            self.needs_newline()?;
                            self.state.newline_mode = false;
                        }

                        self.write_text(delimiter)?; // TODO
                        if needs_newline {
                            self.needs_newline()?;
                        } else {
                            self.needs_space()?;
                        }

                        let state = self.state.clone();
                        match self.format_tree(tree) {
                            Err(Error::MaxColumnsExceeded) if *packed => {
                                self.state = state;
                                self.needs_newline()?;
                                self.state.newline_mode = false; // TODO
                                self.format_tree(tree)?; // TODO: consider delimiter length
                            }
                            Err(e) => return Err(e),
                            Ok(()) => {}
                        }
                    }
                }
            }
            Tree::BinaryOp {
                left,
                delimiter,
                right,
            } => {
                // TODO:
                self.format_tree(left)?;
                self.needs_space()?;
                self.write_text(delimiter)?;
                self.needs_space()?;
                self.format_tree(right)?;
            }
            Tree::IndentOffset { offset, tree } => {
                self.state.indent_level += *offset;
                self.format_tree(tree)?;
                self.state.indent_level -= *offset;
            }
            Tree::Newline(x) => {
                self.format_tree(x)?;
                self.needs_newline()?
            }
            Tree::Linefeed(x) => {
                self.needs_newline()?;
                self.format_tree(x)?;
            }
            Tree::Space { tree, left, right } => {
                if *left {
                    self.needs_space()?;
                }
                self.format_tree(tree)?;
                if *right {
                    self.needs_space()?;
                }
            }
            Tree::Block(x) => {
                self.format_tree(x)?;
            }
            Tree::None => {}
        }

        Ok(())
    }

    pub fn format_module(mut self, forms: &[Form]) -> Result<()> {
        for form in forms {
            self.format_tree(&form.tree())?;
            self.needs_newline()?;
        }

        let eof = Eof::new();
        // TODO: handle empty macro
        self.write_comments(&eof)?;
        if self.state.needs_newline {
            writeln!(self.writer)?;
        }

        Ok(())
    }

    fn write_text(&mut self, item: &impl Span) -> Result<()> {
        self.write_comments(item)?;
        self.write_newline(item)?;
        self.write_space()?;

        let start = item.start_position().offset();
        let end = std::cmp::max(start, item.end_position().offset()); // TODO: remove

        let text = &self.text[start..end];
        write!(self.writer, "{}", text)?;
        self.state.next_text_position = item.end_position();

        Ok(())
    }

    // TODO: rename
    fn needs_newline(&mut self) -> Result<()> {
        self.state.needs_newline = true;
        self.state.newline_mode = true;
        Ok(())
    }

    fn needs_space(&mut self) -> Result<()> {
        self.state.needs_space = 1;
        Ok(())
    }

    fn write_newline(&mut self, next_item: &impl Span) -> Result<()> {
        if !self.state.needs_newline {
            return Ok(());
        }

        if self.state.next_text_position.line() + 1 < next_item.start_position().line() {
            writeln!(self.writer)?;
        }
        write!(
            self.writer,
            "\n{:indent$}",
            "",
            indent = self.state.indent_level // TODO: remove `_level`
        )?;
        self.state.needs_newline = false;
        self.state.needs_space = 0;
        Ok(())
    }

    fn write_space(&mut self) -> Result<()> {
        if self.state.needs_space == 0 {
            return Ok(());
        }

        write!(self.writer, "{:width$}", "", width = self.state.needs_space)?;
        self.state.needs_space = 0;
        Ok(())
    }

    fn write_comments(&mut self, next_item: &impl Span) -> Result<()> {
        let end = next_item.start_position();
        let mut start = std::cmp::min(self.state.next_text_position, end);
        while let Some(token) = self.comments.range(start..end).map(|x| x.1.clone()).next() {
            if !self.state.needs_newline && self.state.next_text_position.offset() != 0 {
                self.state.needs_space = 2;
            }

            let comment = self.item_text(&token);
            if comment.starts_with("%% efmt:max_columns=") {
                let max_columns = (&comment["%% efmt:max_columns=".len()..]).parse()?;
                self.writer.set_max_columns(max_columns);
                eprintln!("[INFO] new max columns: {}", max_columns);
            }

            self.write_text(&token)?;
            self.needs_newline()?;
            start = std::cmp::min(self.state.next_text_position, end);
        }
        Ok(())
    }

    fn item_text(&self, item: &impl Span) -> &str {
        &self.text[item.start_position().offset()..item.end_position().offset()]
    }
}

#[derive(Debug, Clone)]
struct FormatterState {
    next_text_position: Position,
    indent_level: usize,
    needs_space: usize,
    needs_newline: bool,
    newline_mode: bool,
}

impl FormatterState {
    fn new() -> Self {
        Self {
            next_text_position: Position::new(0, 0, 0),
            indent_level: 0,
            needs_space: 0,
            needs_newline: false,
            newline_mode: false,
        }
    }
}

#[derive(Debug)]
struct Eof(Position);

impl Eof {
    fn new() -> Self {
        Self(Position::new(usize::MAX, usize::MAX, usize::MAX))
    }
}

impl Span for Eof {
    fn start_position(&self) -> Position {
        self.0
    }

    fn end_position(&self) -> Position {
        self.0
    }
}
