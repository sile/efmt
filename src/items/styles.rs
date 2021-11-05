use crate::format::{Item, Tree};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

// TODO: use derive trait attribute
// TODO: remove
impl<T: Item> Item for Newline<T> {
    fn tree(&self) -> Tree {
        self.0.tree()
    }

    fn children(&self) -> Vec<&dyn Item> {
        self.0.children()
    }

    fn indent_offset(&self) -> usize {
        self.0.indent_offset()
    }

    fn prefers_oneline(&self) -> bool {
        self.0.prefers_oneline()
    }

    fn needs_linefeed(&self) -> bool {
        self.0.needs_linefeed()
    }

    fn needs_space(&self) -> bool {
        self.0.needs_space()
    }

    fn needs_newline(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T: Item> Item for Space<T> {
    fn tree(&self) -> Tree {
        self.0.tree()
    }

    fn children(&self) -> Vec<&dyn Item> {
        self.0.children()
    }

    fn indent_offset(&self) -> usize {
        self.0.indent_offset()
    }

    fn prefers_oneline(&self) -> bool {
        self.0.prefers_oneline()
    }

    fn needs_linefeed(&self) -> bool {
        self.0.needs_linefeed()
    }

    fn needs_space(&self) -> bool {
        true
    }

    fn needs_newline(&self) -> bool {
        self.0.needs_newline()
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Indent<T, const I: usize>(T);

impl<T, const I: usize> Indent<T, I> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Item, const I: usize> Item for Indent<T, I> {
    fn tree(&self) -> Tree {
        self.0.tree()
    }

    fn children(&self) -> Vec<&dyn Item> {
        self.0.children()
    }

    fn indent_offset(&self) -> usize {
        I
    }

    fn prefers_oneline(&self) -> bool {
        self.0.prefers_oneline()
    }

    fn needs_linefeed(&self) -> bool {
        self.0.needs_linefeed()
    }

    fn needs_space(&self) -> bool {
        self.0.needs_space()
    }

    fn needs_newline(&self) -> bool {
        self.0.needs_newline()
    }
}
