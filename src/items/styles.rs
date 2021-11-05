use crate::format::{self, Format, Formatter, Item};
use crate::parse::Parse;
use crate::span::Span;
use std::io::Write;

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

// TODO: use derive trait attribute
impl<T: Item> Item for Newline<T> {
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

impl<T: Format> Format for Newline<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_newline()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T: Item> Item for Space<T> {
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

impl<T: Format> Format for Space<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Indent<T, const I: usize>(T);

impl<T, const I: usize> Indent<T, I> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Item, const I: usize> Item for Indent<T, I> {
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
