use crate::format::{self, Format, Formatter};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub struct Child<T>(T);

impl<T> Child<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for Child<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_child_item(&self.0)
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Block<T>(T);

impl<T> Block<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for Block<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_child_item_with_options(&self.0, format::ChildOptions::new().newline())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T: Format> Format for Space<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.needs_space();
        fmt.format_item(&self.0)?;
        fmt.needs_space();
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct RightSpace<T>(T);

impl<T: Format> Format for RightSpace<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_space();
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

impl<T: Format> Format for Newline<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_newline();
        Ok(())
    }
}
