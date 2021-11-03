use crate::format::{self, Format, Formatter};
use crate::parse::Parse;
use crate::span::Span;
use std::io::Write;

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

impl<T: Format> Format for Newline<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_newline()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T: Format> Format for Space<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.needs_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Indent<T>(T);

impl<T: Format> Format for Indent<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.enter_block()?;
        fmt.format_item(&self.0)?;
        fmt.leave_block()?;
        Ok(())
    }
}
