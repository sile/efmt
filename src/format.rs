use crate::lex::LexedText;
use crate::token::{Region, TokenRegion};
use std::io::Write;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Format: Region {
    // Note that this method isn't intended to be called by users directly.
    // Please use `Formatter::format()` inside the method implementation instead.
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> Result<()>;
}

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
    text: LexedText,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: LexedText) -> Self {
        Self { writer, text }
    }

    pub fn format(&mut self, item: &impl Format) -> Result<()> {
        if !self.text.comments.is_empty() {
            todo!();
        }
        if !self.text.macro_calls.is_empty() {
            todo!()
        }
        item.format(self)?;
        Ok(())
    }

    pub fn format_child(&mut self, child: &impl Format) -> Result<()> {
        // TODO: indent handling
        child.format(self)?;
        Ok(())
    }

    pub fn format_children<T: Format>(&mut self, children: &[T], delimiter: &str) -> Result<()> {
        for (i, child) in children.iter().enumerate() {
            self.format_child(child)?;
            if i + 1 < children.len() {
                write!(self, "{} ", delimiter)?;
            }
        }
        Ok(())
    }

    pub fn write_original_text(&mut self, region: &TokenRegion) -> Result<()> {
        if !self.text.macro_calls.is_empty() {
            todo!()
        }

        let start = region.start().text_position().offset();
        let end = region.end().text_position().offset();
        write!(self.writer, "{}", &self.text.original_text[start..end])?;
        Ok(())
    }
}

impl<W: Write> Write for Formatter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.writer.write(buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.writer.flush()
    }
}
