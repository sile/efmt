use crate::pp::PreprocessedText;
use crate::token::Region;
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
    context: Context,
    text: PreprocessedText,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: PreprocessedText) -> Self {
        Self {
            writer,
            context: Context::new(),
            text,
        }
    }

    pub fn format(&mut self, _item: &impl Format) -> Result<()> {
        todo!()
    }

    pub fn format_child(&mut self, _child: &impl Format) -> Result<()> {
        todo!()
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

#[derive(Debug)]
pub struct Context {}

impl Context {
    pub fn new() -> Self {
        Self {}
    }
}
