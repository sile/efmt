use crate::lex::LexedText;
use crate::parse::Either;
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

    pub fn format_clauses<T: Format>(&mut self, clauses: &[T]) -> Result<()> {
        for (i, clause) in clauses.iter().enumerate() {
            self.format(clause)?;
            if i + 1 < clauses.len() {
                writeln!(self, ";")?;
            }
        }
        Ok(())
    }

    pub fn noformat(&mut self, item: &impl Region) -> Result<()> {
        if !self.text.macro_calls.is_empty() {
            todo!()
        }

        let start = item.region().start().offset();
        let end = item.region().end().offset();
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

impl<A, B> Format for Either<A, B>
where
    A: Format,
    B: Format,
{
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> Result<()> {
        match self {
            Self::A(x) => x.format(fmt),
            Self::B(x) => x.format(fmt),
        }
    }
}
