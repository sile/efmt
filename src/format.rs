use crate::lex::LexedText;
use crate::parse::Either;
use crate::token::{Region, TokenPosition, TokenRegion};
use std::io::Write;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    // TODO: remove
    #[error("TODO: unaligned macro call")]
    UnalignedMacroCall { region: TokenRegion },

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
    writer: ScopedWriter<W>,
    text: LexedText,
    next_position: TokenPosition,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: LexedText) -> Self {
        Self {
            writer: ScopedWriter::new(writer),
            text,
            next_position: TokenPosition::new(0, 1, 1),
        }
    }

    fn next_macro_call_region(&self) -> Option<TokenRegion> {
        self.text
            .macro_calls
            .values()
            .next()
            .map(|x| x.region().clone())
    }

    fn do_format_with_macro_calls(
        &mut self,
        item: &impl Format,
        macro_call_region: TokenRegion,
    ) -> Result<bool> {
        let item_region = item.region().clone();
        if macro_call_region.start() < item_region.start() {
            // Maybe the macro call was expanded to empty tokens.
            let macro_call = self
                .text
                .macro_calls
                .remove(&macro_call_region.start())
                .expect("unreachable");
            macro_call.format(self)?;
            writeln!(self.writer)?; // TODO
            return Ok(false);
        }

        if item_region.start() == macro_call_region.start() {
            if item_region.end() == macro_call_region.end() {
                let macro_call = self
                    .text
                    .macro_calls
                    .remove(&macro_call_region.start())
                    .expect("unreachable");
                macro_call.format(self)?;
                if macro_call.args().is_none() {
                    // TODO: Changed to put a space if the next visible token could be ambiguous
                    //       (e.g, atom, variable, integer, float, keyword)
                    // MEMO: We might be able to search the original text to detect the next character
                    write!(self.writer, " ")?;
                }

                return Ok(true);
            } else if item_region.end() < macro_call_region.end() {
                todo!() // unreachable?
            }
        }

        Ok(false)
    }

    fn do_format(&mut self, item: &impl Format, noformat: bool) -> Result<()> {
        if !self.text.comments.is_empty() {
            todo!();
        }

        let item_start = item.region().start();
        let item_end = item.region().end();

        if item.region().end() <= self.next_position {
            // May be a macro expanded item.
            // eprintln!("[SKIP] {:?} (next={:?})", item.region(), self.next_position);
            return Ok(());
        }

        if let Some(macro_call_region) = self.next_macro_call_region() {
            if self.do_format_with_macro_calls(item, macro_call_region)? {
                return Ok(());
            }
        }
        if noformat {
            // NOTE: `max` is for "max expanded" + "raw macro arg" case
            let start = std::cmp::max(item_start.offset(), self.next_position.offset());
            let end = item_end.offset();
            let text = &self.text.original_text[start..end];
            write!(self.writer, "{}", text)?;
        } else {
            item.format(self)?;
        }
        self.next_position = item.region().end();
        Ok(())
    }

    pub fn format_toplevel_item(&mut self, item: &impl Format) -> Result<()> {
        self.format(item)?;
        writeln!(self.writer)?;
        Ok(())
    }

    pub fn format_option(&mut self, item: &Option<impl Format>) -> Result<()> {
        item.as_ref()
            .map(|x| self.format(x))
            .transpose()
            .map(|_| ())
    }

    pub fn format(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, false)
    }

    pub fn noformat(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, true)
    }

    pub fn format_child(&mut self, child: &(impl Format + Region)) -> Result<()> {
        // TODO: indent handling
        self.format(child)?;
        Ok(())
    }

    pub fn format_children<T: Format, D: Format>(
        &mut self,
        children: &[T],
        delimiters: &[D],
    ) -> Result<()> {
        self.format_child(&children[0])?;
        for (c, d) in children.iter().skip(1).zip(delimiters.iter()) {
            self.format(d)?;
            write!(self.writer, " ")?;
            self.format_child(c)?;
        }
        Ok(())
    }

    pub fn format_clauses<T: Format + Region>(&mut self, _clauses: &[T]) -> Result<()> {
        // for (i, clause) in clauses.iter().enumerate() {
        //     self.format(clause)?;
        //     if i + 1 < clauses.len() {
        //         writeln!(self, ";")?;
        //     }
        // }
        // Ok(())
        todo!()
    }

    // fn with_scope<F, T>(&mut self, f: F) -> (T, Vec<u8>)
    // where
    //     F: FnOnce(&mut Self) -> T,
    // {
    //     self.writer.buf_stack.push(Vec::new());
    //     let value = f(self);
    //     (value, self.writer.buf_stack.pop().expect("unreachable"))
    // }
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

// TODO: rename
#[derive(Debug)]
struct ScopedWriter<W> {
    inner: W,
    buf_stack: Vec<Vec<u8>>,
}

impl<W> ScopedWriter<W> {
    fn new(inner: W) -> Self {
        Self {
            inner,
            buf_stack: Vec::new(),
        }
    }
}

impl<W: Write> Write for ScopedWriter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if let Some(x) = self.buf_stack.last_mut() {
            x.write(buf)
        } else {
            self.inner.write(buf)
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        if self.buf_stack.is_empty() {
            self.inner.flush()?;
        }
        Ok(())
    }
}
