use crate::lex::LexedText;
use crate::parse::Either;
use crate::token::{Region, TokenRegion};
use std::io::Write;

#[derive(Debug, thiserror::Error)]
pub enum Error {
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
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: LexedText) -> Self {
        Self {
            writer: ScopedWriter::new(writer),
            text,
        }
    }

    fn next_macro_call_region(&self) -> Option<TokenRegion> {
        self.text
            .macro_calls
            .values()
            .next()
            .map(|x| x.region().clone())
    }

    fn remove_macro_calls(&mut self, region: &TokenRegion) -> usize {
        let mut count = 0;
        while let Some(m) = self.next_macro_call_region() {
            if region.start() <= m.start() && m.end() <= region.end() {
                self.text.macro_calls.remove(m.start());
                count += 1;
            } else {
                break;
            }
        }
        count
    }

    fn do_format_with_macro_calls(
        &mut self,
        item: &impl Format,
        noformat: bool,
        macro_call_region: TokenRegion,
    ) -> Result<()> {
        let item_region = item.region().clone();
        if macro_call_region.start() < item_region.start() {
            dbg!(&item_region);
            return Err(Error::UnalignedMacroCall {
                region: macro_call_region,
            });
        }

        if item_region.start() == macro_call_region.start() {
            if item_region.end() == macro_call_region.end() {
                let macro_call = self
                    .text
                    .macro_calls
                    .remove(macro_call_region.start())
                    .expect("unreachable");
                macro_call.format(self)?;
                return Ok(());
            } else if item_region.end() < macro_call_region.end() {
                todo!() // unreachable?
            }
        }

        if noformat {
            let text =
                &self.text.original_text[item_region.start().offset()..item_region.end().offset()];
            write!(self.writer, "{}", text)?;
        } else {
            match self.with_scope(|this| item.format(this)) {
                (Err(Error::UnalignedMacroCall { region }), _) => {
                    let count = self.remove_macro_calls(&item_region);
                    if count > 0 {
                        assert_eq!(count, 1);
                        let text = &self.text.original_text
                            [item_region.start().offset()..item_region.end().offset()];
                        dbg!(text);
                        write!(self.writer, "{}", text)?;
                    } else {
                        return Err(Error::UnalignedMacroCall { region });
                    }
                }
                (Err(e), _) => return Err(e),
                (Ok(()), buf) => {
                    self.writer.write_all(&buf)?;
                }
            }
        }
        Ok(())
    }

    fn do_format(&mut self, item: &impl Format, noformat: bool) -> Result<()> {
        if !self.text.comments.is_empty() {
            todo!();
        }

        let item_start = item.region().start().clone();
        let item_end = item.region().end().clone();

        if let Some(macro_call_region) = self.next_macro_call_region() {
            self.do_format_with_macro_calls(item, noformat, macro_call_region)
        } else {
            if noformat {
                let text = &self.text.original_text[item_start.offset()..item_end.offset()];
                write!(self.writer, "{}", text)?;
            } else {
                item.format(self)?;
            }
            Ok(())
        }
    }

    pub fn format(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, false)
    }

    pub fn noformat(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, true)
    }

    pub fn format_child(&mut self, child: &impl Format) -> Result<()> {
        // TODO: indent handling
        self.format(child)?;
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

    fn with_scope<F, T>(&mut self, f: F) -> (T, Vec<u8>)
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.writer.buf_stack.push(Vec::new());
        let value = f(self);
        (value, self.writer.buf_stack.pop().expect("unreachable"))
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
