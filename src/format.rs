use crate::cst::expressions::Body;
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
    max_line: usize,
    indent_level: usize,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: LexedText) -> Self {
        Self {
            writer: ScopedWriter::new(writer),
            text,
            next_position: TokenPosition::new(0, 1, 1),
            max_line: 100,
            indent_level: 0,
        }
    }

    pub fn write_indent(&mut self) -> Result<()> {
        write!(
            self.writer,
            "{:indent$}",
            "",
            indent = self.indent_level * 4
        )?;
        Ok(())
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

    pub fn finish(self) -> Result<()> {
        // TODO
        Ok(())
    }

    fn write_comments(&mut self, item: &impl Format) -> Result<()> {
        // TODO: improve
        if let Some(comment) = self.text.comments.values().next() {
            if comment.region().start() < item.region().start() {
                let start = comment.region().start();
                let end = comment.region().end();
                let text = &self.text.original_text[start.offset()..end.offset()];
                writeln!(self.writer, "{}", text)?;
                self.text.comments.remove(&start);
            }
        }
        Ok(())
    }

    fn do_format(&mut self, item: &impl Format, noformat: bool) -> Result<()> {
        self.write_comments(item)?;

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
        writeln!(self.writer)?;
        Ok(())
    }

    pub fn format_option(&mut self, item: &Option<impl Format>) -> Result<()> {
        item.as_ref()
            .map(|x| self.format(x))
            .transpose()
            .map(|_| ())
    }

    pub fn format_space(&mut self) -> Result<()> {
        // TODO: omit if the current position is the end of a line.
        write!(self.writer, " ")?;
        Ok(())
    }

    pub fn format_body(&mut self, body: &Body) -> Result<()> {
        if body.exprs().len() == 1 {
            // TODO: check line length
            // TODO: switch mode if the formatted code contains newlines
            write!(self.writer, " ")?;
            self.with_block(|this| this.format(&body.exprs()[0]))?;
        } else {
            writeln!(self.writer)?;
            self.with_block(|this| {
                this.write_indent()?;
                this.format(&body.exprs()[0])?;
                for (delimiter, expr) in body.delimiters().iter().zip(body.exprs().iter().skip(1)) {
                    this.format(delimiter)?;
                    this.write_newline()?;
                    this.format(expr)?;
                }
                Ok(())
            })?;
        }
        Ok(())
    }

    pub fn with_block<F>(&mut self, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.indent_level += 1;
        let result = f(self);
        self.indent_level -= 1;
        result
    }

    pub fn write_newline(&mut self) -> Result<()> {
        writeln!(self.writer)?;
        self.write_indent()?;
        Ok(())
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
