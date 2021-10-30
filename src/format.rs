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

// TODO: remove Debug
pub trait Format: Region + std::fmt::Debug {
    // Note that this method isn't intended to be called by users directly.
    // Please use `Formatter::format()` inside the method implementation instead.
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> Result<()>;
}

#[derive(Debug, Clone)]
struct FormatterState {
    next_position: TokenPosition, // source (TODO: rename)
    indent_level: usize,
    need_space: bool,
}

#[derive(Debug)]
pub struct Formatter<W> {
    writer: Either<ColumnCounter<W>, ColumnCounter<NewlineForbidWriter>>,
    text: LexedText,
    max_column: usize,
    state: FormatterState,
}

impl<W: Write> Formatter<W> {
    pub fn new(writer: W, text: LexedText) -> Self {
        let state = FormatterState {
            next_position: TokenPosition::new(0, 1, 1),
            indent_level: 0,
            need_space: false,
        };
        Self {
            writer: Either::A(ColumnCounter::new(writer)),
            text,
            max_column: 100,
            state,
        }
    }

    pub fn write_indent(&mut self) -> Result<()> {
        write!(
            self.writer,
            "{:indent$}",
            "",
            indent = self.state.indent_level * 4
        )?;
        Ok(())
    }

    fn next_macro_call_region(&self) -> Option<TokenRegion> {
        self.text
            .macro_calls
            .range(self.state.next_position..)
            .next()
            .map(|(_, x)| x.region().clone())
    }

    pub fn finish(self) -> Result<()> {
        // TODO: handle remaining comments and macro calls
        Ok(())
    }

    fn write_comments(&mut self, item: &impl Format) -> Result<()> {
        // TODO: improve
        if let Some((_, comment)) = self.text.comments.range(self.state.next_position..).next() {
            if comment.region().start() < item.region().start() {
                let start = comment.region().start();
                let end = comment.region().end();
                let text = &self.text.original_text[start.offset()..end.offset()];
                writeln!(self.writer, "{}", text)?;
                self.state.next_position = end;
            }
        }
        Ok(())
    }

    fn do_format_with_macro_calls(
        &mut self,
        item: &impl Format,
        macro_call_region: TokenRegion,
    ) -> Result<bool> {
        let item_region = item.region().clone();
        if macro_call_region.start() < item_region.start() {
            // Maybe the macro call was expanded to empty tokens.
            let macro_call = self.text.macro_calls[&macro_call_region.start()].clone();
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
                    self.state.need_space = true;
                }

                return Ok(true);
            } else if item_region.end() < macro_call_region.end() {
                if item_region.start().offset() + 1 == item.region().end().offset() {
                    // This should be '?'.
                    // TODO: improve this case handling
                } else {
                    dbg!(item);
                    dbg!(item_region);
                    dbg!(macro_call_region);
                    todo!() // unreachable?
                }
            }
        }

        Ok(false)
    }

    fn do_format(&mut self, item: &impl Format, noformat: bool) -> Result<()> {
        self.write_comments(item)?;

        let item_start = item.region().start();
        let item_end = item.region().end();

        if item.region().end() <= self.state.next_position {
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
            if self.state.need_space {
                write!(self.writer, " ")?;
                self.state.need_space = false;
            }

            // NOTE: `max` is for "max expanded" + "raw macro arg" case
            let start = std::cmp::max(item_start.offset(), self.state.next_position.offset());
            let end = item_end.offset();
            let text = &self.text.original_text[start..end];
            write!(self.writer, "{}", text)?;
        } else {
            item.format(self)?;
        }
        self.state.next_position = item.region().end();
        Ok(())
    }

    pub fn format_toplevel_item(&mut self, item: &impl Format) -> Result<()> {
        self.format(item)?;
        writeln!(self.writer)?;
        writeln!(self.writer)?;
        Ok(())
    }

    pub fn format_space(&mut self) -> Result<()> {
        self.state.need_space = true;
        Ok(())
    }

    pub fn enter_block(&mut self) -> Result<()> {
        self.state.indent_level += 1;
        self.write_newline()?;
        Ok(())
    }

    pub fn leave_block(&mut self) -> Result<()> {
        self.state.indent_level -= 1;
        self.write_newline()?;
        Ok(())
    }

    fn calc_required_column(&mut self, item: &impl Format) -> Result<usize> {
        let state = self.state.clone();
        let column = self.writer.column();
        let writer = std::mem::replace(
            &mut self.writer,
            Either::B(ColumnCounter {
                inner: NewlineForbidWriter,
                column,
            }),
        );
        let result = self.format(item);

        self.state = state;
        let column = self.writer.column();
        self.writer = writer;

        result.map(|_| column)
    }

    pub fn format_with_newline_if_multiline(&mut self, item: &impl Format) -> Result<()> {
        if let Ok(column) = self.calc_required_column(item) {
            if column <= self.max_column {
                self.format(item)?;
                return Ok(());
            }
            dbg!((column, self.max_column));
        } else {
            dbg!("error");
        }

        self.state.indent_level += 1;
        self.write_newline()?;
        self.format(item)?;
        self.state.indent_level -= 1;

        Ok(())
    }

    pub fn format_body(&mut self, body: &Body) -> Result<()> {
        if body.exprs().len() == 1 {
            if let Ok(column) = self.calc_required_column(&body.exprs()[0]) {
                if column <= self.max_column {
                    self.format(&body.exprs()[0])?;
                    return Ok(());
                }
            }
        }

        self.state.indent_level += 1;
        self.write_newline()?;
        self.format(&body.exprs()[0])?;
        for (delimiter, expr) in body.delimiters().iter().zip(body.exprs().iter().skip(1)) {
            self.format(delimiter)?;
            self.write_newline()?;
            self.format(expr)?;
        }
        self.state.indent_level -= 1;

        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<()> {
        writeln!(self.writer)?;
        self.write_indent()?;
        self.state.need_space = false;
        Ok(())
    }

    pub fn format(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, false)
    }

    // TODO: rename
    pub fn noformat(&mut self, item: &impl Format) -> Result<()> {
        self.do_format(item, true)
    }

    pub fn format_items<T: Format, D: Format>(
        &mut self,
        items: &[T],
        delimiters: &[D],
    ) -> Result<()> {
        self.format(&items[0])?;
        for (c, d) in items.iter().skip(1).zip(delimiters.iter()) {
            self.format(d)?;
            // TODO: insert newline if the formatted code of the next item will be too long
            self.format(c)?;
        }
        Ok(())
    }
}

// TODO
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

impl<A, B> Write for Either<A, B>
where
    A: Write,
    B: Write,
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match self {
            Self::A(x) => x.write(buf),
            Self::B(x) => x.write(buf),
        }
    }

    fn flush(&mut self) -> std::io::Result<()> {
        match self {
            Self::A(x) => x.flush(),
            Self::B(x) => x.flush(),
        }
    }
}

impl<A, B> Either<ColumnCounter<A>, ColumnCounter<B>> {
    fn column(&self) -> usize {
        match self {
            Self::A(x) => x.column,
            Self::B(x) => x.column,
        }
    }
}

#[derive(Debug, Default)]
struct NewlineForbidWriter;

impl Write for NewlineForbidWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        if buf.contains(&b'\n') {
            return Err(std::io::Error::new(
                std::io::ErrorKind::Other,
                "a newline was detected",
            ));
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Default)]
struct ColumnCounter<W> {
    inner: W,
    column: usize,
}

impl<W: Write> ColumnCounter<W> {
    fn new(inner: W) -> Self {
        Self { inner, column: 0 }
    }
}

impl<W: Write> Write for ColumnCounter<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let written = self.inner.write(buf)?;
        let buf = &buf[..written];
        // TODO: consider control characters such as '\r'
        if let Some(x) = buf.rsplit(|b| *b == b'\n').next() {
            self.column = x.len();
        } else {
            self.column += buf.len();
        }
        Ok(written)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.inner.flush()
    }
}
