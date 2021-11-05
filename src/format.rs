use crate::items::forms::Form;
use crate::items::macros::Macro;
use crate::items::tokens::CommentToken;
use crate::span::{Position, Span};
use std::collections::BTreeMap;
use std::io::Write;

pub use efmt_derive::Item;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Io(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

impl<A: Item, B: Item> Item for (A, B) {
    fn children(&self) -> Vec<&dyn Item> {
        vec![&self.0, &self.1]
    }
}

pub trait Item: Span {
    fn children(&self) -> Vec<&dyn Item> {
        Vec::new()
    }

    fn indent_offset(&self) -> usize {
        0
    }

    fn prefers_oneline(&self) -> bool {
        false
    }

    // TODO: rename
    fn needs_linefeed(&self) -> bool {
        false
    }

    fn needs_space(&self) -> bool {
        false
    }

    fn needs_newline(&self) -> bool {
        false
    }
}

impl<T: Item + ?Sized> Item for &T {
    fn children(&self) -> Vec<&dyn Item> {
        (**self).children()
    }

    fn indent_offset(&self) -> usize {
        (**self).indent_offset()
    }

    fn prefers_oneline(&self) -> bool {
        (**self).prefers_oneline()
    }

    fn needs_linefeed(&self) -> bool {
        (**self).needs_linefeed()
    }

    fn needs_space(&self) -> bool {
        (**self).needs_space()
    }

    fn needs_newline(&self) -> bool {
        (**self).needs_newline()
    }
}

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
    state: FormatterState,
    text: String,
    macros: BTreeMap<Position, Macro>,
    comments: BTreeMap<Position, CommentToken>,
    max_columns: usize,
}

impl<W: Write> Formatter<W> {
    pub fn new(
        writer: W,
        text: String,
        comments: BTreeMap<Position, CommentToken>,
        macros: BTreeMap<Position, Macro>,
    ) -> Self {
        Self {
            writer,
            state: FormatterState::new(),
            text,
            macros,
            comments,
            max_columns: 100,
        }
    }

    pub fn format(&mut self, item: &impl Item) -> Result<()> {
        if item.needs_linefeed() {
            self.needs_newline()?;
            // TODO: enable newline mode
        }

        self.state.indent_level += item.indent_offset();

        let children = item.children();
        if children.is_empty() {
            self.write_text(item)?;

            // TODO: factor out
            self.state.indent_level -= item.indent_offset();
            if item.needs_space() {
                self.needs_space()?;
            }
            if item.needs_newline() {
                self.needs_newline()?;
            }

            return Ok(());
        } else {
            for child in children {
                self.format(&child)?;
            }
        }

        self.state.indent_level -= item.indent_offset();
        if item.needs_space() {
            self.needs_space()?;
        }
        if item.needs_newline() {
            self.needs_newline()?;
        }
        Ok(())
    }

    pub fn format_module(mut self, forms: &[Form]) -> Result<()> {
        for form in forms {
            self.format(form)?;
            self.needs_newline()?;
        }

        let eof = Eof::new();
        // TODO: handle empty macro
        self.write_comments(&eof)?;
        if self.state.needs_newline {
            writeln!(self.writer)?;
        }

        Ok(())
    }

    pub fn write_text(&mut self, item: &impl Item) -> Result<()> {
        self.write_comments(item)?;
        self.write_newline(item)?;
        self.write_space()?;

        let start = item.start_position().offset();
        let end = std::cmp::max(start, item.end_position().offset()); // TODO: remove

        let text = &self.text[start..end];
        write!(self.writer, "{}", text)?;
        self.state.next_text_position = item.end_position();

        Ok(())
    }

    pub fn enter_block(&mut self) -> Result<()> {
        self.state.indent_level += 1;
        Ok(())
    }

    pub fn leave_block(&mut self) -> Result<()> {
        self.state.indent_level -= 1; // TODO: check
        Ok(())
    }

    // TODO: rename
    pub fn needs_newline(&mut self) -> Result<()> {
        self.state.needs_newline = true;
        Ok(())
    }

    pub fn needs_space(&mut self) -> Result<()> {
        self.state.needs_space = 1;
        Ok(())
    }

    fn write_newline(&mut self, next_item: &impl Span) -> Result<()> {
        if !self.state.needs_newline {
            return Ok(());
        }

        if self.state.next_text_position.line() + 1 < next_item.start_position().line() {
            writeln!(self.writer)?;
        }
        write!(
            self.writer,
            "\n{:indent$}",
            "",
            indent = self.state.indent_level // TODO: remove `_level`
        )?;
        self.state.needs_newline = false;
        self.state.needs_space = 0;
        Ok(())
    }

    fn write_space(&mut self) -> Result<()> {
        if self.state.needs_space == 0 {
            return Ok(());
        }

        write!(self.writer, "{:width$}", "", width = self.state.needs_space)?;
        self.state.needs_space = 0;
        Ok(())
    }

    fn write_comments(&mut self, next_item: &impl Span) -> Result<()> {
        let end = next_item.start_position();
        let mut start = std::cmp::min(self.state.next_text_position, end);
        while let Some(token) = self.comments.range(start..end).map(|x| x.1.clone()).next() {
            if !self.state.needs_newline && self.state.next_text_position.offset() != 0 {
                self.state.needs_space = 2;
            }
            self.write_text(&token)?;
            self.needs_newline()?;
            start = std::cmp::min(self.state.next_text_position, end);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct FormatterState {
    next_text_position: Position,
    indent_level: usize,
    needs_space: usize,
    needs_newline: bool,
}

impl FormatterState {
    fn new() -> Self {
        Self {
            next_text_position: Position::new(0, 0, 0),
            indent_level: 0,
            needs_space: 0,
            needs_newline: false,
        }
    }
}

#[derive(Debug)]
struct Eof(Position);

impl Eof {
    fn new() -> Self {
        Self(Position::new(usize::MAX, usize::MAX, usize::MAX))
    }
}

impl Span for Eof {
    fn start_position(&self) -> Position {
        self.0
    }

    fn end_position(&self) -> Position {
        self.0
    }
}

// use crate::cst::expressions::Body;
// use crate::lex::LexedText;
// use crate::parse::Either;
// use crate::token::{Region, TokenPosition, TokenRegion};
// use std::io::Write;

// #[derive(Debug)]
// pub struct Formatter<W> {
//     writer: Either<ColumnCounter<W>, ColumnCounter<NewlineForbidWriter>>,
//     text: LexedText,
//     max_column: usize,
//     state: FormatterState,
// }

// impl<W: Write> Formatter<W> {
//     pub fn new(writer: W, text: LexedText) -> Self {
//         let state = FormatterState {
//             next_position: TokenPosition::new(0, 1, 1),
//             indent_level: 0,
//             need_space: false,
//         };
//         Self {
//             writer: Either::A(ColumnCounter::new(writer)),
//             text,
//             max_column: 100,
//             state,
//         }
//     }

//     pub fn write_indent(&mut self) -> Result<()> {
//         write!(
//             self.writer,
//             "{:indent$}",
//             "",
//             indent = self.state.indent_level * 4
//         )?;
//         Ok(())
//     }

//     fn next_macro_call_region(&self) -> Option<TokenRegion> {
//         self.text
//             .macro_calls
//             .range(self.state.next_position..)
//             .next()
//             .map(|(_, x)| x.region().clone())
//     }

//     pub fn finish(self) -> Result<()> {
//         // TODO: handle remaining comments and macro calls
//         Ok(())
//     }

//     fn write_comments(&mut self, item: &impl Format) -> Result<()> {
//         // TODO: improve
//         if let Some((_, comment)) = self.text.comments.range(self.state.next_position..).next() {
//             if comment.region().start() < item.region().start() {
//                 let start = comment.region().start();
//                 let end = comment.region().end();
//                 let text = &self.text.original_text[start.offset()..end.offset()];
//                 writeln!(self.writer, "{}", text)?;
//                 self.state.next_position = end;
//             }
//         }
//         Ok(())
//     }

//     fn do_format_with_macro_calls(
//         &mut self,
//         item: &impl Format,
//         macro_call_region: TokenRegion,
//     ) -> Result<bool> {
//         let item_region = item.region().clone();
//         if macro_call_region.start() < item_region.start() {
//             // Maybe the macro call was expanded to empty tokens.
//             let macro_call = self.text.macro_calls[&macro_call_region.start()].clone();
//             macro_call.format(self)?;
//             writeln!(self.writer)?; // TODO
//             return Ok(false);
//         }

//         if item_region.start() == macro_call_region.start() {
//             if item_region.end() == macro_call_region.end() {
//                 let macro_call = self
//                     .text
//                     .macro_calls
//                     .remove(&macro_call_region.start())
//                     .expect("unreachable");
//                 macro_call.format(self)?;
//                 if macro_call.args().is_none() {
//                     // TODO: Changed to put a space if the next visible token could be ambiguous
//                     //       (e.g, atom, variable, integer, float, keyword)
//                     // MEMO: We might be able to search the original text to detect the next character
//                     self.state.need_space = true;
//                 }

//                 return Ok(true);
//             } else if item_region.end() < macro_call_region.end() {
//                 if item_region.start().offset() + 1 == item.region().end().offset() {
//                     // This should be '?'.
//                     // TODO: improve this case handling
//                 } else {
//                     dbg!(item);
//                     dbg!(item_region);
//                     dbg!(macro_call_region);
//                     todo!() // unreachable?
//                 }
//             }
//         }

//         Ok(false)
//     }

//     fn do_format(&mut self, item: &impl Format, noformat: bool) -> Result<()> {
//         self.write_comments(item)?;

//         let item_start = item.region().start();
//         let item_end = item.region().end();

//         if item.region().end() <= self.state.next_position {
//             // May be a macro expanded item.
//             // eprintln!("[SKIP] {:?} (next={:?})", item.region(), self.next_position);
//             return Ok(());
//         }

//         if let Some(macro_call_region) = self.next_macro_call_region() {
//             if self.do_format_with_macro_calls(item, macro_call_region)? {
//                 return Ok(());
//             }
//         }

//         if noformat {
//             if self.state.need_space {
//                 write!(self.writer, " ")?;
//                 self.state.need_space = false;
//             }

//             // NOTE: `max` is for "max expanded" + "raw macro arg" case
//             let start = std::cmp::max(item_start.offset(), self.state.next_position.offset());
//             let end = item_end.offset();
//             let text = &self.text.original_text[start..end];
//             write!(self.writer, "{}", text)?;
//         } else {
//             item.format(self)?;
//         }
//         self.state.next_position = item.region().end();
//         Ok(())
//     }

//     pub fn format_toplevel_item(&mut self, item: &impl Format) -> Result<()> {
//         self.format(item)?;
//         writeln!(self.writer)?;
//         writeln!(self.writer)?;
//         Ok(())
//     }

//     pub fn format_space(&mut self) -> Result<()> {
//         self.state.need_space = true;
//         Ok(())
//     }

//     pub fn enter_block(&mut self) -> Result<()> {
//         self.state.indent_level += 1;
//         self.write_newline()?;
//         Ok(())
//     }

//     pub fn leave_block(&mut self) -> Result<()> {
//         self.state.indent_level -= 1;
//         self.write_newline()?;
//         Ok(())
//     }

//     fn calc_required_column(&mut self, item: &impl Format) -> Result<usize> {
//         let state = self.state.clone();
//         let column = self.writer.column();
//         let writer = std::mem::replace(
//             &mut self.writer,
//             Either::B(ColumnCounter {
//                 inner: NewlineForbidWriter,
//                 column,
//             }),
//         );
//         let result = self.format(item);

//         self.state = state;
//         let column = self.writer.column();
//         self.writer = writer;

//         result.map(|_| column)
//     }

//     pub fn format_with_newline_if_multiline(&mut self, item: &impl Format) -> Result<()> {
//         if let Ok(column) = self.calc_required_column(item) {
//             dbg!(column);
//             if column <= self.max_column {
//                 self.format(item)?;
//                 return Ok(());
//             }
//             dbg!((column, self.max_column));
//         } else {
//             dbg!("error");
//         }

//         self.state.indent_level += 1;
//         self.write_newline()?;
//         self.format(item)?;
//         self.state.indent_level -= 1;

//         Ok(())
//     }

//     pub fn format_body(&mut self, body: &Body) -> Result<()> {
//         if body.exprs().len() == 1 {
//             if let Ok(column) = self.calc_required_column(&body.exprs()[0]) {
//                 if column <= self.max_column {
//                     self.format(&body.exprs()[0])?;
//                     return Ok(());
//                 }
//             }
//         }

//         self.state.indent_level += 1;
//         self.write_newline()?;
//         self.format(&body.exprs()[0])?;
//         for (delimiter, expr) in body.delimiters().iter().zip(body.exprs().iter().skip(1)) {
//             self.format(delimiter)?;
//             self.write_newline()?;
//             self.format(expr)?;
//         }
//         self.state.indent_level -= 1;

//         Ok(())
//     }

//     pub fn write_newline(&mut self) -> Result<()> {
//         writeln!(self.writer)?;
//         self.write_indent()?;
//         self.state.need_space = false;
//         Ok(())
//     }

//     pub fn format(&mut self, item: &impl Format) -> Result<()> {
//         self.do_format(item, false)
//     }

//     // TODO: rename
//     pub fn noformat(&mut self, item: &impl Format) -> Result<()> {
//         self.do_format(item, true)
//     }

//     pub fn format_items<T: Format, D: Format>(
//         &mut self,
//         items: &[T],
//         delimiters: &[D],
//     ) -> Result<()> {
//         self.format(&items[0])?;
//         for (c, d) in items.iter().skip(1).zip(delimiters.iter()) {
//             self.format(d)?;
//             // TODO: insert newline if the formatted code of the next item will be too long
//             self.format(c)?;
//         }
//         Ok(())
//     }
// }

// // TODO
// impl<A, B> Format for Either<A, B>
// where
//     A: Format,
//     B: Format,
// {
//     fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> Result<()> {
//         match self {
//             Self::A(x) => x.format(fmt),
//             Self::B(x) => x.format(fmt),
//         }
//     }
// }

// impl<A, B> Write for Either<A, B>
// where
//     A: Write,
//     B: Write,
// {
//     fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
//         match self {
//             Self::A(x) => x.write(buf),
//             Self::B(x) => x.write(buf),
//         }
//     }

//     fn flush(&mut self) -> std::io::Result<()> {
//         match self {
//             Self::A(x) => x.flush(),
//             Self::B(x) => x.flush(),
//         }
//     }
// }

// impl<A, B> Either<ColumnCounter<A>, ColumnCounter<B>> {
//     fn column(&self) -> usize {
//         match self {
//             Self::A(x) => x.column,
//             Self::B(x) => x.column,
//         }
//     }
// }

// #[derive(Debug, Default)]
// struct NewlineForbidWriter;

// impl Write for NewlineForbidWriter {
//     fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
//         if buf.contains(&b'\n') {
//             return Err(std::io::Error::new(
//                 std::io::ErrorKind::Other,
//                 "a newline was detected",
//             ));
//         }
//         Ok(buf.len())
//     }

//     fn flush(&mut self) -> std::io::Result<()> {
//         Ok(())
//     }
// }

// #[derive(Debug, Default)]
// struct ColumnCounter<W> {
//     inner: W,
//     column: usize,
// }

// impl<W: Write> ColumnCounter<W> {
//     fn new(inner: W) -> Self {
//         Self { inner, column: 0 }
//     }
// }

// impl<W: Write> Write for ColumnCounter<W> {
//     fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
//         let written = self.inner.write(buf)?;
//         let buf = &buf[..written];

//         // TODO: consider control characters such as '\r'
//         let mut split = buf.rsplit(|b| *b == b'\n');
//         match (split.next(), split.next()) {
//             (Some(_), None) => {
//                 self.column += buf.len();
//             }
//             (Some(x), Some(_)) => {
//                 self.column = x.len();
//             }
//             _ => unreachable!(),
//         }
//         Ok(written)
//     }

//     fn flush(&mut self) -> std::io::Result<()> {
//         self.inner.flush()
//     }
// }
