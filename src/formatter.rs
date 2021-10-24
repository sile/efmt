use crate::ast::Ast;
use crate::lexer::Region;
use crate::pp::PreprocessedText;
use erl_tokenize::{Position, PositionRange};
use std::io::Write;

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
    context: Context,
    last_line: usize,
}

impl<W> Formatter<W>
where
    W: Write,
{
    pub fn new(context: Context, writer: W) -> Self {
        Self {
            context,
            writer,
            last_line: 0,
        }
    }

    pub fn format(&mut self, ast: Ast) -> Result<()> {
        let start = self.context.start_position(&ast);
        let end = self.context.end_position(&ast);
        self.print_toplevel_comments(Some(start.clone()))?;

        if self.last_line + 1 < start.line() {
            writeln!(self.writer)?;
        }
        ast.format(&mut self.writer, &mut self.context)?;
        self.context
            .print_trailing_comments(&mut self.writer, start.clone(), end.clone())?;
        self.last_line = end.line();
        writeln!(self.writer)?;
        Ok(())
    }

    pub fn print_trailing_comments(&mut self) -> Result<()> {
        self.print_toplevel_comments(None)
    }

    fn print_toplevel_comments(&mut self, next_ast_position: Option<Position>) -> Result<()> {
        while self.context.pp.comments.keys().next().map_or(false, |p0| {
            next_ast_position.as_ref().map_or(true, |p1| p0 < p1)
        }) {
            let position = self
                .context
                .pp
                .comments
                .keys()
                .cloned()
                .next()
                .expect("unreachable");
            if self.last_line + 1 < position.line() {
                writeln!(self.writer)?;
            }
            self.last_line = position.line();

            let comment = self
                .context
                .pp
                .comments
                .remove(&position)
                .expect("unreachable");
            writeln!(self.writer, "{}", comment.text())?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pp: PreprocessedText,
}

impl Context {
    pub fn new(pp: PreprocessedText) -> Self {
        Self { pp }
    }

    pub fn start_position(&self, x: &impl Format) -> Position {
        self.pp.tokens[x.region().start.0].start_position()
    }

    pub fn end_position(&self, x: &impl Format) -> Position {
        self.pp.tokens[x.region().end.0 - 1].end_position()
    }

    pub fn print_trailing_comments(
        &mut self,
        writer: &mut impl Write,
        start: Position,
        end: Position,
    ) -> Result<()> {
        let mut comments = Vec::new();
        while let Some(position) = self.pp.comments.keys().next().cloned() {
            if position.line() < start.line() || end.line() < position.line() {
                break;
            }
            comments.push(self.pp.comments.remove(&position).expect("unreachable"));
        }

        for comment in comments {
            write!(writer, "  {}", comment)?;
        }
        Ok(())
    }

    pub fn remove_comments_by_region(&mut self, region: Region) {
        let start = self.pp.tokens[region.start.0].start_position();
        let end = self.pp.tokens[region.end.0].end_position();
        self.remove_comments(start, end);
    }

    pub fn remove_comments(&mut self, start: Position, end: Position) {
        while let Some(position) = self.pp.comments.keys().next().cloned() {
            if position.line() < start.line() || end.line() < position.line() {
                break;
            }
            self.pp.comments.remove(&position);
        }
    }

    pub fn print_original_text(
        &mut self,
        writer: &mut impl Write,
        start: Position,
        end: Position,
    ) -> Result<()> {
        write!(writer, "{}", &self.pp.text[start.offset()..end.offset()])?;
        self.remove_comments(start, end);
        Ok(())
    }

    pub fn print_original_text_by_region(
        &mut self,
        writer: &mut impl Write,
        region: Region,
    ) -> Result<()> {
        // TODO: consider macro expansion (i.e., return an error if the range is invalid)
        let start = self.pp.tokens[region.start.0].start_position();
        let end = self.pp.tokens[region.end.0].end_position();
        self.print_original_text(writer, start, end)
    }

    pub fn remove_macro_calls_by_region(&mut self, region: Region) -> bool {
        let mut removed = false;
        while let Some(token_range) = self.pp.macro_calls.keys().next().cloned() {
            if region.start.0 <= token_range.start && token_range.end <= region.end.0 {
                removed = true;
                let _ = self.pp.macro_calls.remove(&token_range);
            } else {
                break;
            }
        }
        removed
    }
}

pub trait Format {
    fn format(&self, writer: &mut impl Write, context: &mut Context) -> Result<()> {
        let mut buf = Vec::new();
        self.do_format(&mut buf, context)?;
        let formatted = String::from_utf8(buf)?;
        if context.remove_macro_calls_by_region(self.region()) {
            context.print_original_text_by_region(writer, self.region())?;
        } else {
            write!(writer, "{}", formatted)?;
        }
        Ok(())
    }

    fn do_format(&self, writer: &mut impl Write, context: &mut Context) -> Result<()>;

    fn region(&self) -> crate::lexer::Region;

    // fn children(&self) -> Vec<Box<dyn Format>> {
    //     Vec::new()
    // }
}

pub type Result<T> = anyhow::Result<T>;
