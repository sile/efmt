use crate::ast::Ast;
use crate::pp::Preprocessed;
use erl_tokenize::{Position, PositionRange};
use std::io::Write;

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
    context: Context,
}

impl<W> Formatter<W>
where
    W: Write,
{
    pub fn new(context: Context, writer: W) -> Self {
        Self { context, writer }
    }

    pub fn format(&mut self, ast: Ast) -> Result<()> {
        self.print_toplevel_comments(self.context.start_position(&ast))?;
        ast.format(&mut self.writer, &mut self.context)?;
        Ok(())
    }

    fn print_toplevel_comments(&mut self, next_ast_position: Position) -> Result<()> {
        let mut last_line = 0;
        while self
            .context
            .pp
            .comments
            .keys()
            .next()
            .map_or(false, |p| *p < next_ast_position)
        {
            let position = self
                .context
                .pp
                .comments
                .keys()
                .cloned()
                .next()
                .expect("unreachable");
            if last_line + 1 < position.line() {
                writeln!(self.writer)?;
            }
            last_line = position.line();

            let comment = self
                .context
                .pp
                .comments
                .remove(&position)
                .expect("unreachable");
            writeln!(self.writer, "{}", comment.value())?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    pp: Preprocessed,
}

impl Context {
    pub fn new(pp: Preprocessed) -> Self {
        Self { pp }
    }

    pub fn start_position(&self, x: &impl Format) -> Position {
        self.pp.tokens[x.region().start.0].start_position()
    }
}

pub trait Format {
    fn format(&self, writer: &mut impl Write, context: &mut Context) -> Result<()>;

    fn region(&self) -> crate::lexer::Region;
}

pub type Result<T> = anyhow::Result<T>;
