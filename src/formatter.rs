use crate::ast::Ast;
use crate::pp::MacroCall;
use erl_tokenize::tokens::CommentToken;
use erl_tokenize::Position;
use std::collections::BTreeMap;
use std::io::Write;

#[derive(Debug)]
pub struct Formatter<W> {
    ast: Ast,
    context: Context,
    writer: W,
}

impl<W> Formatter<W>
where
    W: Write,
{
    pub fn new(ast: Ast, context: Context, writer: W) -> Self {
        Self {
            ast,
            context,
            writer,
        }
    }

    pub fn format(mut self) -> Result<()> {
        self.ast.format(&mut self.writer, &mut self.context)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Context {
    comments: BTreeMap<Position, CommentToken>, // TODO: Rc
    macro_calls: BTreeMap<Position, MacroCall>,
}

impl Context {
    pub fn new(
        comments: BTreeMap<Position, CommentToken>,
        macro_calls: BTreeMap<Position, MacroCall>,
    ) -> Self {
        Self {
            comments,
            macro_calls,
        }
    }
}

pub trait Format {
    fn format(&self, writer: &mut impl Write, context: &mut Context) -> Result<()>;
}

pub type Result<T> = anyhow::Result<T>;
