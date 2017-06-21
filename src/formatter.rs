use std::io::Write;
use erl_parse::cst::{self, ModuleDecl};
use erl_tokenize::PositionRange;

use {Result, Error};

macro_rules! fmt {
    ($writer:expr, $($arg:tt)+) => {
        track!(write!($writer, $($arg)+).map_err(Error::from))
    }
}
macro_rules! fmtln {
    ($writer:expr, $($arg:tt)+) => {
        track!(writeln!($writer, $($arg)+).map_err(Error::from))
    }
}

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
    last_line: usize,
}
impl<W: Write> Formatter<W> {
    pub fn new(writer: W) -> Self {
        Formatter {
            writer,
            last_line: 1,
        }
    }
    pub fn format(&mut self, module: &ModuleDecl) -> Result<()> {
        for form in module.forms.iter() {
            match *form {
                cst::Form::CallbackSpec(ref f) => unimplemented!(),
                cst::Form::ExportAttr(ref f) => unimplemented!(),
                cst::Form::ExportTypeAttr(ref f) => unimplemented!(),
                cst::Form::FileAttr(ref f) => unimplemented!(),
                cst::Form::FunDecl(ref f) => unimplemented!(),
                cst::Form::FunSpec(ref f) => unimplemented!(),
                cst::Form::ImportAttr(ref f) => unimplemented!(),
                cst::Form::ModuleAttr(ref f) => track!(self.fmt_module_attr(f))?,
                cst::Form::RecordDecl(ref f) => unimplemented!(),
                cst::Form::TypeDecl(ref f) => unimplemented!(),
                cst::Form::WildAttr(ref f) => unimplemented!(),
            }
        }
        Ok(())
    }
    fn newlines<P: PositionRange>(&mut self, x: &P) -> Result<()> {
        for _ in self.last_line..x.start_position().line() {
            fmtln!(self.writer, "")?;
        }
        self.last_line = x.start_position().line();
        Ok(())
    }
    fn fmt_module_attr(&mut self, module: &cst::forms::ModuleAttr) -> Result<()> {
        self.newlines(module);
        fmtln!(self.writer, "-module({}).", module.module_name.text())?;
        self.last_line += 1;
        Ok(())
    }
}
