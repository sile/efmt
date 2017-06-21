use std::io::Write;
use erl_parse::cst::ModuleDecl;

use {Result, Error};

#[derive(Debug)]
pub struct Formatter<W> {
    writer: W,
}
impl<W: Write> Formatter<W> {
    pub fn new(writer: W) -> Self {
        Formatter { writer }
    }
    pub fn format(&mut self, module: &ModuleDecl) -> Result<()> {
        for form in module.forms.iter() {
            track!(writeln!(&mut self.writer, "{:?}", form).map_err(
                Error::from,
            ))?;
        }
        Ok(())
    }
}
