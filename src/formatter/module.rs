use std::collections::VecDeque;
use std::io::Write;
use erl_parse::{self, TokenReader};
use erl_parse::cst::Form;
use erl_parse::cst::forms;
use erl_pp::Preprocessor;
use erl_tokenize::{Lexer, PositionRange};

use Result;
use super::Input;

#[derive(Debug)]
pub struct ModuleFormatter<'a, W> {
    input: Input<'a>,
    output: W,
    forms: VecDeque<Form>,
}
impl<'a, W: Write> ModuleFormatter<'a, W> {
    pub fn new(code: &'a str, output: W) -> Result<Self> {
        let lexer = Lexer::new(code);
        let preprocessor = Preprocessor::new(lexer);
        let mut reader = TokenReader::new(preprocessor);
        let module_decl = track!(erl_parse::builtin::parse_module(&mut reader))?;
        Ok(ModuleFormatter {
            input: Input::new(code),
            output,
            forms: VecDeque::from(module_decl.forms),
        })
    }
    pub fn format(&mut self) -> Result<()> {
        while let Some(form) = self.forms.pop_front() {
            let count = form.start_position().offset() - self.input.position();
            for c in self.input.by_ref().take(count) {
                fmt!(self.output, "{}", c)?;
            }
            track!(self.format_form(form))?;
        }
        for c in self.input.by_ref() {
            fmt!(self.output, "{}", c)?;
        }
        Ok(())
    }

    fn format_form(&mut self, form: Form) -> Result<()> {
        match form {
            Form::ModuleAttr(f) => track!(self.format_module_attr(f)),
            Form::ExportAttr(f) => track!(self.format_export_attr(f)),
            Form::ExportTypeAttr(f) => unimplemented!(),
            Form::ImportAttr(f) => unimplemented!(),
            Form::FileAttr(f) => unimplemented!(),
            Form::WildAttr(f) => unimplemented!(),
            Form::FunSpec(f) => unimplemented!(),
            Form::CallbackSpec(f) => unimplemented!(),
            Form::FunDecl(f) => unimplemented!(),
            Form::RecordDecl(f) => unimplemented!(),
            Form::TypeDecl(f) => unimplemented!(),
        }
    }
    fn format_module_attr(&mut self, attr: forms::ModuleAttr) -> Result<()> {
        fmt!(self.output, "-module({}).", attr.module_name)?;
        self.input.consume_until(attr.end_position().offset());
        Ok(())
    }
    fn format_export_attr(&mut self, attr: forms::ExportAttr) -> Result<()> {
        fmt!(self.output, "-export([\n")?;
        fmt!(self.output, "        ]).")?;
        self.input.consume_until(attr.end_position().offset());
        Ok(())
    }
}
