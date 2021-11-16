use crate::format::{self, Format};
use crate::items::forms::Form;
use crate::items::styles::Newline;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};

/// [Form]*
#[derive(Debug, Clone, Span)]
pub struct Module {
    sof: Position,
    forms: Vec<Newline<Form>>,
    eof: Position,
}

impl Parse for Module {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.current_whitespace_token()?.start_position();
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            forms.push(ts.parse()?);
        }
        let eof = ts.current_whitespace_token()?.end_position();
        Ok(Self { sof, forms, eof })
    }
}

impl Format for Module {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        for form in &self.forms {
            form.format(fmt)?;
        }
        Ok(())
    }
}
