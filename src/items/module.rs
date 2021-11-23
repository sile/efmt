use crate::format::{Format, Formatter};
use crate::items::Form;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};

/// [Form]*
#[derive(Debug, Clone, Span)]
pub struct Module {
    sof: Position,
    forms: Vec<Form>,
    eof: Position,
}

impl Parse for Module {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.prev_token_end_position();
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            forms.push(ts.parse()?);
        }
        let eof = ts.next_token_start_position()?;
        Ok(Self { sof, forms, eof })
    }
}

impl Format for Module {
    fn format(&self, fmt: &mut Formatter) {
        for form in &self.forms {
            form.format(fmt);
            fmt.add_newline();
        }
    }
}
