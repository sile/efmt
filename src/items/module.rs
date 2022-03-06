use crate::format::{Format, Formatter};
use crate::items::Form;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use std::num::NonZeroUsize;

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
        let three = NonZeroUsize::new(3).expect("unreachable");
        let mut is_last_spec = false;

        for form in &self.forms {
            if form.is_func_decl() && !is_last_spec {
                fmt.add_newlines(three);
            }

            is_last_spec = form.is_func_spec();
            if form.is_func_spec() {
                fmt.add_newlines(three);
            }

            form.format(fmt);
            fmt.add_newline();
        }
    }
}
