use crate::format::{self, Format};
use crate::items::tokens::Token;
use crate::items::Form;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use std::ops::Range;

/// [Form]*
#[derive(Debug, Clone, Span)]
pub struct Module {
    sof: Position,
    forms: Vec<Form>,
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
            fmt.write_newline()?;
        }
        Ok(())
    }
}

// TODO: delete
/// ([Form] | [Token])*
#[derive(Debug, Clone, Span)]
pub struct LooseModule {
    sof: Position,
    forms: Vec<FormOrToken>,
    eof: Position,
}

impl Parse for LooseModule {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.current_whitespace_token()?.start_position();
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            if let Ok(form) = ts.parse() {
                forms.push(FormOrToken::Form(form));
            } else if let Some(FormOrToken::Token(range)) = forms.last_mut() {
                let token: Token = ts.parse()?;
                range.end = token.end_position();
            } else {
                let token: Token = ts.parse()?;
                forms.push(FormOrToken::Token(Range {
                    start: token.start_position(),
                    end: token.end_position(),
                }));
            }
        }
        let eof = ts.current_whitespace_token()?.end_position();
        Ok(Self { sof, forms, eof })
    }
}

impl Format for LooseModule {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        for form in &self.forms {
            match form {
                FormOrToken::Form(x) => x.format(fmt)?,
                FormOrToken::Token(x) => {
                    log::warn!("malformed tokens: {:?}", x);
                    fmt.write_text(x)?
                }
            }
            fmt.write_newline()?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span)]
enum FormOrToken {
    Form(Form),
    Token(Range<Position>),
}
