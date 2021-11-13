use crate::format::{self, Format, Formatter};
use crate::items::forms::Form;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};

#[derive(Debug, Clone)]
pub struct Module {
    forms: Vec<Form>,
    eof: Eof,
}

impl Span for Module {
    fn start_position(&self) -> Position {
        Position::new(0, 1, 1)
    }

    fn end_position(&self) -> Position {
        self.eof.position
    }
}

impl Parse for Module {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            forms.push(ts.parse()?);
        }
        let position = ts.current_whitespace_token()?.end_position();
        Ok(Self {
            forms,
            eof: Eof { position },
        })
    }
}

impl Format for Module {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        for form in &self.forms {
            fmt.with_subregion(format::RegionOptions::new(), |fmt| fmt.format_item(form))?;
            fmt.write_newline()?;
        }
        fmt.format_item(&self.eof)?; // For comments and empty macros
        Ok(())
    }
}

// TODO: delete?
#[derive(Debug, Clone)]
pub struct Eof {
    pub position: Position,
}

impl Span for Eof {
    fn start_position(&self) -> Position {
        self.position
    }

    fn end_position(&self) -> Position {
        self.position
    }
}

impl Format for Eof {
    fn format(&self, _fmt: &mut Formatter) -> format::Result<()> {
        Ok(())
    }
}
