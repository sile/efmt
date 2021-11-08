use crate::format::{self, Format, Formatter};
use crate::items::forms::Form;
use crate::parse::{self, Parse, Parser};
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
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let mut forms = Vec::new();
        while !parser.is_eof()? {
            forms.push(parser.parse()?);
        }
        let position = parser.next_token_start_position()?;
        Ok(Self {
            forms,
            eof: Eof { position },
        })
    }
}

impl Format for Module {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        for form in &self.forms {
            fmt.format_item(form)?;
            fmt.needs_newline();
        }
        fmt.format_item(&self.eof)?; // For comments and empty macros
        Ok(())
    }
}

#[derive(Debug, Clone)]
struct Eof {
    position: Position,
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
