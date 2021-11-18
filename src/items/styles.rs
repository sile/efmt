use crate::format::{self, Format, Formatter};
use crate::parse::Parse;
use crate::span::{Position, Span};

#[derive(Debug, Clone, Span, Parse)]
pub struct ColumnIndent<T>(T);

impl<T: Format> Format for ColumnIndent<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.0.format(fmt))
    }
}

// TODO: move to `blocks`
#[derive(Debug, Clone, Span, Parse)]
pub struct Block<T>(T);

impl<T: Format> Format for Block<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().indent_offset(4).enter(|fmt| {
            fmt.write_newline()?;
            self.0.format(fmt)?;
            fmt.write_newline()?;
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Parse)]
pub struct Space<T>(T);

impl<T> Space<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Span> Span for Space<T> {
    fn start_position(&self) -> Position {
        self.0.start_position()
    }

    fn end_position(&self) -> Position {
        self.0.end_position()
    }

    fn len(&self) -> usize {
        if self.0.is_empty() {
            0
        } else {
            self.0.len() + 1
        }
    }
}

impl<T: Format> Format for Space<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.write_space()?;
        self.0.format(fmt)?;
        fmt.write_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct RightSpace<T>(T);

impl<T> RightSpace<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for RightSpace<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.0.format(fmt)?;
        fmt.write_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

impl<T: Format> Format for Newline<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.0.format(fmt)?;
        fmt.write_newline()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct TrailingColumns<T, const N: usize>(T);

impl<T, const N: usize> TrailingColumns<T, N> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format, const N: usize> Format for TrailingColumns<T, N> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .trailing_columns2(N)
            .check_trailing_columns(true) // TODO: delete
            .enter(|fmt| self.0.format(fmt))
    }
}
