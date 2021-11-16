use crate::format::{self, Format, Formatter};
use crate::parse::Parse;
use crate::span::Span;

// TODO: delete or rename
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Child<T>(pub T); // TODO: private

impl<T> Child<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ColumnIndent<T>(T);

impl<T: Format> Format for ColumnIndent<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.0.format(fmt))
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Block<T>(T);

impl<T> Block<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for Block<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().indent_offset(4).enter(|fmt| {
            fmt.write_newline()?;
            self.0.format(fmt)
        })
    }
}

// TODO: implement Span::len method?
#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T> Space<T> {
    pub fn get(&self) -> &T {
        &self.0
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
