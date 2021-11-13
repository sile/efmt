use crate::format::{self, Format, Formatter};
use crate::parse::Parse;
use crate::span::Span;

// TODO: delete or rename
#[derive(Debug, Clone, Span, Parse)]
pub struct Child<T>(T);

impl<T> Child<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for Child<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.with_subregion(format::RegionOptions::new(), |fmt| fmt.format_item(&self.0))
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ColumnIndent<T>(T);

impl<T: Format> Format for ColumnIndent<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.with_subregion(
            format::RegionOptions::new().indent(format::IndentMode::CurrentColumn),
            |fmt| fmt.format_item(&self.0),
        )
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
        fmt.with_subregion(
            format::RegionOptions::new()
                .newline()
                .indent(format::IndentMode::offset(4)),
            |fmt| fmt.format_item(&self.0),
        )
    }
}

// TODO: s/Space/Blank/
#[derive(Debug, Clone, Span, Parse)]
pub struct Space<T>(T);

impl<T> Space<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for Space<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.write_blank()?;
        fmt.format_item(&self.0)?;
        fmt.write_blank()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct RightSpace<T>(T);

impl<T: Format> Format for RightSpace<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.write_blank()?;
        Ok(())
    }
}

// TODO: remove
#[derive(Debug, Clone, Span, Parse)]
pub struct LeftSpace<T>(T);

impl<T: Format> Format for LeftSpace<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.write_blank()?;
        fmt.format_item(&self.0)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Newline<T>(T);

impl<T: Format> Format for Newline<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_item(&self.0)?;
        fmt.write_newline()?;
        Ok(())
    }
}
