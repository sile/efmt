use crate::format::{self, Format, Formatter};
use crate::items::tokens::TokenStr;
use crate::parse::Parse;
use crate::span::{Position, Span};

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

// TODO: delete
impl<T: TokenStr> TokenStr for Space<T> {
    fn token_str(&self) -> &str {
        self.0.token_str()
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
            .trailing_columns(N)
            .check_trailing_columns(true) // TODO: delete
            .enter(|fmt| self.0.format(fmt))
    }
}
