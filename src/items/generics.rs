use crate::format::{self, Format, Formatter};
use crate::items::symbols::{CloseParenSymbol, OpenParenSymbol};
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct Maybe<T> {
    item: Option<T>,
    prev_token_end_position: Position,
    next_token_start_position: Position,
}

impl<T> Maybe<T> {
    pub fn none(parser: &mut Parser) -> parse::Result<Self> {
        let prev_token_end_position = parser.prev_token_end_position()?;
        let next_token_start_position = parser.next_token_start_position()?;
        Ok(Self {
            item: None,
            prev_token_end_position,
            next_token_start_position,
        })
    }

    pub fn get(&self) -> Option<&T> {
        self.item.as_ref()
    }
}

impl<T> Span for Maybe<T> {
    fn start_position(&self) -> Position {
        self.next_token_start_position
    }

    fn end_position(&self) -> Position {
        self.prev_token_end_position
    }
}

impl<T: Parse> Parse for Maybe<T> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let prev_token_end_position = parser.prev_token_end_position()?;
        let next_token_start_position = parser.next_token_start_position()?;
        Ok(Self {
            item: parser.try_parse(),
            prev_token_end_position,
            next_token_start_position,
        })
    }
}

impl<T: Format> Format for Maybe<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        if let Some(x) = &self.item {
            fmt.format(x)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Parenthesized<T> {
    open: OpenParenSymbol,
    item: T,
    close: CloseParenSymbol,
}

impl<T> Parenthesized<T> {
    pub fn get(&self) -> &T {
        &self.item
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D> {
    items: Vec<T>,
    delimiters: Vec<D>,
}

impl<T, D> NonEmptyItems<T, D> {
    pub fn get(&self) -> &[T] {
        &self.items
    }
}

impl<T: Span, D: Span> Span for NonEmptyItems<T, D> {
    fn start_position(&self) -> Position {
        self.items[0].start_position()
    }

    fn end_position(&self) -> Position {
        self.items[self.items.len() - 1].end_position()
    }
}

impl<T: Parse, D: Parse> Parse for NonEmptyItems<T, D> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let mut items = vec![parser.parse()?];
        let mut delimiters = Vec::new();
        while let Some(delimiter) = parser.try_parse() {
            delimiters.push(delimiter);
            items.push(parser.parse()?);
        }
        Ok(Self { items, delimiters })
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems<T, D> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.items[0])?;
        for (delimiter, item) in self.delimiters.iter().zip(self.items.iter().skip(1)) {
            fmt.format(delimiter)?;
            fmt.format(item)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Items<T, D>(Maybe<NonEmptyItems<T, D>>);

impl<T, D> Items<T, D> {
    pub fn get(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.get()
        } else {
            &[]
        }
    }
}
