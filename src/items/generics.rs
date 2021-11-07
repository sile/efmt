use crate::format::{self, Format, Formatter};
use crate::items::styles::{Child, Newline};
use crate::items::symbols::{CloseParenSymbol, CommaSymbol, OpenParenSymbol, SemicolonSymbol};
use crate::parse::{self, Parse, Parser};
use crate::span::{Position, Span};

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
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if let Some(x) = self.get() {
            x.format(fmt)?;
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
    item: Child<T>,
    close: CloseParenSymbol,
}

impl<T> Parenthesized<T> {
    pub fn get(&self) -> &T {
        self.item.get()
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Args<T> {
    open: OpenParenSymbol,
    items: Items<T>,
    close: CloseParenSymbol,
}

impl<T: Format> Format for Args<T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.format_item(&self.open)?;

        fmt.format_child_item(&self.items)?;
        // // TODO: maybe very inefficient
        // fmt.format_either_child_item(
        //     &self.items,
        //     format::ChildOptions::new(),
        //     format::ChildOptions::new().newline().multiline_mode(),
        // )?;

        fmt.format_item(&self.close)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D = CommaSymbol> {
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
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        for (item, delimiter) in self.items.iter().zip(self.delimiters.iter()) {
            fmt.format_item(item)?;
            fmt.format_item(delimiter)?;
            if fmt.multiline_mode() {
                fmt.needs_newline();
            } else {
                fmt.needs_space();
            }
        }
        fmt.format_item(self.items.last().expect("unreachable"))?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Items<T, D = CommaSymbol>(Maybe<NonEmptyItems<T, D>>);

impl<T, D> Items<T, D> {
    pub fn get(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.get()
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Elements<T>(Items<T>);

impl<T: Format> Format for Elements<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        let packed = self.0.get().iter().all(|x| x.is_primitive());
        if !packed {
            self.0.format(fmt)?;
        } else if let Some((items, delimiters)) = self.0 .0.get().map(|x| (&x.items, &x.delimiters))
        {
            for (item, delimiter) in items.iter().zip(delimiters.iter()) {
                if fmt.current_columns() + item.len() + delimiter.len() > fmt.max_columns() {
                    fmt.needs_newline();
                }
                fmt.format_item(item)?;
                fmt.format_item(delimiter)?;
                fmt.needs_space();
            }
            let item = items.last().expect("unreachable");
            if fmt.current_columns() + item.len() > fmt.max_columns() {
                fmt.needs_newline();
            }
            fmt.format_item(item)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Clauses<T>(NonEmptyItems<T, Newline<SemicolonSymbol>>);

impl<T> Clauses<T> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}
