use crate::format::{self, Format, Formatter};
use crate::items::styles::Newline;
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

#[derive(Debug, Clone, Span, Parse)]
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

impl<T: Format> Format for Parenthesized<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.format_item(&self.open)?;
        fmt.with_subregion(
            format::RegionOptions::new()
                .indent(format::IndentMode::CurrentColumn)
                .trailing_item_size(1),
            |fmt| fmt.format_item(&self.item),
        )?;
        fmt.format_item(&self.close)?;
        Ok(())
    }
}

// TODO: remove?
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Args<T>(Parenthesized<Items<T>>);

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D = CommaSymbol> {
    // TODO: private
    pub items: Vec<T>,
    pub delimiters: Vec<D>,
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
            if fmt.multiline_mode().is_recommended() {
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

impl<T: Format> Elements<T> {
    fn format_packed_items(&self, fmt: &mut Formatter) -> format::Result<()> {
        let (items, delimiters) =
            if let Some((items, delimiters)) = self.0 .0.get().map(|x| (&x.items, &x.delimiters)) {
                (items, delimiters)
            } else {
                return Ok(());
            };

        let mut first = true;
        for (item, delimiter) in items.iter().zip(delimiters.iter()) {
            if !first && fmt.current_column() + item.len() + delimiter.len() > fmt.max_columns() {
                fmt.needs_newline();
            }
            first = false;
            fmt.format_item(item)?;
            fmt.format_item(delimiter)?;
            fmt.needs_space();
        }
        let item = items.last().expect("unreachable");
        if !first && fmt.current_column() + item.len() > fmt.max_columns() {
            fmt.needs_newline();
        }
        fmt.format_item(item)?;

        Ok(())
    }
}

impl<T: Format> Format for Elements<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.with_subregion(
            format::RegionOptions::new()
                .indent(format::IndentMode::CurrentColumn)
                .trailing_item_size(1), // TODO: maybe ">>"
            |fmt| {
                let packed = self.0.get().iter().all(|x| x.is_primitive());
                if packed {
                    self.format_packed_items(fmt)
                } else {
                    self.0.format(fmt)
                }
            },
        )
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses<T>(NonEmptyItems<T, Newline<SemicolonSymbol>>);

impl<T> Clauses<T> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}

impl<T: Format> Format for Clauses<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.with_subregion(
            format::RegionOptions::new().indent(format::IndentMode::CurrentColumn),
            |fmt| fmt.format_item(&self.0),
        )
    }
}

#[derive(Debug, Clone)]
pub struct MaybeRepeat<T>(Vec<T>);

impl<T> MaybeRepeat<T> {
    pub fn get(&self) -> &[T] {
        &self.0
    }
}

impl<T: Span> Span for MaybeRepeat<T> {
    fn start_position(&self) -> Position {
        self.0[0].start_position()
    }

    fn end_position(&self) -> Position {
        self.0[self.0.len() - 1].end_position()
    }
}

impl<T: Parse> Parse for MaybeRepeat<T> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let mut items = vec![parser.parse()?];
        while let Some(item) = parser.try_parse() {
            items.push(item);
        }
        Ok(Self(items))
    }
}

impl<T: Format> Format for MaybeRepeat<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        for item in &self.0 {
            fmt.format_item(item)?;
        }
        Ok(())
    }
}
