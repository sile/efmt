use crate::format::Item;
use crate::items::styles::{Indent, Newline, Space};
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

impl<T: Item> Item for Maybe<T> {
    fn children(&self) -> Vec<&dyn Item> {
        if let Some(x) = self.get() {
            x.children()
        } else {
            Vec::new()
        }
    }

    fn indent_offset(&self) -> usize {
        self.get().map_or(0, |x| x.indent_offset())
    }

    fn prefers_oneline(&self) -> bool {
        self.get().map_or(false, |x| x.prefers_oneline())
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct Parenthesized<T> {
    open: OpenParenSymbol,
    item: Indent<T, 4>,
    close: CloseParenSymbol,
}

impl<T> Parenthesized<T> {
    pub fn get(&self) -> &T {
        self.item.get()
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D = Space<CommaSymbol>> {
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

impl<T: Item, D: Item> Item for NonEmptyItems<T, D> {
    fn children(&self) -> Vec<&dyn Item> {
        // TODO: change struct layout like `items: Vec<(T, D)>, last_item: T`
        let mut children = vec![&self.items[0] as &dyn Item];
        for (delimiter, item) in self.delimiters.iter().zip(self.items.iter().skip(1)) {
            children.push(delimiter);
            children.push(item);
        }
        children
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct Items<T, D = Space<CommaSymbol>>(Maybe<NonEmptyItems<T, D>>);

impl<T, D> Items<T, D> {
    pub fn get(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.get()
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Item)]
pub struct Clauses<T>(NonEmptyItems<T, Newline<SemicolonSymbol>>);

impl<T> Clauses<T> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}
