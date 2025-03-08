use crate::format::{Format, Formatter};
use crate::items::keywords::WhenKeyword;
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CloseSquareSymbol, CommaSymbol, DoubleLeftAngleSymbol,
    DoubleRightAngleSymbol, DoubleRightArrowSymbol, MapMatchSymbol, OpenBraceSymbol,
    OpenParenSymbol, OpenSquareSymbol, SemicolonSymbol,
};
use crate::items::tokens::AtomToken;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};

pub use efmt_derive::Element;

#[derive(Debug, Clone)]
pub struct Never;

impl Span for Never {
    fn start_position(&self) -> Position {
        unreachable!()
    }

    fn end_position(&self) -> Position {
        unreachable!()
    }
}

impl Parse for Never {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let token = ts.parse()?;
        Err(parse::Error::unexpected_token(ts, token))
    }
}

impl Format for Never {
    fn format(&self, _: &mut Formatter) {
        unreachable!()
    }
}

#[derive(Debug, Clone, Span)]
pub struct Null {
    // Note that `next_token_start_position` can be larger than `prev_token_end_position`
    // because this behavior is required when using `Null` in other items
    // (please imagine the case where `Null` is at the front (or last) of an item and
    // `#[derive(Format)]` is specified on that item).
    next_token_start_position: Position,
    prev_token_end_position: Position,
}

impl Parse for Null {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        Ok(Self {
            next_token_start_position: ts.next_token_start_position()?,
            prev_token_end_position: ts.prev_token_end_position(),
        })
    }
}

impl Format for Null {
    fn format(&self, _: &mut Formatter) {}
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Maybe<T>(Either<T, Null>);

impl<T> Maybe<T> {
    pub fn some(item: T) -> Self {
        Self(Either::A(item))
    }

    pub fn parse_none(ts: &mut TokenStream) -> parse::Result<Self> {
        ts.parse().map(Either::B).map(Self)
    }

    pub fn none_from_position(position: Position) -> Self {
        Self(Either::B(Null {
            prev_token_end_position: position,
            next_token_start_position: position,
        }))
    }

    pub fn get(&self) -> Option<&T> {
        if let Either::A(x) = &self.0 {
            Some(x)
        } else {
            None
        }
    }
}

impl<T: Element> Element for Maybe<T> {
    fn is_packable(&self) -> bool {
        self.get().is_none_or(Element::is_packable)
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> Iterator for Either<A, B>
where
    A: Iterator<Item = B::Item>,
    B: Iterator,
{
    type Item = A::Item;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Either::A(x) => x.next(),
            Either::B(x) => x.next(),
        }
    }
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
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.item.format(fmt);
            fmt.set_next_comment_indent(fmt.indent());
        });
        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Params<T>(Parenthesized<Items<T>>);

impl<T> Params<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Args<T>(Parenthesized<Items<T>>);

impl<T> Args<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

impl<T: Format> Format for Args<T> {
    fn format(&self, fmt: &mut Formatter) {
        self.0.open.format(fmt);
        if !self.get().is_empty() && fmt.has_newline_until(&self.0.item) {
            fmt.set_indent(fmt.indent() + 2);
            fmt.write_newline();
        }
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.0.item.format(fmt);
            fmt.set_next_comment_indent(fmt.indent());
        });
        self.0.close.format(fmt);
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D = CommaSymbol> {
    items: Vec<T>,
    delimiters: Vec<D>,
}

impl<T, D> NonEmptyItems<T, D> {
    pub fn items(&self) -> &[T] {
        &self.items
    }

    pub fn delimiters(&self) -> &[D] {
        &self.delimiters
    }
}

impl<T: Span, D> Span for NonEmptyItems<T, D> {
    fn start_position(&self) -> Position {
        self.items[0].start_position()
    }

    fn end_position(&self) -> Position {
        self.items[self.items.len() - 1].end_position()
    }
}

impl<T: Parse, D: Parse> Parse for NonEmptyItems<T, D> {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let mut items = vec![ts.parse()?];
        let mut delimiters = Vec::new();
        while let Ok(delimiter) = ts.parse() {
            delimiters.push(delimiter);
            items.push(ts.parse()?);
        }
        Ok(Self { items, delimiters })
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems<T, D> {
    fn format(&self, fmt: &mut Formatter) {
        if self.contains_newline() {
            self.format_items(fmt);
        } else {
            fmt.with_single_line_mode(|fmt| {
                self.format_items(fmt);
            });
        }
    }
}

impl<T: Format, D: Format> NonEmptyItems<T, D> {
    fn format_items(&self, fmt: &mut Formatter) {
        format_non_empty_items(fmt, self.items().iter(), self.delimiters.iter());
    }
}

fn format_non_empty_items<T, D>(
    fmt: &mut Formatter,
    mut items: impl Iterator<Item = T>,
    delimiters: impl Iterator<Item = D>,
) where
    T: Format,
    D: Format,
{
    fmt.with_scoped_indent(|fmt| {
        fmt.set_indent(fmt.column());

        let item = items.next().expect("unreachable");
        item.format(fmt);
        for (item, delimiter) in items.zip(delimiters) {
            delimiter.format(fmt);
            fmt.write_newline();
            item.format(fmt);
        }
    });
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Items<T, D = CommaSymbol>(Maybe<NonEmptyItems<T, D>>);

impl<T, D> Items<T, D> {
    pub fn items(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.items()
        } else {
            &[]
        }
    }

    pub fn delimiters(&self) -> &[D] {
        if let Some(x) = self.0.get() {
            x.delimiters()
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MaybePackedItems<T, D = CommaSymbol>(Items<T, D>);

impl<T, D> MaybePackedItems<T, D> {
    pub(crate) fn items(&self) -> &[T] {
        self.0.items()
    }

    fn delimiters(&self) -> &[D] {
        self.0.delimiters()
    }
}

impl<T: Format, D: Format> MaybePackedItems<T, D> {
    fn packed_format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            let item = self.0.items().first().expect("unreachable");
            item.format(fmt);
            for (item, delimiter) in self
                .0
                .items()
                .iter()
                .skip(1)
                .zip(self.0.delimiters().iter())
            {
                let newline = fmt.has_newline_until(item);
                delimiter.format(fmt);
                if newline {
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                item.format(fmt);
            }
        });
    }
}

impl<T: Format + Element, D: Format> Format for MaybePackedItems<T, D> {
    fn format(&self, fmt: &mut Formatter) {
        if self.0.items().is_empty() {
        } else if self.0.items().iter().all(Element::is_packable) {
            fmt.with_scoped_indent(|fmt| {
                fmt.set_indent(fmt.column());
                self.packed_format(fmt);
            });
        } else {
            self.0.format(fmt);
        }
    }
}

pub trait Element {
    fn is_packable(&self) -> bool;
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ListLike<T: Element, D = CommaSymbol> {
    open: OpenSquareSymbol,
    items: MaybePackedItems<T, D>,
    close: CloseSquareSymbol,
}

impl<T: Element, D> ListLike<T, D> {
    pub(crate) fn items(&self) -> &[T] {
        self.items.items()
    }
}

impl<T: Element + Format, D: Format> Format for ListLike<T, D> {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        self.items.format(fmt);
        fmt.set_next_comment_indent(fmt.indent() + 1);
        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct TupleLike<T: Element> {
    open: OpenBraceSymbol,
    tag: Maybe<(AtomToken, CommaSymbol)>,
    items: MaybePackedItems<T, CommaSymbol>,
    close: CloseBraceSymbol,
}

impl<T: Element + Format> TupleLike<T> {
    pub(crate) fn items(&self) -> (Option<&AtomToken>, &[T]) {
        (self.tag.get().map(|(x, _)| x), self.items.items())
    }

    pub(crate) fn try_format_app_file(&self, fmt: &mut Formatter) -> bool {
        if fmt.indent() != 0 {
            return false;
        }

        match self.items() {
            (Some(tag), items) if tag.value() == "application" && items.len() == 2 => {
                fmt.with_scoped_indent(|fmt| {
                    self.open.format(fmt);
                    self.tag.format(fmt);
                    fmt.write_space();

                    items[0].format(fmt);
                    self.items.0.delimiters()[0].format(fmt);
                    fmt.set_indent(1);
                    fmt.write_newline();

                    items[1].format(fmt);
                    self.close.format(fmt);
                });
                return true;
            }
            _ => {}
        }
        false
    }
}

impl<T: Element + Format> Format for TupleLike<T> {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        if let Some(tag) = self.tag.get() {
            if fmt.has_newline_until(&self.items) {
                // Not a tagged tuple
                let items_indent = fmt.column();
                format_non_empty_items(
                    fmt,
                    std::iter::once(Either::A(&tag.0))
                        .chain(self.items.items().iter().map(Either::B)),
                    std::iter::once(Either::A(&tag.1))
                        .chain(self.items.delimiters().iter().map(Either::B)),
                );
                fmt.set_next_comment_indent(items_indent);
                self.close.format(fmt);
                return;
            }

            tag.format(fmt);
            fmt.write_space();
        }

        let items_indent = fmt.column();
        self.items.format(fmt);
        fmt.set_next_comment_indent(items_indent);
        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BitstringLike<T: Element> {
    open: DoubleLeftAngleSymbol,
    items: MaybePackedItems<T>,
    close: DoubleRightAngleSymbol,
}

impl<T: Element> BitstringLike<T> {
    pub(crate) fn items(&self) -> &[T] {
        self.items.items()
    }
}

impl<T: Element + Format> Format for BitstringLike<T> {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        self.items.format(fmt);
        fmt.set_next_comment_indent(fmt.indent() + 1);
        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapLike<Prefix, Item> {
    inner: RecordLike<Prefix, MapItem<Item>>,
}

impl<Prefix, Item> MapLike<Prefix, Item> {
    pub fn new(prefix: Prefix, items: RecordFieldsLike<MapItem<Item>>) -> Self {
        Self {
            inner: RecordLike::new(prefix, items),
        }
    }

    pub(crate) fn prefix(&self) -> &Prefix {
        &self.inner.prefix
    }

    pub(crate) fn items(&self) -> impl Iterator<Item = (&Item, &Item)> {
        self.inner
            .fields
            .fields
            .items()
            .iter()
            .map(|x| (&x.key, &x.value))
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MapItem<T> {
    key: T,
    delimiter: Either<DoubleRightArrowSymbol, MapMatchSymbol>,
    value: T,
}

impl<T> Element for MapItem<T> {
    fn is_packable(&self) -> bool {
        false
    }
}

impl<T: Format> Format for MapItem<T> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            self.key.format(fmt);

            let multiline = fmt.has_newline_until(&self.value);
            fmt.write_space();
            self.delimiter.format(fmt);

            if multiline {
                fmt.set_indent(fmt.indent() + 4);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }
            self.value.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct RecordLike<Prefix, Field> {
    prefix: Prefix,
    fields: RecordFieldsLike<Field>,
}

impl<Prefix, Field> RecordLike<Prefix, Field> {
    pub(crate) fn new(prefix: Prefix, fields: RecordFieldsLike<Field>) -> Self {
        Self { prefix, fields }
    }

    pub(crate) fn prefix(&self) -> &Prefix {
        &self.prefix
    }

    pub(crate) fn fields(&self) -> impl Iterator<Item = &Field> {
        self.fields.fields.items().iter()
    }
}

impl<Prefix, Field> Format for RecordLike<Prefix, Field>
where
    Prefix: Format,
    Field: Format + Element,
{
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.prefix.format(fmt);
            self.fields.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct RecordFieldsLike<T> {
    open: OpenBraceSymbol,
    fields: Items<T>,
    close: CloseBraceSymbol,
}

impl<T> RecordFieldsLike<T> {
    pub(crate) fn get(&self) -> &[T] {
        self.fields.items()
    }
}

impl<T: Format> Format for RecordFieldsLike<T> {
    fn format(&self, fmt: &mut Formatter) {
        let multiline = self.contains_newline();
        fmt.with_scoped_indent(|fmt| {
            let base_indent = fmt.indent();
            self.open.format(fmt);
            if !self.fields.items().is_empty() {
                if multiline {
                    fmt.set_indent(base_indent + 2);
                    fmt.write_newline();
                }
                self.fields.format(fmt);
            }

            if multiline {
                fmt.set_indent(base_indent + 1);
                fmt.write_newline();
            }

            fmt.set_next_comment_indent(base_indent + 2);
            self.close.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Clauses<T>(NonEmptyItems<T, SemicolonSymbol>);

impl<T> Clauses<T> {
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.0.items().iter()
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Guard<T, D = GuardDelimiter> {
    when: WhenKeyword,
    conditions: NonEmptyItems<T, D>,
}

impl<T, D> Guard<T, D> {
    pub fn conditions(&self) -> &NonEmptyItems<T, D> {
        &self.conditions
    }

    pub fn children(&self) -> impl Iterator<Item = &T> {
        self.conditions.items().iter()
    }
}

impl<T: Format, D: Format> Format for Guard<T, D> {
    fn format(&self, fmt: &mut Formatter) {
        self.when.format(fmt);
        fmt.write_space();
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.conditions.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct GuardDelimiter(Either<CommaSymbol, SemicolonSymbol>);
