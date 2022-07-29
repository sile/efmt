use crate::format::{Format, Formatter};
use crate::items::keywords::WhenKeyword;
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CloseSquareSymbol, CommaSymbol, DoubleLeftAngleSymbol,
    DoubleRightAngleSymbol, DoubleRightArrowSymbol, MapMatchSymbol, OpenBraceSymbol,
    OpenParenSymbol, OpenSquareSymbol, SemicolonSymbol,
};
use crate::items::tokens::AtomToken;
use crate::parse::{self, Parse, ResumeParse, TokenStream};
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
        self.get().map_or(true, Element::is_packable)
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
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.item.format(fmt);
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

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Args<T>(Parenthesized<Items<T>>);

impl<T> Args<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

// TODO: delete
#[derive(Debug, Clone, Span, Parse)]
pub struct CommaDelimiter(CommaSymbol);

impl Format for CommaDelimiter {
    fn format(&self, fmt: &mut Formatter) {
        self.0.format(fmt);
        fmt.write_space();
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
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            if self.contains_newline() {
                self.format_items(fmt);
            } else {
                fmt.with_single_line_mode(|fmt| {
                    self.format_items(fmt);
                });
            }
        });
    }
}

impl<T: Format, D: Format> NonEmptyItems<T, D> {
    fn format_items(&self, fmt: &mut Formatter) {
        let item = self.items().first().expect("unreachable");
        item.format(fmt);
        for (item, delimiter) in self.items.iter().skip(1).zip(self.delimiters.iter()) {
            delimiter.format(fmt);
            fmt.write_newline();
            item.format(fmt);
        }
    }
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
struct MaybePackedItems<T, D = CommaSymbol>(Items<T, D>);

impl<T, D> MaybePackedItems<T, D> {
    pub(crate) fn items(&self) -> &[T] {
        self.0.items()
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

#[derive(Debug, Clone, Span, Parse, Format)]
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

#[derive(Debug, Clone, Span, Parse)]
pub struct TupleLike<T: Element> {
    open: OpenBraceSymbol,
    tag: Maybe<(AtomToken, CommaSymbol)>,
    items: MaybePackedItems<T, CommaSymbol>,
    close: CloseBraceSymbol,
}

impl<T: Element> TupleLike<T> {
    pub(crate) fn items(&self) -> (Option<&AtomToken>, &[T]) {
        (self.tag.get().map(|(x, _)| x), self.items.items())
    }
}

impl<T: Element + Format> Format for TupleLike<T> {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        if let Some(tag) = self.tag.get() {
            tag.format(fmt);
            fmt.write_space();
        }
        self.items.format(fmt);
        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringLike<T: Element> {
    open: DoubleLeftAngleSymbol,
    items: MaybePackedItems<T>,
    close: DoubleRightAngleSymbol,
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

impl<T: Format> Format for RecordFieldsLike<T> {
    fn format(&self, fmt: &mut Formatter) {
        let multiline = self.contains_newline();
        fmt.with_scoped_indent(|fmt| {
            let base_indent = fmt.indent();
            self.open.format(fmt);
            if self.fields.items().len() > 0 {
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
            self.close.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Clauses<T>(NonEmptyItems<T, SemicolonSymbol>);

// TODO: delete
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpLike<O, T> {
    op: O,
    item: T,
}

// TODO: delete(?)
pub trait BinaryOpStyle<RHS> {
    fn needs_spaces(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpLike<L, O, R> {
    pub left: L,
    pub op: O,
    pub right: R,
}

impl<L, O, R> Element for BinaryOpLike<L, O, R> {
    fn is_packable(&self) -> bool {
        false
    }
}

impl<L: Parse, O: Parse, R: Parse> ResumeParse<L> for BinaryOpLike<L, O, R> {
    fn resume_parse(ts: &mut parse::TokenStream, left: L) -> parse::Result<Self> {
        Ok(Self {
            left,
            op: ts.parse()?,
            right: ts.parse()?,
        })
    }
}

impl<L: Format, O: Format + BinaryOpStyle<R>, R: Format> Format for BinaryOpLike<L, O, R> {
    fn format(&self, fmt: &mut Formatter) {
        self.left.format(fmt);

        if self.op.needs_spaces() {
            fmt.write_space();
            self.op.format(fmt);
            fmt.write_space();
        } else {
            self.op.format(fmt);
        }

        // let indent = self.op.indent(); TODO
        self.right.format(fmt);
    }
}

// TODO: remove OFFSET
#[derive(Debug, Clone, Span, Parse)]
pub struct Guard<T, D = GuardDelimiter, const OFFSET: usize = 2> {
    when: WhenKeyword,
    conditions: NonEmptyItems<T, D>,
}

impl<T: Format, D: Format, const OFFSET: usize> Format for Guard<T, D, OFFSET> {
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
