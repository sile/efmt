use crate::format::{self, Format, Formatter};
use crate::items::styles::{Newline, TrailingColumns};
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CloseSquareSymbol, CommaSymbol, OpenBraceSymbol,
    OpenParenSymbol, OpenSquareSymbol, SemicolonSymbol,
};
use crate::items::tokens::WhitespaceToken;
use crate::parse::{self, Parse, ResumeParse, TokenStream};
use crate::span::{Position, Span};

#[derive(Debug, Clone)]
pub struct Maybe<T>(Either<T, WhitespaceToken>);

impl<T> Maybe<T> {
    pub fn is_none(&self) -> bool {
        matches!(self.0, Either::B(_))
    }

    pub fn from_item(item: T) -> Self {
        Self(Either::A(item))
    }

    pub fn from_position(position: Position) -> Self {
        Self(Either::B(WhitespaceToken::new(position, position)))
    }

    pub fn none(ts: &mut TokenStream) -> parse::Result<Self> {
        ts.current_whitespace_token().map(Either::B).map(Self)
    }

    pub fn get(&self) -> Option<&T> {
        if let Either::A(x) = &self.0 {
            Some(x)
        } else {
            None
        }
    }
}

impl<T: Span> Span for Maybe<T> {
    fn start_position(&self) -> Position {
        match &self.0 {
            Either::A(x) => x.start_position(),
            Either::B(x) => x.end_position(), // TODO: add note comment
        }
    }

    fn end_position(&self) -> Position {
        match &self.0 {
            Either::A(x) => x.end_position(),
            Either::B(x) => x.start_position(), // TODO: add note comment
        }
    }
}

impl<T: Parse> Parse for Maybe<T> {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        ts.parse()
            .map(Either::A)
            .or_else(|_| ts.current_whitespace_token().map(Either::B))
            .map(Self)
    }
}

impl<T: Format> Format for Maybe<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if let Some(x) = self.get() {
            x.format(fmt)?;
        }
        Ok(())
    }

    fn should_be_packed(&self) -> bool {
        self.get().map_or(true, |x| x.should_be_packed())
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
        self.open.format(fmt)?;
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns(1) // ')'
            .enter(|fmt| self.item.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Parenthesized2<T> {
    open: OpenParenSymbol,
    item: TrailingColumns<T, 1>, // trailing: ")"
    close: CloseParenSymbol,
}

impl<T> Parenthesized2<T> {
    pub fn get(&self) -> &T {
        self.item.get()
    }
}

impl<T: Format> Format for Parenthesized2<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.item.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Params<T>(Parenthesized2<Items2<T>>);

impl<T> Params<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().get()
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Args<T>(Parenthesized<Items<T>>);

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Args2<T>(Parenthesized2<Items2<T>>);

impl<T> Args2<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().get()
    }
}

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

impl<T: Format, D: Format> NonEmptyItems<T, D> {
    fn format_items(&self, fmt: &mut Formatter, multi_line: bool) -> format::Result<()> {
        for (item, delimiter) in self.items.iter().zip(self.delimiters.iter()) {
            item.format(fmt)?;
            delimiter.format(fmt)?;
            if multi_line {
                fmt.write_newline()?;
            } else {
                fmt.write_space()?;
            }
        }
        self.items.last().expect("unreachable").format(fmt)?;
        Ok(())
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if fmt
            .subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .enter(|fmt| self.format_items(fmt, false))
            .is_err()
        {
            self.format_items(fmt, true)?;
        }
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

        let max_columns = fmt.region_config().max_columns;
        fn is_head(fmt: &Formatter) -> bool {
            fmt.current_column() == fmt.region_config().indent
        }

        for (item, delimiter) in items.iter().zip(delimiters.iter()) {
            if !is_head(fmt) && fmt.current_column() + item.len() + delimiter.len() > max_columns {
                fmt.write_newline()?;
            }

            item.format(fmt)?;
            delimiter.format(fmt)?;
            fmt.write_space()?;
        }

        let item = items.last().expect("unreachable");
        if !is_head(fmt) && fmt.current_column() + item.len() > max_columns {
            fmt.write_newline()?;
        }
        item.format(fmt)?;

        Ok(())
    }
}

impl<T: Format> Format for Elements<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns(2) // '>>' or ']' or ... (use const generic?)
            .enter(|fmt| {
                let packed = self.0.get().iter().all(|x| x.should_be_packed());
                if packed {
                    self.format_packed_items(fmt)
                } else {
                    self.0.format(fmt)
                }
            })
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses<T>(NonEmptyItems<T, Newline<SemicolonSymbol>>);

impl<T: Format> Format for Clauses<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.0.format(fmt))
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
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let mut items = vec![ts.parse()?];
        while let Ok(item) = ts.parse() {
            items.push(item);
        }
        Ok(Self(items))
    }
}

impl<T: Format> Format for MaybeRepeat<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        for item in &self.0 {
            item.format(fmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListLike<T, D = CommaSymbol> {
    open: OpenSquareSymbol,
    items: TrailingColumns<MaybePackedItems<T, D>, 1>, // "]"
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleLike<T> {
    open: OpenBraceSymbol,
    items: TrailingColumns<MaybePackedItems<T>, 1>, // "}"
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MaybePackedItems<T, D = CommaSymbol>(Items2<T, D>);

impl<T: Format, D: Format> MaybePackedItems<T, D> {
    fn format_packed_items(&self, fmt: &mut Formatter) -> format::Result<()> {
        let (items, delimiters) = if let Some((items, delimiters)) =
            self.0 .0.get().map(|x| (&x.0.items, &x.0.delimiters))
        {
            (items, delimiters)
        } else {
            return Ok(());
        };

        let max_columns = fmt.region_config().max_columns;
        fn is_head(fmt: &Formatter) -> bool {
            fmt.current_column() == fmt.region_config().indent
        }

        for (item, delimiter) in items.iter().zip(delimiters.iter()) {
            if !is_head(fmt) && fmt.current_column() + item.len() + delimiter.len() > max_columns {
                fmt.write_newline()?;
            }

            item.format(fmt)?;
            delimiter.format(fmt)?;
            fmt.write_space()?;
        }

        // TODO: check trailing columns
        let item = items.last().expect("unreachable");
        if !is_head(fmt) && fmt.current_column() + item.len() > max_columns {
            fmt.write_newline()?;
        }
        item.format(fmt)?;

        Ok(())
    }
}

impl<T: Format, D: Format> Format for MaybePackedItems<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            let packed = self.0.get().iter().all(|x| x.should_be_packed());
            if packed {
                self.format_packed_items(fmt)
            } else {
                self.0.format(fmt)
            }
        })
    }
}

// TODO: refactor
#[derive(Debug, Clone, Span, Parse)]
pub struct NonEmptyItems2<T, D = CommaSymbol>(pub NonEmptyItems<T, D>); // TODO: pub

impl<T, D> NonEmptyItems2<T, D> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}

impl<T: Format, D: Format> NonEmptyItems2<T, D> {
    fn format_items(&self, fmt: &mut Formatter, multi_line: bool) -> format::Result<()> {
        for (item, delimiter) in self.0.items.iter().zip(self.0.delimiters.iter()) {
            fmt.subregion()
                .reset_trailing_columns(delimiter.len())
                .enter(|fmt| item.format(fmt))?;
            delimiter.format(fmt)?;
            if multi_line {
                fmt.write_newline()?;
            } else {
                fmt.write_space()?;
            }
        }
        fmt.subregion()
            .check_trailing_columns(true)
            .enter(|fmt| self.0.items.last().expect("unreachable").format(fmt))?;
        Ok(())
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems2<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            if fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .enter(|fmt| self.format_items(fmt, false))
                .is_err()
            {
                self.format_items(fmt, true)?;
            }
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses2<T, const N: usize>(NonEmptyItems2<T, Newline<SemicolonSymbol>>);

impl<T: Format, const N: usize> Format for Clauses2<T, N> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns2(N)
            .enter(|fmt| self.0.format(fmt))
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Items2<T, D = CommaSymbol>(Maybe<NonEmptyItems2<T, D>>);

impl<T, D> Items2<T, D> {
    pub fn get(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.get()
        } else {
            &[]
        }
    }
}

pub trait NeedsBeforeSpace {
    fn needs_before_space(&self, fmt: &format::Formatter) -> bool;
}

#[derive(Debug, Clone, Span, Parse)]
pub struct UnaryOpLike<O, T> {
    op: O,
    item: T,
}

impl<O: Format + NeedsBeforeSpace, T: Format> Format for UnaryOpLike<O, T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.op.needs_before_space(fmt) {
            fmt.write_space()?;
        }
        self.op.format(fmt)?;
        self.item.format(fmt)?;
        Ok(())
    }

    fn should_be_packed(&self) -> bool {
        self.item.should_be_packed()
    }
}

// TODO: rename
#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpLike<T0, O, T1, const I: usize> {
    left: T0,
    op: O,
    right: T1,
}

impl<T0: Parse, O: Parse, T1: Parse, const I: usize> ResumeParse<T0>
    for BinaryOpLike<T0, O, T1, I>
{
    fn resume_parse(ts: &mut parse::TokenStream, left: T0) -> parse::Result<Self> {
        Ok(Self {
            left,
            op: ts.parse()?,
            right: ts.parse()?,
        })
    }
}

impl<T0: Format, O: Format, T1: Format, const I: usize> Format for BinaryOpLike<T0, O, T1, I> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            fmt.subregion()
                .reset_trailing_columns(self.op.len())
                .enter(|fmt| self.left.format(fmt))?;
            self.op.format(fmt)?;

            if fmt.current_relative_column() <= I {
                // Inserting a newline cannot shorten the line length.
                self.right.format(fmt)?
            } else if fmt
                .subregion()
                .forbid_too_long_line()
                .forbid_multi_line()
                .check_trailing_columns(true)
                .enter(|fmt| self.right.format(fmt))
                .is_err()
            {
                fmt.subregion().indent_offset(I).enter(|fmt| {
                    fmt.write_newline()?;
                    self.right.format(fmt)
                })?;
            }
            Ok(())
        })
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MatchLike<L, D, R> {
    left: L,
    delimiter: D,
    right: R,
}

impl<L: Format, D: Format, R: Format> Format for MatchLike<L, D, R> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns2(self.delimiter.len())
            .enter(|fmt| self.left.format(fmt))?;
        self.delimiter.format(fmt)?;

        fmt.subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .check_trailing_columns(true)
            .enter(|fmt| self.right.format(fmt))
            .or_else(|_| {
                if fmt.current_relative_column() <= 4 {
                    self.right.format(fmt)
                } else {
                    fmt.subregion().indent_offset(4).enter(|fmt| {
                        fmt.write_newline()?;
                        self.right.format(fmt)
                    })
                }
            })?;

        Ok(())
    }
}

pub type UnbalancedBinaryOpLike<L, D, R> = MatchLike<L, D, R>;
