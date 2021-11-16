use crate::format::{self, Format, Formatter};
use crate::items::styles::{Newline, TrailingColumns};
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CommaSymbol, OpenBraceSymbol, OpenParenSymbol,
    SemicolonSymbol,
};
use crate::items::tokens::WhitespaceToken;
use crate::parse::{self, Parse, TokenStream};
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

impl<T> Clauses<T> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}

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
pub struct Tuple<T, const N: usize> {
    open: OpenBraceSymbol,
    items: TupleElements<T, N>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse)]
pub struct TupleElements<T, const N: usize>(Items<T>);

impl<T: Format, const N: usize> TupleElements<T, N> {
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

impl<T: Format, const N: usize> Format for TupleElements<T, N> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns(N) // TODO
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

// TODO: refactor
#[derive(Debug, Clone, Span, Parse)]
pub struct NonEmptyItems2<T, D>(NonEmptyItems<T, D>);

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
        dbg!(fmt.region_config());
        fmt.subregion()
            .check_trailing_columns(true)
            .enter(|fmt| self.0.items.last().expect("unreachable").format(fmt))?;
        Ok(())
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems2<T, D> {
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

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses2<T, const N: usize>(NonEmptyItems2<T, Newline<SemicolonSymbol>>);

impl<T, const N: usize> Clauses2<T, N> {
    pub fn get(&self) -> &[T] {
        self.0.get()
    }
}

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
