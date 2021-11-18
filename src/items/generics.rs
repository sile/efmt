use crate::format::{self, Format, Formatter};
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CloseSquareSymbol, CommaSymbol, OpenBraceSymbol,
    OpenParenSymbol, OpenSquareSymbol, SemicolonSymbol,
};
use crate::items::tokens::{TokenStr, WhitespaceToken};
use crate::parse::{self, Parse, ResumeParse, TokenStream};
use crate::span::{Position, Span};

#[derive(Debug, Clone)]
pub struct Null(WhitespaceToken);

impl Span for Null {
    fn start_position(&self) -> Position {
        self.0.end_position() // TODO: note
    }

    fn end_position(&self) -> Position {
        self.0.start_position() // TODO: note
    }
}

impl Parse for Null {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        ts.current_whitespace_token().map(Self)
    }
}

impl Format for Null {
    fn format(&self, _: &mut format::Formatter) -> format::Result<()> {
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Maybe<T>(Either<T, Null>);

impl<T> Maybe<T> {
    pub fn is_none(&self) -> bool {
        matches!(self.0, Either::B(_))
    }

    pub fn from_item(item: T) -> Self {
        Self(Either::A(item))
    }

    pub fn from_position(position: Position) -> Self {
        Self(Either::B(Null(WhitespaceToken::new(position, position))))
    }

    pub fn none(ts: &mut TokenStream) -> parse::Result<Self> {
        ts.current_whitespace_token()
            .map(Null)
            .map(Either::B)
            .map(Self)
    }

    pub fn get(&self) -> Option<&T> {
        if let Either::A(x) = &self.0 {
            Some(x)
        } else {
            None
        }
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

impl<A: TokenStr, B: TokenStr> TokenStr for Either<A, B> {
    fn token_str(&self) -> &str {
        match self {
            Self::A(x) => x.token_str(),
            Self::B(x) => x.token_str(),
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
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .current_column_as_indent()
            .trailing_columns(1) // ")"
            .enter(|fmt| self.item.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
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

impl<T: Format, D: Format + TokenStr> NonEmptyItems<T, D> {
    fn format_items(&self, fmt: &mut Formatter, multi_line: bool) -> format::Result<()> {
        for (item, delimiter) in self.items.iter().zip(self.delimiters.iter()) {
            fmt.subregion()
                .reset_trailing_columns(delimiter.token_str().len())
                .enter(|fmt| item.format(fmt))?;
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

impl<T: Format, D: Format + TokenStr> Format for NonEmptyItems<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            fmt.subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .check_trailing_columns(true)
                .enter(|fmt| self.format_items(fmt, false))
                .or_else(|_| self.format_items(fmt, true))
        })
    }
}

#[derive(Debug, Clone, Span, Parse)]
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

impl<T: Format, D: Format + TokenStr> Format for Items<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.0.format(fmt)
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct MaybePackedItems<T, D = CommaSymbol>(Items<T, D>);

impl<T: Format, D: Format> MaybePackedItems<T, D> {
    fn format_packed_item(
        &self,
        fmt: &mut Formatter,
        item: &T,
        delimiter: Option<&D>,
    ) -> format::Result<()> {
        let format = |fmt: &mut Formatter| {
            item.format(fmt)?;
            if let Some(d) = delimiter {
                d.format(fmt)?;
                fmt.write_space()?;
            }
            Ok(())
        };

        let is_head = fmt.current_column() == fmt.region_config().indent;
        if is_head {
            format(fmt)
        } else {
            fmt.subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .check_trailing_columns(delimiter.is_none())
                .enter(format)
                .or_else(|_| {
                    fmt.write_newline()?;
                    format(fmt)
                })
        }
    }

    fn format_packed_items(&self, fmt: &mut Formatter) -> format::Result<()> {
        for (item, delimiter) in self.0.items().iter().zip(self.0.delimiters().iter()) {
            self.format_packed_item(fmt, item, Some(delimiter))?;
        }

        let item = self.0.items().last().expect("unreachable");
        self.format_packed_item(fmt, item, None)?;

        Ok(())
    }
}

impl<T: Format, D: Format + TokenStr> Format for MaybePackedItems<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        if self.0.items().is_empty() {
            Ok(())
        } else if self.0.items().iter().all(|x| x.should_be_packed()) {
            fmt.subregion()
                .current_column_as_indent()
                .enter(|fmt| self.format_packed_items(fmt))
        } else {
            self.0.format(fmt)
        }
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ListLike<T, D = CommaSymbol> {
    open: OpenSquareSymbol,
    items: MaybePackedItems<T, D>,
    close: CloseSquareSymbol,
}

impl<T: Format, D: Format + TokenStr> Format for ListLike<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .trailing_columns(1) // "]"
            .enter(|fmt| self.items.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct TupleLike<T> {
    open: OpenBraceSymbol,
    items: MaybePackedItems<T>,
    close: CloseBraceSymbol,
}

impl<T: Format> Format for TupleLike<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .trailing_columns(1) // "}"
            .enter(|fmt| self.items.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses<T>(NonEmptyItems<T, SemicolonSymbol>);

impl<T: Format> Format for Clauses<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            for (item, delimiter) in self.0.items().iter().zip(self.0.delimiters().iter()) {
                fmt.subregion()
                    .reset_trailing_columns(1) // ";"
                    .enter(|fmt| item.format(fmt))?;
                delimiter.format(fmt)?;
                fmt.write_newline()?;
            }
            self.0.items().last().expect("unreachable").format(fmt)?;
            Ok(())
        })
    }
}

// TODO: delete
#[derive(Debug, Clone, Span, Parse)]
pub struct UnaryOpLike<O, T> {
    op: O,
    item: T,
}

impl<O: Format + TokenStr, T: Format> Format for UnaryOpLike<O, T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let op = self.op.token_str();
        if op.starts_with(fmt.last_char()) {
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

// TODO: s/../BinaryOpProperty?/
pub trait IndentOffset {
    // TODO: support no-newline
    fn indent_offset(&self) -> usize;

    // needs_space
}

// TODO: rename
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Indent<T, const N: usize>(T);

impl<T, const N: usize> IndentOffset for Indent<T, N> {
    fn indent_offset(&self) -> usize {
        N
    }
}

impl<T: TokenStr, const N: usize> TokenStr for Indent<T, N> {
    fn token_str(&self) -> &str {
        self.0.token_str()
    }
}

// TODO: rename
#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpLike<L, O, R> {
    pub left: L,
    pub op: O,
    pub right: R,
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

impl<L: Format, O: Format + IndentOffset + TokenStr, R: Format> Format for BinaryOpLike<L, O, R> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            fmt.subregion()
                .reset_trailing_columns(self.op.token_str().len() + 1)
                .enter(|fmt| self.left.format(fmt))?;
            fmt.write_space()?;
            self.op.format(fmt)?;
            fmt.write_space()?;

            let indent_offset = self.op.indent_offset();
            if fmt.current_relative_column() <= indent_offset {
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
                fmt.subregion().indent_offset(indent_offset).enter(|fmt| {
                    fmt.write_newline()?;
                    self.right.format(fmt)
                })?;
            }
            Ok(())
        })
    }
}
