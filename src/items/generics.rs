// s/generics/components/
use crate::format::{self, Format, Formatter};
use crate::format2::{Format2, Formatter2, Indent, Newline, NewlineIf};
use crate::items::keywords::WhenKeyword;
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CloseSquareSymbol, CommaSymbol, DoubleLeftAngleSymbol,
    DoubleRightAngleSymbol, OpenBraceSymbol, OpenParenSymbol, OpenSquareSymbol, RightArrowSymbol,
    SemicolonSymbol,
};
use crate::items::tokens::WhitespaceToken;
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

impl<T: Format2> Format2 for Maybe<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        if let Some(x) = self.get() {
            x.format2(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
            .trailing_columns(1) // ")"
            .enter(|fmt| self.item.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

impl<T: Format2> Format2 for Parenthesized<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        self.open.format2(fmt);
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.item.format2(fmt);
        });
        self.close.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct Params<T>(Parenthesized<Items<T>>);

impl<T> Params<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct Args<T>(Parenthesized<Items<T>>);

impl<T> Args<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct CommaDelimiter(CommaSymbol);

impl Format2 for CommaDelimiter {
    fn format2(&self, fmt: &mut Formatter2) {
        self.0.format2(fmt);
        fmt.add_space();
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D = CommaDelimiter> {
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

impl<T: Format, D: Format> NonEmptyItems<T, D> {
    fn format_items(&self, fmt: &mut Formatter, multi_line: bool) -> format::Result<()> {
        for (item, delimiter) in self.items.iter().zip(self.delimiters.iter()) {
            let trailing_columns = fmt
                .item_formatted_text(delimiter)
                .map_or(1, |s| s.trim_end().len());
            fmt.subregion()
                .reset_trailing_columns(trailing_columns)
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

    pub fn format_multiline(&self, fmt: &mut Formatter) -> format::Result<()> {
        fmt.subregion()
            .current_column_as_indent()
            .enter(|fmt| self.format_items(fmt, true))
    }
}

impl<T: Format, D: Format> Format for NonEmptyItems<T, D> {
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

impl<T: Format2, D: Format2> Format2 for NonEmptyItems<T, D> {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            let item = self.items().first().expect("unreachable");
            fmt.subregion(Indent::Inherit, Newline::Never, |fmt| item.format2(fmt));
            for (item, delimiter) in self.items.iter().skip(1).zip(self.delimiters.iter()) {
                delimiter.format2(fmt);
                fmt.subregion(
                    Indent::Inherit,
                    Newline::If(NewlineIf {
                        too_long: true,
                        multi_line_parent: true,
                        ..Default::default()
                    }),
                    |fmt| item.format2(fmt),
                );
            }
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct Items<T, D = CommaDelimiter>(Maybe<NonEmptyItems<T, D>>);

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
pub struct MaybePackedItems<T, D = CommaDelimiter>(Items<T, D>);

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

impl<T: Format, D: Format> Format for MaybePackedItems<T, D> {
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

impl<T: Format2, D: Format2> MaybePackedItems<T, D> {
    fn packed_format2(&self, fmt: &mut Formatter2) {
        todo!()
    }
}

impl<T: Format2, D: Format2> Format2 for MaybePackedItems<T, D> {
    fn format2(&self, fmt: &mut Formatter2) {
        if self.0.items().is_empty() {
        } else if self.0.items().iter().all(|x| !x.has_whitespace()) {
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                self.packed_format2(fmt)
            });
        } else {
            self.0.format2(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct ListLike<T, D = CommaDelimiter> {
    open: OpenSquareSymbol,
    items: MaybePackedItems<T, D>,
    close: CloseSquareSymbol,
}

impl<T: Format, D: Format> Format for ListLike<T, D> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .trailing_columns(1) // "]"
            .enter(|fmt| self.items.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
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

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct BitstringLike<T> {
    open: DoubleLeftAngleSymbol,
    items: MaybePackedItems<T>,
    close: DoubleRightAngleSymbol,
}

impl<T: Format> Format for BitstringLike<T> {
    fn format(&self, fmt: &mut Formatter) -> format::Result<()> {
        self.open.format(fmt)?;
        fmt.subregion()
            .trailing_columns(2) // ">>"
            .enter(|fmt| self.items.format(fmt))?;
        self.close.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Clauses<T>(NonEmptyItems<T, SemicolonSymbol>);

impl<T> Clauses<T> {
    pub fn items(&self) -> &[T] {
        self.0.items()
    }
}

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

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct UnaryOpLike<O, T> {
    op: O,
    item: T,
}

impl<O: Format, T: Format> Format for UnaryOpLike<O, T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if fmt
            .item_formatted_text(&self.op)
            .map_or(false, |s| s.starts_with(fmt.last_char()))
        {
            fmt.write_space()?;
        }
        self.op.format(fmt)?;
        if fmt.last_char().is_alphabetic() {
            fmt.write_space()?;
        }

        self.item.format(fmt)?;
        Ok(())
    }

    fn should_be_packed(&self) -> bool {
        self.item.should_be_packed()
    }
}

pub trait BinaryOpStyle {
    fn indent_offset(&self) -> usize;

    fn allow_newline(&self) -> bool;

    fn should_pack(&self) -> bool;

    fn needs_left_space(&self) -> bool {
        true
    }

    fn needs_right_space(&self) -> bool {
        true
    }
}

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

impl<L: Format, O: Format + BinaryOpStyle, R: Format> Format for BinaryOpLike<L, O, R> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let trailing_columns = fmt
            .item_formatted_text(&self.op)
            .map_or(1, |s| s.trim_end().len())
            + 1;

        fmt.subregion()
            .reset_trailing_columns(trailing_columns)
            .enter(|fmt| self.left.format(fmt))?;
        fmt.write_space()?;
        self.op.format(fmt)?;
        fmt.write_space()?;

        let indent_offset = self.op.indent_offset();
        if !self.op.allow_newline() || fmt.current_relative_column() <= indent_offset {
            // Inserting a newline is forbiddend or cannot shorten the line length.
            return self.right.format(fmt);
        }

        let mut options = fmt
            .subregion()
            .forbid_too_long_line()
            .check_trailing_columns(true);
        if !self.op.should_pack() {
            options = options.forbid_multi_line();
        }
        if options.enter(|fmt| self.right.format(fmt)).is_err() {
            fmt.subregion().indent_offset(indent_offset).enter(|fmt| {
                fmt.write_newline()?;
                self.right.format(fmt)
            })?;
        }
        Ok(())
    }
}

impl<L: Format2, O: Format2 + BinaryOpStyle, R: Format2> Format2 for BinaryOpLike<L, O, R> {
    fn format2(&self, fmt: &mut Formatter2) {
        self.left.format2(fmt);

        if self.op.needs_left_space() {
            fmt.add_space();
        }
        self.op.format2(fmt);
        if self.op.needs_right_space() {
            fmt.add_space();
        }

        if !self.op.allow_newline() {
            self.right.format2(fmt);
            return;
        }

        let newline_cond = NewlineIf {
            too_long: true,
            multi_line: !self.op.should_pack(),
            ..Default::default()
        };
        fmt.subregion(
            Indent::Offset(self.op.indent_offset()),
            Newline::If(newline_cond),
            |fmt| self.right.format2(fmt),
        );
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct WithArrow<T> {
    item: T,
    arrow: RightArrowSymbol,
}

impl<T: Format> Format for WithArrow<T> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion()
            .reset_trailing_columns(3) // " ->"
            .enter(|fmt| {
                self.item.format(fmt)?;
                fmt.write_space()?;
                self.arrow.format(fmt)?;
                fmt.write_space()?;
                Ok(())
            })
        // TODO: .enter_with_trailer(|fmt| {...}, |fmt| {})
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct WithGuard<T, U, D = Either<CommaSymbol, SemicolonSymbol>> {
    item: T,
    guard: Maybe<Guard<U, D>>,
}

impl<T: Format, U: Format, D: Format> Format for WithGuard<T, U, D> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if let Some(guard) = self.guard.get() {
            fmt.subregion()
                .clear_trailing_columns(true)
                .enter(|fmt| self.item.format(fmt))?;
            fmt.write_space()?;
            guard.format(fmt)?;
        } else {
            self.item.format(fmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Guard<T, D> {
    when: WhenKeyword,
    conditions: NonEmptyItems<T, D>,
}

impl<T: Format, D: Format> Format for Guard<T, D> {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        let format = |fmt: &mut format::Formatter| {
            self.when.format(fmt)?;
            fmt.write_space()?;
            self.conditions.format(fmt)?;
            Ok(())
        };

        if fmt.current_relative_column() <= 2 {
            format(fmt)?;
        } else if fmt
            .subregion()
            .forbid_too_long_line()
            .forbid_multi_line()
            .check_trailing_columns(true)
            .enter(format)
            .is_err()
        {
            fmt.write_newline()?;
            fmt.subregion().indent_offset(2).enter(format)?;
        }
        Ok(())
    }
}
