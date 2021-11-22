// s/generics/components/
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

impl Format2 for Null {
    fn format2(&self, _: &mut Formatter2) {}
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

impl<T: Format2> Format2 for Maybe<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        if let Some(x) = self.get() {
            x.format2(fmt);
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
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

impl<T: Format2> Format2 for Parenthesized<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        self.open.format2(fmt);
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.item.format2(fmt);
        });
        self.close.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct Params<T>(Parenthesized<Items<T>>);

impl<T> Params<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct Args<T>(Parenthesized<Items<T>>);

impl<T> Args<T> {
    pub fn get(&self) -> &[T] {
        self.0.get().items()
    }
}

#[derive(Debug, Clone, Span, Parse)]
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

    // TODO
    // pub fn map<F, U>(self, f: F) -> NonEmptyItems<U, D>
    // where
    //     F: Fn(T) -> U,
    // {
    //     NonEmptyItems {
    //         items: self.items.into_iter().map(f).collect(),
    //         delimiters: self.delimiters,
    //     }
    // }
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

impl<T: Format2, D: Format2> NonEmptyItems<T, D> {
    pub fn format2_multi_line(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            let item = self.items().first().expect("unreachable");
            fmt.subregion(Indent::Inherit, Newline::Never, |fmt| item.format2(fmt));
            for (item, delimiter) in self.items.iter().skip(1).zip(self.delimiters.iter()) {
                delimiter.format2(fmt);
                fmt.subregion(Indent::Inherit, Newline::Always, |fmt| item.format2(fmt));
            }
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
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

impl<T: Format2, D: Format2> MaybePackedItems<T, D> {
    fn packed_format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            let item = self.0.items().first().expect("unreachable");
            fmt.subregion(Indent::Inherit, Newline::Never, |fmt| item.format2(fmt));
            for (item, delimiter) in self
                .0
                .items()
                .iter()
                .skip(1)
                .zip(self.0.delimiters().iter())
            {
                delimiter.format2(fmt);
                fmt.subregion(
                    Indent::Inherit,
                    Newline::If(NewlineIf {
                        too_long: true,
                        ..Default::default()
                    }),
                    |fmt| item.format2(fmt),
                );
            }
        });
    }
}

impl<T: Format2, D: Format2> Format2 for MaybePackedItems<T, D> {
    fn format2(&self, fmt: &mut Formatter2) {
        if self.0.items().is_empty() {
        } else if self
            .0
            .items()
            .iter()
            .all(|x| !fmt.item_to_text(x).contains(&[' ', '\n'][..]))
        {
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

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct TupleLike<T> {
    open: OpenBraceSymbol,
    items: MaybePackedItems<T>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct BitstringLike<T> {
    open: DoubleLeftAngleSymbol,
    items: MaybePackedItems<T>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct Clauses<T>(NonEmptyItems<T, SemicolonDelimiter>);

impl<T> Clauses<T> {
    pub fn items(&self) -> &[T] {
        self.0.items()
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct SemicolonDelimiter(SemicolonSymbol);

impl Format2 for SemicolonDelimiter {
    fn format2(&self, fmt: &mut Formatter2) {
        self.0.format2(fmt);
        fmt.add_newline();
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct UnaryOpLike<O, T> {
    op: O,
    item: T,
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

impl<T: Format2> Format2 for WithArrow<T> {
    fn format2(&self, fmt: &mut Formatter2) {
        self.item.format2(fmt);
        fmt.add_space();
        self.arrow.format2(fmt);
        fmt.add_space();
    }
}

#[derive(Debug, Clone, Span, Parse, Format2)]
pub struct WithGuard<T, U, D = Either<CommaSymbol, SemicolonSymbol>> {
    item: T,
    guard: Maybe<Guard<U, D>>,
}

#[derive(Debug, Clone, Span, Parse)]
struct Guard<T, D> {
    when: WhenKeyword,
    conditions: NonEmptyItems<T, D>,
}

impl<T: Format2, D: Format2> Format2 for Guard<T, D> {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.subregion(
            Indent::Offset(2),
            Newline::If(NewlineIf {
                too_long: true,
                multi_line: true,
                ..Default::default()
            }),
            |fmt| {
                fmt.add_space();
                self.when.format2(fmt);
                fmt.add_space();
                self.conditions.format2(fmt);
            },
        );
    }
}
