pub use efmt_derive::Span;

pub trait Span {
    fn start_position(&self) -> Position;
    fn end_position(&self) -> Position;

    fn is_empty(&self) -> bool {
        self.len() == 0
    }

    // TODO: delete (cannot work well with macros)
    fn len(&self) -> usize {
        if self.start_position() <= self.end_position() {
            self.end_position().offset() - self.start_position().offset()
        } else {
            // This branch is for `crate::items::generics::Maybe<T>` that
            // can have a smaller end position than the start position
            // if `T` is missing and there are whitespaces between previous and next tokens.
            0
        }
    }
}

impl<T: Span + ?Sized> Span for &T {
    fn start_position(&self) -> Position {
        (**self).start_position()
    }

    fn end_position(&self) -> Position {
        (**self).end_position()
    }
}

impl<T: Span> Span for Box<T> {
    fn start_position(&self) -> Position {
        (**self).start_position()
    }

    fn end_position(&self) -> Position {
        (**self).end_position()
    }
}

impl Span for Position {
    fn start_position(&self) -> Position {
        *self
    }

    fn end_position(&self) -> Position {
        *self
    }
}

impl Span for std::ops::Range<Position> {
    fn start_position(&self) -> Position {
        self.start
    }

    fn end_position(&self) -> Position {
        self.end
    }
}

impl<A: Span, B: Span> Span for (A, B) {
    fn start_position(&self) -> Position {
        self.0.start_position()
    }

    fn end_position(&self) -> Position {
        self.1.end_position()
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, serde::Serialize, serde::Deserialize,
)]
pub struct Position {
    offset: usize,
    line: usize,
    column: usize,
}

impl Position {
    pub const fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    pub const fn offset(self) -> usize {
        self.offset
    }

    pub const fn line(self) -> usize {
        self.line
    }

    pub const fn column(self) -> usize {
        self.column
    }
}

impl From<erl_tokenize::Position> for Position {
    fn from(x: erl_tokenize::Position) -> Self {
        Self::new(x.offset(), x.line(), x.column())
    }
}
