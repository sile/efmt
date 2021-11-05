pub use efmt_derive::Span;

pub trait Span {
    fn start_position(&self) -> Position;
    fn end_position(&self) -> Position;

    fn is_empty(&self) -> bool {
        // TODO: Add a note comment about the cases where the end position would be smaller than the start one.
        self.end_position() <= self.start_position()
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

impl<A: Span, B: Span> Span for (A, B) {
    fn start_position(&self) -> Position {
        self.0.start_position()
    }

    fn end_position(&self) -> Position {
        self.1.end_position()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
