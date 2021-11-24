use crate::span::Span;

mod formatter;
mod writer;

pub use self::formatter::{Formatter, Indent, Newline};

/// A procedural macro to derive [Format].
pub use efmt_derive::Format;

/// This trait allows formatting an item.
pub trait Format: Span {
    /// Formats this item.
    fn format(&self, fmt: &mut Formatter);
}

impl<T: Format> Format for Box<T> {
    fn format(&self, fmt: &mut Formatter) {
        (**self).format(fmt);
    }
}

impl<A: Format, B: Format> Format for (A, B) {
    fn format(&self, fmt: &mut Formatter) {
        self.0.format(fmt);
        self.1.format(fmt);
    }
}
