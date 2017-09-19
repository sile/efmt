use std::str::Chars;

macro_rules! fmt {
    ($writer:expr, $($arg:tt)+) => {
        track!(write!($writer, $($arg)+).map_err(::Error::from))
    }
}
// macro_rules! fmtln {
//     ($writer:expr, $($arg:tt)+) => {
//         track!(writeln!($writer, $($arg)+).map_err(::Error::from))
//     }
// }

pub use self::module::ModuleFormatter;

mod module;

// TODO: move
#[derive(Debug, Clone)]
pub struct Input<'a> {
    code: Chars<'a>,
    position: usize,
}
impl<'a> Input<'a> {
    pub fn new(code: &'a str) -> Self {
        Input {
            code: code.chars(),
            position: 0,
        }
    }
    pub fn position(&self) -> usize {
        self.position
    }
    pub fn consume_until(&mut self, position: usize) {
        if self.position < position {
            let count = position - self.position;
            for _ in self.by_ref().take(count) {}
        }
    }
}
impl<'a> Iterator for Input<'a> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.code.next() {
            self.position += 1;
            Some(c)
        } else {
            None
        }
    }
}
