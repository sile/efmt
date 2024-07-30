use erl_tokenize::{Position, Token};
use std::path::Path;

#[derive(Debug)]
pub struct Tokenizer {
    inner: erl_tokenize::Tokenizer<String>,
}

impl Tokenizer {
    pub fn new(text: String) -> Self {
        Tokenizer {
            inner: erl_tokenize::Tokenizer::new(text),
        }
    }

    pub fn text(&self) -> &str {
        self.inner.text()
    }

    pub fn next_position(&self) -> Position {
        self.inner.next_position()
    }

    pub fn set_filepath<P: AsRef<Path>>(&mut self, path: P) {
        self.inner.set_filepath(path)
    }
}

impl Iterator for Tokenizer {
    type Item = erl_tokenize::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }
}
