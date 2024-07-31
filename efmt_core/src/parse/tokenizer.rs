use erl_tokenize::{Position, PositionRange, Token};
use std::path::Path;

#[derive(Debug)]
pub struct Tokenizer {
    shebang_end_position: Option<Position>,
    inner: erl_tokenize::Tokenizer<String>,
}

impl Tokenizer {
    pub fn new(text: String) -> Self {
        let mut inner = erl_tokenize::Tokenizer::new(text);
        let shebang_end_position = if inner.text().starts_with("#!") && inner.text().contains('\n')
        {
            let mut line_end_position = Position::new();
            while inner.consume_char() != Some('\n') {
                line_end_position = inner.next_position();
            }
            inner.set_position(line_end_position.clone());
            Some(line_end_position)
        } else {
            None
        };
        Tokenizer {
            shebang_end_position,
            inner,
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
    type Item = erl_tokenize::Result<TokenOrShebang>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(position) = self.shebang_end_position.take() {
            return Some(Ok(TokenOrShebang::Shebang(position)));
        }
        self.inner
            .next()
            .map(|result| result.map(TokenOrShebang::Token))
    }
}

#[derive(Debug, Clone)]
pub enum TokenOrShebang {
    Token(Token),
    Shebang(Position),
}

impl PositionRange for TokenOrShebang {
    fn start_position(&self) -> Position {
        match self {
            Self::Token(token) => token.start_position(),
            Self::Shebang(_) => Position::new(),
        }
    }

    fn end_position(&self) -> Position {
        match self {
            Self::Token(token) => token.end_position(),
            Self::Shebang(position) => position.clone(),
        }
    }
}
