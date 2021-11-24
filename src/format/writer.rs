use crate::span::{Position, Span};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    LineTooLong,

    #[error("unexpected multi-line")]
    MultiLine,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Clone)]
pub struct RegionConfig {
    pub indent: usize,
    pub max_columns: Option<usize>,
    pub allow_multi_line: bool,
}

impl RegionConfig {
    pub fn new() -> Self {
        Self {
            indent: 0,
            max_columns: None,
            allow_multi_line: true,
        }
    }
}

#[derive(Debug)]
struct RegionState {
    next_position: Position,
    current_column: usize,
    formatted_text: String,
    popped_parent_chars: usize,
}

#[derive(Debug)]
pub struct RegionWriter {
    config: RegionConfig,
    state: RegionState,
    parent: Option<Box<Self>>,
}

impl RegionWriter {
    pub fn new() -> Self {
        Self {
            config: RegionConfig::new(),
            state: RegionState {
                next_position: Position::new(0, 0, 0),
                current_column: 0,
                formatted_text: String::new(),
                popped_parent_chars: 0,
            },
            parent: None,
        }
    }

    pub fn start_subregion(&mut self, mut config: RegionConfig) {
        if !self.config.allow_multi_line {
            config.allow_multi_line = false;
        }
        if self.config.max_columns.is_some() {
            if config.max_columns.is_some() {
                assert_eq!(config.max_columns, self.config.max_columns);
            }
            config.max_columns = self.config.max_columns;
        }

        let state = RegionState {
            next_position: self.state.next_position,
            current_column: self.state.current_column,
            formatted_text: String::new(),
            popped_parent_chars: 0,
        };
        let parent = std::mem::replace(
            self,
            Self {
                config,
                state,
                parent: None,
            },
        );
        self.parent = Some(Box::new(parent));
    }

    pub fn commit_subregion(&mut self) {
        let parent = *self.parent.take().expect("bug");
        let commited = std::mem::replace(self, parent);
        self.state.next_position = commited.state.next_position;
        self.state.current_column = commited.state.current_column;
        for _ in 0..commited.state.popped_parent_chars {
            self.pop_last_char();
        }
        self.state
            .formatted_text
            .push_str(&commited.state.formatted_text);
    }

    pub fn abort_subregion(&mut self) {
        let parent = *self.parent.take().expect("bug");
        let _ = std::mem::replace(self, parent);
    }

    pub fn config(&self) -> &RegionConfig {
        &self.config
    }

    pub fn parent_indent(&self) -> usize {
        let current = self.config.indent;
        if let Some(parent) = &self.parent {
            if parent.config().indent != current {
                parent.config().indent
            } else {
                parent.parent_indent()
            }
        } else {
            current
        }
    }

    pub fn current_column(&self) -> usize {
        self.state.current_column
    }

    pub fn formatted_text(&self) -> &str {
        &self.state.formatted_text
    }

    pub fn write_space(&mut self, mut n: usize) -> Result<()> {
        if !matches!(self.last_char(), ' ' | '\n') {
            self.write(" ")?;
        }
        n -= 1;

        for _ in 0..n {
            self.write(" ")?;
        }
        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<()> {
        if self.last_char() != '\n' {
            if self.last_char() == ' ' && self.next_last_char() != '$' {
                self.pop_last_char();
            }
            self.write("\n")?;
        }
        Ok(())
    }

    pub fn write_item(&mut self, text: &str, item: &impl Span, is_comment: bool) -> Result<()> {
        let start = item.start_position();
        let end = item.end_position();
        let text = &text[start.offset()..end.offset()];

        // TODO: move to `formatter.rs`
        if self.state.next_position.line() + 1 < item.start_position().line() {
            self.write("\n")?;
        }

        if !text.is_empty() && self.last_char() == '\n' {
            for _ in 0..self.config.indent {
                self.state.formatted_text.push(' ');
            }
            self.state.current_column = self.config.indent;
        }

        if is_comment {
            let old = self.config.max_columns.take();
            let result = self.write(text);
            self.config.max_columns = old;
            result?;
        } else {
            self.write(text)?;
        }
        self.state.next_position = end;
        Ok(())
    }

    fn write(&mut self, s: &str) -> Result<()> {
        for c in s.chars() {
            if c == '\n' {
                if !self.config.allow_multi_line {
                    return Err(Error::MultiLine);
                }
                self.state.current_column = 0;
            } else if (self.last_char() == '$' || c != ' ')
                && self
                    .config
                    .max_columns
                    .map_or(false, |n| self.state.current_column >= n)
            {
                return Err(Error::LineTooLong);
            }

            self.state.formatted_text.push(c);
            if c != '\n' {
                self.state.current_column += 1;
            }
        }
        Ok(())
    }

    fn last_char(&self) -> char {
        self.n_last_char(0)
    }

    fn next_last_char(&self) -> char {
        self.n_last_char(1)
    }

    fn n_last_char(&self, n: usize) -> char {
        if let Some(c) = self.state.formatted_text.chars().rev().nth(n) {
            c
        } else if let Some(parent) = &self.parent {
            let m = self.state.formatted_text.chars().count();
            parent.n_last_char(self.state.popped_parent_chars + (n - m))
        } else {
            '\n'
        }
    }

    fn pop_last_char(&mut self) {
        if self.state.formatted_text.pop().is_none() {
            self.state.popped_parent_chars += 1;
        }
    }
}
