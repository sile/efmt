use crate::span::{Position, Span};
use std::num::NonZeroUsize;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("max columns exceeded")]
    LineTooLong,

    #[error("unexpected multi-line")]
    MultiLine,
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Writer {
    max_columns: usize,
    buf: String,
    region: RegionState,
}

impl Writer {
    pub fn new(max_columns: usize) -> Self {
        Self {
            max_columns,
            buf: String::new(),
            region: RegionState::new(),
        }
    }

    pub fn finish(self) -> String {
        self.buf
    }

    fn last_whitespace_char(&self) -> Option<char> {
        let mut chars = self.buf.chars().rev();
        match (chars.next(), chars.next()) {
            (Some(_), Some('$')) => None,
            (Some(c @ (' ' | '\n')), _) => Some(c),
            (None, None) => Some('\n'), // sentinel value
            _ => None,
        }
    }

    fn is_last_triple_newlines(&self) -> bool {
        let mut chars = self.buf.chars().rev();
        matches!(
            (chars.next(), chars.next(), chars.next()),
            (Some('\n'), Some('\n'), Some('\n'))
        )
    }

    pub fn write_space(&mut self) -> Result<()> {
        if self.last_whitespace_char().is_none() {
            self.write(" ", true)?;
        }
        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<()> {
        self.write_newlines(NonZeroUsize::new(1).expect("unreachable"))
    }

    pub fn write_newlines(&mut self, count: NonZeroUsize) -> Result<()> {
        if self.buf.is_empty() {
            return Ok(());
        }

        let mut n = count.get();
        loop {
            match self.last_whitespace_char() {
                Some('\n') => {
                    self.pop_last_char();
                    n -= 1;
                    if n == 0 {
                        break;
                    }
                }
                Some(' ') => {
                    self.pop_last_char();
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        for _ in 0..count.get() {
            self.write("\n", false)?;
        }
        Ok(())
    }

    fn pop_last_char(&mut self) {
        let c = self.buf.pop();
        if self.buf.len() < self.region.buf_start {
            self.region.buf_start = self.buf.len();
            self.region
                .popped_parent_chars
                .push(c.expect("unreachable"));
        }
    }

    pub fn write_span(&mut self, text: &str, span: &impl Span) -> Result<()> {
        let start = span.start_position();
        let end = span.end_position();
        let text = &text[start.offset()..end.offset()];
        if text.is_empty() {
            return Ok(());
        }

        if self.region.next_position.line() + 1 < span.start_position().line()
            && !self.is_last_triple_newlines()
        {
            self.write("\n", false)?;
        }

        if self.last_whitespace_char() == Some('\n') {
            for _ in 0..self.current_indent() {
                self.buf.push(' ');
            }
            self.region.current_column = self.current_indent();
        }

        let is_first_span = self.region.buf_start == self.buf.len();
        if is_first_span {
            for _ in self.region.current_column..self.current_indent() {
                self.buf.push(' ');
                self.region.current_column = self.current_indent();
            }
        }

        self.write(text, false)?;
        self.region.next_position = end;
        Ok(())
    }

    pub fn write_trailing_comment(&mut self, text: &str, span: &impl Span) -> Result<()> {
        let start = span.start_position();
        let end = span.end_position();
        let text = &text[start.offset()..end.offset()];

        while self.last_whitespace_char().is_some() {
            self.pop_last_char();
        }

        self.write("  ", true)?;
        self.write(text, true)?;
        self.region.next_position = end;
        Ok(())
    }

    fn write(&mut self, s: &str, skip_column_check: bool) -> Result<()> {
        for c in s.chars() {
            if c == '\n' {
                if !self.is_multi_line_allowed() {
                    return Err(Error::MultiLine);
                }
                self.region.current_column = 0;
                self.region.config.allow_too_long_line = true; // Only first line is relevant.
            } else if !skip_column_check
                && !self.region.config.allow_too_long_line
                && self.region.current_column >= self.max_columns
            {
                return Err(Error::LineTooLong);
            }

            self.buf.push(c);
            if c != '\n' {
                self.region.current_column += 1;
            }
        }
        Ok(())
    }

    pub fn start_subregion(&mut self, mut config: RegionConfig) {
        if !self.region.config.allow_multi_line {
            config.allow_multi_line = false;
        }
        if !self.region.config.allow_too_long_line {
            config.allow_too_long_line = false;
        }

        let new = RegionState {
            config,
            next_position: self.region.next_position,
            current_column: self.region.current_column,
            buf_start: self.buf.len(),
            popped_parent_chars: Vec::new(),
            parent: None,
        };
        let parent = std::mem::replace(&mut self.region, new);
        self.region.parent = Some(Box::new(parent));
    }

    pub fn commit_subregion(&mut self) {
        let parent = *self.region.parent.take().expect("bug");
        let commited = std::mem::replace(&mut self.region, parent);
        self.region.next_position = commited.next_position;
        self.region.current_column = commited.current_column;
    }

    pub fn abort_subregion(&mut self) {
        let parent = *self.region.parent.take().expect("bug");
        let aborted = std::mem::replace(&mut self.region, parent);

        self.buf.truncate(aborted.buf_start);
        for c in aborted.popped_parent_chars.into_iter().rev() {
            self.buf.push(c);
        }
    }

    pub fn current_column(&self) -> usize {
        self.region.current_column
    }

    pub fn current_indent(&self) -> usize {
        self.region.config.indent
    }

    pub fn parent_indent(&self) -> usize {
        self.region.parent_indent()
    }

    pub fn is_multi_line_allowed(&self) -> bool {
        self.region.config.allow_multi_line
    }
}

#[derive(Debug, Clone)]
pub struct RegionConfig {
    pub indent: usize,
    pub allow_too_long_line: bool,
    pub allow_multi_line: bool,
}

impl RegionConfig {
    fn new() -> Self {
        Self {
            indent: 0,
            allow_too_long_line: true,
            allow_multi_line: true,
        }
    }
}

#[derive(Debug)]
struct RegionState {
    config: RegionConfig,
    next_position: Position,
    current_column: usize,
    buf_start: usize,
    popped_parent_chars: Vec<char>,
    parent: Option<Box<Self>>,
}

impl RegionState {
    fn new() -> Self {
        Self {
            config: RegionConfig::new(),
            next_position: Position::new(0, 0, 0),
            current_column: 0,
            buf_start: 0,
            popped_parent_chars: Vec::new(),
            parent: None,
        }
    }

    fn parent_indent(&self) -> usize {
        let current = self.config.indent;
        if let Some(parent) = &self.parent {
            if parent.config.indent != current {
                parent.config.indent
            } else {
                parent.parent_indent()
            }
        } else {
            current
        }
    }
}
