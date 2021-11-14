use crate::format::{Error, Result};
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};

#[derive(Debug, Clone)]
pub struct RegionConfig {
    pub indent: usize,
    pub max_columns: usize,
    pub allow_multi_line: bool,
    pub allow_too_long_line: bool,
}

impl RegionConfig {
    pub fn new(max_columns: usize) -> Self {
        Self {
            indent: 0,
            max_columns,
            allow_multi_line: true,
            allow_too_long_line: true,
        }
    }
}

#[derive(Debug)]
struct RegionState {
    next_position: Position,
    current_column: usize,
    formatted_text: String,
}

#[derive(Debug)]
pub struct RegionWriter {
    config: RegionConfig,
    state: RegionState,
    parent: Option<Box<Self>>,
}

impl RegionWriter {
    pub fn new(max_columns: usize) -> Self {
        Self {
            config: RegionConfig::new(max_columns),
            state: RegionState {
                next_position: Position::new(0, 0, 0),
                current_column: 0,
                formatted_text: String::new(),
            },
            parent: None,
        }
    }

    pub fn start_subregion(&mut self, mut config: RegionConfig) {
        if !self.config.allow_multi_line {
            config.allow_multi_line = false;
        }
        if !self.config.allow_too_long_line {
            config.allow_too_long_line = false;
        }

        let state = RegionState {
            next_position: self.state.next_position,
            current_column: self.state.current_column,
            formatted_text: String::new(),
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

    pub fn next_position(&self) -> Position {
        self.state.next_position
    }

    pub fn current_column(&self) -> usize {
        self.state.current_column
    }

    pub fn formatted_text(&self) -> &str {
        &self.state.formatted_text
    }

    pub fn write_blank(&mut self) -> Result<()> {
        if !matches!(self.last_char(), ' ' | '\n') {
            self.write(" ")?;
        }
        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<()> {
        if self.last_char() != '\n' {
            if self.last_char() == ' ' {
                self.pop_last_char();
            }
            self.write("\n")?;
        }
        Ok(())
    }

    pub fn write_item(&mut self, text: &str, item: &impl Span) -> Result<()> {
        // If macros have appeared in the text,
        // an item's start and end positions could be smaller than `next_position`.
        let start = std::cmp::max(item.start_position(), self.state.next_position);
        let end = std::cmp::max(item.end_position(), start);
        // TODO
        // if start == end {
        //     return Ok(());
        // }
        let text = &text[start.offset()..end.offset()];

        // TODO: move to format.rs
        if self.state.next_position.line() + 1 < item.start_position().line() {
            self.write("\n")?;
        }

        self.write(text)?;
        self.state.next_position = end;
        Ok(())
    }

    pub fn write_comment(&mut self, text: &str, comment: &CommentToken) -> Result<()> {
        assert!(!comment.is_empty());

        match comment.kind() {
            CommentKind::Post => {
                assert_eq!(self.last_char(), '\n');

                // TODO: move to format.rs
                if self.state.next_position.line() + 1 < comment.start_position().line() {
                    self.write("\n")?;
                }

                for _ in 0..self.config.indent {
                    self.state.formatted_text.push(' ');
                }
            }
            CommentKind::Trailing => {
                if self.last_char() == '\n' {
                    self.pop_last_char();
                }
                if self.last_char() != ' ' {
                    self.state.formatted_text.push(' ');
                }
                self.state.formatted_text.push(' ');
            }
        }

        let text = &text[comment.start_position().offset()..comment.end_position().offset()];
        self.state.formatted_text.push_str(text);
        self.state.next_position = comment.end_position();
        self.write_newline()?;

        Ok(())
    }

    fn write(&mut self, s: &str) -> Result<()> {
        for c in s.chars() {
            if c == '\n' {
                if !self.config.allow_multi_line {
                    return Err(Error::Multiline);
                }
                self.state.current_column = 0;
            } else if c != ' ' {
                if self.state.current_column >= self.config.max_columns {
                    if !self.config.allow_too_long_line {
                        return Err(Error::MaxColumnsExceeded);
                    }
                }

                if self.state.current_column < self.config.indent {
                    for _ in self.state.current_column..self.config.indent {
                        self.state.formatted_text.push(' ');
                    }
                    self.state.current_column = self.config.indent;
                }
            }

            self.state.formatted_text.push(c);
            if c != '\n' {
                self.state.current_column += 1;
            }
        }
        Ok(())
    }

    pub fn last_char(&self) -> char {
        if let Some(c) = self.state.formatted_text.chars().last() {
            c
        } else if let Some(parent) = &self.parent {
            parent.last_char()
        } else {
            '\n'
        }
    }

    fn pop_last_char(&mut self) {
        if self.state.formatted_text.pop().is_none() {
            self.parent.as_mut().map(|x| x.pop_last_char());
        }
    }
}
