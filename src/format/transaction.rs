use crate::format::{Error, MultilineMode, Result};
use crate::items::tokens::{CommentKind, CommentToken};
use crate::span::{Position, Span};

#[derive(Debug, Clone)]
pub struct TransactionConfig {
    pub indent: usize,
    pub max_columns: usize,
    pub multiline_mode: MultilineMode,
}

impl TransactionConfig {
    fn root(max_columns: usize) -> Self {
        Self {
            indent: 0,
            max_columns,
            multiline_mode: MultilineMode::Allow,
        }
    }
}

#[derive(Debug, Clone)]
pub struct TransactionState {
    next_position: Position,
    current_column: usize,
    formatted_text: String,
}

impl TransactionState {
    fn clone_for_new_transaction(&self) -> Self {
        Self {
            next_position: self.next_position,
            current_column: self.current_column,
            formatted_text: String::new(),
        }
    }

    fn copy_from_committed_transaction(&mut self, commited: Self) {
        self.next_position = commited.next_position;
        self.current_column = commited.current_column;
        self.formatted_text.push_str(&commited.formatted_text);
    }
}

#[derive(Debug, Clone)]
pub struct Transaction {
    config: TransactionConfig,
    state: TransactionState,
    parent: Option<Box<Self>>,
}

impl Transaction {
    pub fn root(max_columns: usize) -> Self {
        Self {
            config: TransactionConfig::root(max_columns),
            state: TransactionState {
                next_position: Position::new(0, 0, 0),
                current_column: 0,
                formatted_text: String::new(),
            },
            parent: None,
        }
    }

    pub fn formatted_text(&self) -> &str {
        &self.state.formatted_text
    }

    pub fn start_new_transaction(&mut self, config: TransactionConfig) {
        let state = self.state.clone_for_new_transaction();
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

    pub fn commit(&mut self) {
        let parent = *self.parent.take().expect("bug");
        let commited = std::mem::replace(self, parent);
        self.state.copy_from_committed_transaction(commited.state);
    }

    pub fn abort(&mut self) {
        let parent = *self.parent.take().expect("bug");
        let _ = std::mem::replace(self, parent);
    }

    pub fn config(&self) -> &TransactionConfig {
        &self.config
    }

    pub fn parent(&self) -> Option<&Self> {
        self.parent.as_ref().map(|x| x.as_ref())
    }

    pub fn pop_last_char(&mut self) {
        if self.state.formatted_text.pop().is_none() {
            self.parent.as_mut().map(|x| x.pop_last_char());
        }
    }

    pub fn next_position(&self) -> Position {
        self.state.next_position
    }

    pub fn current_column(&self) -> usize {
        self.state.current_column
    }

    pub fn write_blank(&mut self) -> Result<()> {
        if self.last_char().map_or(false, |c| !matches!(c, ' ' | '\n')) {
            self.write(" ")?;
        }
        Ok(())
    }

    pub fn write_newline(&mut self) -> Result<()> {
        if self.last_char().map_or(false, |c| c != '\n') {
            if self.last_char() == Some(' ') {
                self.pop_last_char();
            }
            self.write("\n")?;
        }
        Ok(())
    }

    pub fn write_item(&mut self, text: &str, item: &impl Span) -> Result<()> {
        if item.is_empty() {
            return Ok(());
        }

        let start = std::cmp::max(
            item.start_position().offset(),
            self.state.next_position.offset(), // Maybe macros were already written here
        );
        let end = std::cmp::max(item.end_position().offset(), start);
        if start == end {
            // A macro call
            return Ok(());
        }
        let text = &text[start..end];

        if self.state.next_position.line() + 1 < item.start_position().line() {
            self.write("\n")?;
        }
        self.write(text)?;
        self.state.next_position = item.end_position();
        Ok(())
    }

    pub fn write_comment(&mut self, text: &str, comment: &CommentToken) -> Result<()> {
        assert!(!comment.is_empty());

        if comment.kind() == CommentKind::Post {
            if self.state.next_position.line() + 1 < comment.start_position().line() {
                self.write("\n")?;
            }
            for _ in 0..self.config.indent {
                self.state.formatted_text.push(' ');
            }
            self.state.current_column += self.config.indent;
        } else {
            if self.last_char() == Some('\n') {
                self.pop_last_char();
            }
            if self.last_char() == Some(' ') {
                self.write(" ")?;
            } else {
                self.write("  ")?;
            }
            self.state.current_column += 2;
        }

        let text = &text[comment.start_position().offset()..comment.end_position().offset()];
        self.state.formatted_text.push_str(text);
        self.state.current_column += text.len();
        self.state.next_position = comment.end_position();
        self.write_newline()?;

        Ok(())
    }

    pub fn finish(self) -> Result<String> {
        assert!(self.parent.is_none());
        Ok(self.state.formatted_text)
    }

    pub fn last_char(&self) -> Option<char> {
        self.state
            .formatted_text
            .chars()
            .last()
            .or_else(|| self.parent.as_ref().and_then(|x| x.last_char()))
    }

    fn is_multiline_forbiden(&self) -> bool {
        if self.config.multiline_mode == MultilineMode::Forbid {
            true
        } else if let Some(parent) = &self.parent {
            parent.is_multiline_forbiden()
        } else {
            false
        }
    }

    fn write(&mut self, s: &str) -> Result<()> {
        for c in s.chars() {
            if c == '\n' {
                if self.is_multiline_forbiden() {
                    //self.config.multiline_mode == MultilineMode::Forbid {
                    let position = self.next_position();
                    return Err(Error::Multiline { position });
                }

                self.state.current_column = 0;
            } else {
                assert!(!c.is_control());

                if self.state.current_column >= self.config.max_columns {
                    if c != ' ' && self.config.multiline_mode != MultilineMode::Recommend {
                        // Should retry with setting `multiline_mode` to `MultilineMode::Recommend.
                        return Err(Error::MaxColumnsExceeded);
                    } else {
                        // TODO: Emit warning log
                    }
                }

                if c != ' ' {
                    if self.state.current_column < self.config.indent {
                        for _ in self.state.current_column..self.config.indent {
                            self.state.formatted_text.push(' ');
                        }
                        self.state.current_column = self.config.indent;
                    }
                }
                self.state.current_column += 1;
            }
            self.state.formatted_text.push(c);
        }
        Ok(())
    }
}
