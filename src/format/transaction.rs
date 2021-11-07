use crate::format::{Error, MultilineMode, Result, Whitespace};
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
    needs_whitespace: Option<Whitespace>,
    formatted_text: String,
}

impl TransactionState {
    fn clone_for_new_transaction(&self) -> Self {
        Self {
            next_position: self.next_position,
            current_column: self.current_column,
            needs_whitespace: self.needs_whitespace,
            formatted_text: String::new(),
        }
    }

    fn copy_from_committed_transaction(&mut self, commited: Self) {
        self.next_position = commited.next_position;
        self.current_column = commited.current_column;
        self.needs_whitespace = commited.needs_whitespace;
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
                needs_whitespace: None,
                formatted_text: String::new(),
            },
            parent: None,
        }
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

    pub fn next_position(&self) -> Position {
        self.state.next_position
    }

    pub fn current_column(&self) -> usize {
        self.state.current_column
    }

    pub fn needs_whitespace(&mut self, whitespace: Whitespace) {
        let c = self.last_char();
        if c == None {
            return;
        }
        if c == Some('\n') {
            return;
        }
        if c == Some(' ') && whitespace == Whitespace::Blank {
            return;
        }
        if self.state.needs_whitespace == Some(Whitespace::Newline)
            && whitespace == Whitespace::Blank
        {
            return;
        }
        self.state.needs_whitespace = Some(whitespace);
    }

    pub fn write_item(&mut self, text: &str, item: &impl Span) -> Result<()> {
        if item.is_empty() {
            return Ok(());
        }

        self.write_whitespace()?;
        if self.state.next_position.line() + 1 < item.start_position().line() {
            self.write("\n")?;
        }

        let text = &text[item.start_position().offset()..item.end_position().offset()];
        self.write(text)?;
        self.state.next_position = item.end_position();
        Ok(())
    }

    pub fn write_comment(&mut self, text: &str, comment: &impl Span) -> Result<()> {
        assert!(!comment.is_empty());

        if self.state.next_position.line() + 1 < comment.start_position().line() {
            self.write("\n")?;
        } else if !matches!(self.last_char(), Some('\n' | ' ')) {
            self.write("  ")?;
        }

        let text = &text[comment.start_position().offset()..comment.end_position().offset()];
        self.state.formatted_text.push_str(text);
        self.state.current_column += text.len();
        self.state.next_position = comment.end_position();
        self.needs_whitespace(Whitespace::Newline);

        Ok(())
    }

    fn last_char(&self) -> Option<char> {
        self.state
            .formatted_text
            .chars()
            .last()
            .or_else(|| self.parent.as_ref().and_then(|x| x.last_char()))
    }

    fn write_whitespace(&mut self) -> Result<()> {
        match self.state.needs_whitespace.take() {
            None => Ok(()),
            Some(Whitespace::Blank) => self.write(" "),
            Some(Whitespace::Newline) => self.write("\n"),
        }
    }

    fn write(&mut self, s: &str) -> Result<()> {
        for c in s.chars() {
            if c == '\n' {
                if self.config.multiline_mode == MultilineMode::Forbid {
                    return Err(Error::Multiline);
                }

                self.state.current_column = 0;
            }
            assert!(!c.is_control());

            if self.state.current_column >= self.config.max_columns {
                if self.config.multiline_mode != MultilineMode::Force {
                    // Should retry with setting `multiline_mode` to `MultilineMode::Force`.
                    return Err(Error::MaxColumnsExceeded);
                } else {
                    // TODO: Emit warning log
                }
            }

            if self.state.current_column < self.config.indent {
                for _ in self.state.current_column..self.config.indent {
                    self.state.formatted_text.push(' ');
                }
                self.state.current_column = self.config.indent;
            }
            self.state.current_column += 1;
            self.state.formatted_text.push(c);
        }
        Ok(())
    }
}
