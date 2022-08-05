use crate::items::tokens::CommentToken;
use crate::parse::TokenStream;
use crate::span::{Position, Span};

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

const EOF: Position = Position::new(usize::MAX, usize::MAX, usize::MAX);
const EOF_MINUS_1: Position = Position::new(usize::MAX - 1, usize::MAX, usize::MAX);

#[derive(Debug)]
pub struct Formatter {
    ts: TokenStream,
    indent: usize,
    next_comment_indent: Option<usize>,
    column: usize,
    next_position: Position,
    buf: String,
    single_line_mode: bool,
    last_comment_or_macro_position: Option<Position>,
    skipping: bool,
    pending_blank: Option<Blank>,
    is_last_macro: bool,
}

impl Formatter {
    pub fn new(ts: TokenStream) -> Self {
        Self {
            ts,
            indent: 0,
            next_comment_indent: None,
            column: 0,
            next_position: Position::new(0, 0, 0),
            buf: String::new(),
            single_line_mode: false,
            last_comment_or_macro_position: None,
            skipping: false,
            pending_blank: None,
            is_last_macro: false,
        }
    }

    pub fn finish(mut self) -> String {
        self.write_macros_and_comments(EOF_MINUS_1);
        self.buf
    }

    pub fn last_char(&self) -> Option<char> {
        self.buf.chars().last()
    }

    pub fn flush_non_preceding_comments(&mut self, next: &impl Span) {
        for (i, comment_start) in self
            .ts
            .comments()
            .range(self.next_position..next.start_position())
            .map(|(k, _)| *k)
            .rev()
            .enumerate()
        {
            if comment_start.line() == next.start_position().line() - i - 1 {
                continue;
            }

            self.write_macros_and_comments(comment_start);
            break;
        }
    }

    pub(crate) fn token_stream(&self) -> &TokenStream {
        &self.ts
    }

    pub(crate) fn token_stream_mut(&mut self) -> &mut TokenStream {
        &mut self.ts
    }

    pub fn indent(&self) -> usize {
        self.indent
    }

    pub fn set_indent(&mut self, indent: usize) {
        self.indent = indent;
    }

    pub fn column(&self) -> usize {
        self.column
    }

    pub fn has_newline_until(&self, next: &impl Span) -> bool {
        self.next_position.line() != next.start_position().line()
    }

    pub fn write_span(&mut self, span: &impl Span) {
        let start_position = span.start_position();
        self.write_macros_and_comments(start_position);
        if span.end_position() <= self.next_position {
            self.skipping = true;
            self.pending_blank = None;
            return;
        }
        self.skipping = false;

        self.with_multi_line_mode(|this| {
            match this.pending_blank.take() {
                None => {}
                Some(Blank::Space(n)) => this.write_spaces(n),
                Some(Blank::Newline(n)) => this.write_newlines(n),
            }

            if this.next_position.line() + 1 < span.start_position().line()
                && this.is_single_blank_line()
            {
                this.cancel_last_spaces();
                this.write_newlines(2);
            }
        });

        let start = std::cmp::max(start_position.offset(), self.next_position.offset());
        let text = &self.ts.text()[start..span.end_position().offset()];
        if !text.starts_with('%') && !text.is_empty() {
            self.next_comment_indent = None;
        }

        if self.is_last_macro && !matches!(self.buf.chars().last(), Some('\n' | ' ')) {
            if let Some('0'..='9' | 'a'..='z' | 'A'..='Z' | '_') = text.chars().next() {
                self.write_space();
            }
        }
        self.is_last_macro = false;

        self.buf.push_str(text);

        for c in text.chars() {
            if c == '\n' {
                self.column = 0;
            } else {
                self.column += 1;
            }
        }

        assert!(self.next_position < span.end_position());
        self.next_position = span.end_position();
    }

    pub fn write_newlines(&mut self, mut n: usize) {
        if n == 0 {
            return;
        }

        if self.buf.is_empty() {
            return;
        }

        if self.skipping {
            self.pending_blank = Some(Blank::Newline(n));
            return;
        }
        self.pending_blank = None;

        if self.single_line_mode {
            self.write_space();
            return;
        }

        self.cancel_last_spaces();
        for c in self.buf.chars().rev() {
            if c == '\n' {
                n = n.saturating_sub(1);
            } else {
                break;
            }
        }

        for _ in 0..n {
            self.buf.push('\n');
        }

        self.column = 0;
        self.write_spaces(self.indent);
    }

    pub fn write_newline(&mut self) {
        self.write_newlines(1);
    }

    pub fn write_space(&mut self) {
        self.write_spaces(1);
    }

    pub fn write_spaces(&mut self, mut n: usize) {
        if self.skipping {
            self.pending_blank = Some(Blank::Space(n));
            return;
        }
        self.pending_blank = None;

        for c in self.buf.chars().rev() {
            if c == ' ' {
                n = n.saturating_sub(1);
            } else {
                break;
            }
        }

        for _ in 0..n {
            self.buf.push(' ');
            self.column += 1;
        }
    }

    pub fn write_subsequent_comments(&mut self) {
        loop {
            let position = self.next_comment_start();
            if self.next_lexical_token_start() <= position
                || position.line() > self.next_position.line() + 1
            {
                break;
            }

            self.write_macros_and_comments(position);
        }
    }

    pub fn set_next_comment_indent(&mut self, n: usize) {
        self.next_comment_indent = Some(n);
    }

    pub fn with_scoped_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.next_comment_indent = None;

        let indent = self.indent;
        f(self);
        self.indent = indent;
    }

    pub fn with_single_line_mode<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let mode = self.single_line_mode;
        self.single_line_mode = true;
        f(self);
        self.single_line_mode = mode;
    }

    fn with_multi_line_mode<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let mode = self.single_line_mode;
        self.single_line_mode = false;
        f(self);
        self.single_line_mode = mode;
    }

    fn write_macros_and_comments(&mut self, next_position: Position) {
        if self.last_comment_or_macro_position == Some(next_position) {
            return;
        }

        loop {
            let next_comment_start = self.next_comment_start();
            let next_macro_start = self.next_macro_start();
            if next_position < next_comment_start && next_position < next_macro_start {
                break;
            }

            if next_comment_start < next_macro_start {
                let comment = self.ts.comments()[&next_comment_start].clone();
                self.last_comment_or_macro_position = Some(next_comment_start);
                self.write_comment(&comment);
            } else {
                let r#macro = self.ts.macros()[&next_macro_start].clone();
                self.last_comment_or_macro_position = Some(next_macro_start);
                r#macro.format(self);
                self.is_last_macro = true;
            }
        }
    }

    fn write_comment(&mut self, comment: &CommentToken) {
        self.skipping = false;

        let mode = self.single_line_mode;
        self.single_line_mode = false;

        let mut skip = false;
        match comment.text(&self.ts.text()).parse() {
            Err(()) => {}
            Ok(Directive::FormatOn) => {
                log::warn!("Found a `@efmt:on` comment at line {} without a preceeding `@efmt:off` (just ignored).",
                           comment.start_position().line());
            }
            Ok(Directive::FormatOff) => {
                self.next_position = comment.start_position();
                skip = true;
            }
        }

        if skip {
            self.skip_formatting();
        } else {
            if comment.is_trailing() {
                self.cancel_last_newline();
                self.write_spaces(2);
            } else if let Some(comment_indent) = self.next_comment_indent {
                let indent = self.indent;
                self.indent = comment_indent;
                self.write_newline();
                self.indent = indent;
            } else {
                self.write_newline();
            }

            self.write_span(comment);
            self.write_newline();
        }

        self.single_line_mode = mode;
    }

    pub fn skip_formatting(&mut self) {
        let position = self.find_format_on_position(self.next_position);
        self.write_span(&(self.next_position, position));
    }

    fn find_format_on_position(&self, current: Position) -> Position {
        self.ts
            .comments()
            .range(current..)
            .find(|c| matches!(c.text(&self.ts.text()).parse(), Ok(Directive::FormatOn)))
            .map(|c| c.end_position())
            .unwrap_or_else(|| Position::new(self.ts.text().len(), usize::MAX, usize::MAX))
    }

    fn is_single_blank_line(&self) -> bool {
        for (c0, c1) in self.buf.chars().rev().skip(1).zip(self.buf.chars().rev()) {
            match (c0, c1) {
                (' ', ' ') => {}
                ('\n', ' ') => {}
                ('\n', '\n') => return false,
                (_, '\n') => return true,
                _ => return false,
            }
        }
        false
    }

    fn cancel_last_spaces(&mut self) -> usize {
        let mut n = 0;
        while self.buf.ends_with(' ') {
            self.buf.pop();
            n += 1;
        }
        n
    }

    fn cancel_last_newline(&mut self) {
        let n = self.cancel_last_spaces();
        if self.buf.ends_with('\n') {
            self.buf.pop();
        } else {
            self.write_spaces(n);
        }
    }

    fn next_comment_start(&self) -> Position {
        self.ts
            .comments()
            .range(self.next_position..)
            .next()
            .map(|(k, _)| *k)
            .unwrap_or(EOF)
    }

    fn next_macro_start(&self) -> Position {
        self.ts
            .macros()
            .range(self.next_position..)
            .next()
            .map(|(k, _)| *k)
            .unwrap_or(EOF)
    }

    fn next_lexical_token_start(&self) -> Position {
        let tokens = self.ts.visited_tokens();
        let i = tokens
            .binary_search_by_key(&self.next_position, |t| t.start_position())
            .map(|i| i + 1)
            .unwrap_or_else(|i| i);
        tokens.get(i).map(|t| t.start_position()).unwrap_or(EOF)
    }
}

#[derive(Debug)]
enum Directive {
    FormatOn,
    FormatOff,
}

impl std::str::FromStr for Directive {
    type Err = ();
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        match s.trim().trim_start_matches(&[' ', '%'][..]) {
            "@efmt:on" => Ok(Self::FormatOn),
            "@efmt:off" => Ok(Self::FormatOff),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
enum Blank {
    Space(usize),
    Newline(usize),
}

#[cfg(test)]
mod tests {
    use crate::items::Module;

    #[test]
    fn comments_works() {
        let texts = [
            indoc::indoc! {"
            %% comment 1
            -module(foo).


            %% comment 2
            foo() ->
                foo.
            "},
            indoc::indoc! {"
            -record(foo, {
                      foo :: bar
                      %% baz
                     }).
            "},
            indoc::indoc! {"
            foo() ->
                case A of
                    a ->
                        ok
                        %% comment
                end.
            "},
            indoc::indoc! {"
            -module(foo).  % comment 1


            foo() ->  % comment 2
                foo.  %comment 3
            "},
            indoc::indoc! {"
            -include_lib(\"kernel/include/logger.hrl\").


            foo() ->
                ?LOG_INFO(\"hello\"),
                %% comment
                ok.
            "},
            indoc::indoc! {"
            -include_lib(\"kernel/include/logger.hrl\").


            foo() ->
                ?LOG_INFO(\"hello ~p\", [?MODULE]),
                %% comment
                ok.
            "},
            indoc::indoc! {"
            foo() ->
                foo.
            %% foo


            bar() ->
                bar.
            "},
            indoc::indoc! {"
            -type foo() :: integer().
            -type bar() :: integer().  %% bar
            "},
        ];
        for text in texts {
            crate::assert_format!(text, Module);
        }
    }

    #[test]
    fn directives_works() {
        let texts = [(
            indoc::indoc! {"
            foo()->foo.

            %% @efmt:off
            bar()->bar.
            %% @efmt:on

            -spec baz() ->
            list().
            baz()->
                [1,
                 %% @efmt:off
                 2,3,4,
                 %% @efmt:on
                 5,6]."},
            indoc::indoc! {"
            foo() -> foo.


            %% @efmt:off
            bar()->bar.
            %% @efmt:on


            -spec baz() ->
                      list().
            baz() ->
                [1,
                 %% @efmt:off
                 2,3,4,
                 %% @efmt:on
                 5, 6].
            "},
        )];
        for (text, expected) in texts {
            crate::assert_format!(text, expected, Module);
        }
    }
}
