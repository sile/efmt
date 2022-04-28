use crate::format::writer::{Error, RegionConfig, Result, Writer};
use crate::format::Format;
use crate::items::tokens::{CommentToken, VisibleToken};
use crate::parse::TokenStream;
use crate::span::{Position, Span};
use std::num::NonZeroUsize;

#[derive(Debug)]
pub struct Formatter {
    ts: TokenStream,
    item: Item,
    last_comment_or_macro_position: Option<Position>,
    next_position: Position,
    last_token: Option<VisibleToken>,
    skip_whitespaces: bool,
    last_skipped_whitespace: Option<Item>,
}

impl Formatter {
    pub fn new(ts: TokenStream) -> Self {
        Self {
            ts,
            item: Item::new(),
            last_comment_or_macro_position: None,
            next_position: Position::new(0, 0, 0),
            last_token: None,
            skip_whitespaces: false,
            last_skipped_whitespace: None,
        }
    }

    pub fn token_stream(&self) -> &TokenStream {
        &self.ts
    }

    pub fn skip_formatting(&mut self) {
        let position = self.find_format_on_position(self.next_position);
        self.add_span(&(self.next_position, position));
        self.add_newline();
    }

    pub fn add_token(&mut self, token: VisibleToken) {
        let start_position = token.start_position();
        let end_position = token.end_position();

        self.add_macros_and_comments(start_position);

        if start_position < self.next_position {
            // Macro expanded token.
            self.last_skipped_whitespace = None;
            return;
        }
        self.skip_whitespaces = false;
        match self.last_skipped_whitespace.take() {
            Some(Item::Space) => self.item.add_space(),
            Some(Item::Newline { count }) => self.item.add_newlines(count),
            None => {}
            _ => unreachable!(),
        }

        if let Some(last) = &self.last_token {
            if last.needs_space(&token) {
                self.add_space();
            }
        }

        self.last_token = Some(token.clone());
        self.item.add_token(token);

        assert!(self.next_position <= end_position);
        self.next_position = end_position;
    }

    pub fn add_span(&mut self, span: &impl Span) {
        let next_macro_start = self.next_macro_start();
        let mut span_start = span.start_position();
        let span_end = span.end_position();

        if next_macro_start != span.start_position() {
            self.add_macros_and_comments(span.start_position());
            if span_start < self.next_position {
                span_start = self.next_position;
            }
        }

        self.item.add_span(&(span_start, span_end));
        self.last_token = None;
        assert!(self.next_position <= span_end);
        self.next_position = span_end;
    }

    fn add_macros_and_comments(&mut self, next_position: Position) {
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
                self.add_comment(comment);
            } else {
                let r#macro = self.ts.macros()[&next_macro_start].clone();
                self.last_comment_or_macro_position = Some(next_macro_start);
                r#macro.format(self);
                self.skip_whitespaces = true;
            }
        }
    }

    fn next_comment_start(&self) -> Position {
        self.ts
            .comments()
            .range(self.next_position..)
            .next()
            .map(|(k, _)| *k)
            .unwrap_or_else(|| Position::new(usize::MAX, usize::MAX, usize::MAX))
    }

    fn next_macro_start(&self) -> Position {
        self.ts
            .macros()
            .range(self.next_position..)
            .next()
            .map(|(k, _)| *k)
            .unwrap_or_else(|| Position::new(usize::MAX, usize::MAX, usize::MAX))
    }

    pub fn add_space(&mut self) {
        if self.skip_whitespaces {
            self.last_skipped_whitespace = Some(Item::Space);
            return;
        }
        self.item.add_space();
    }

    pub fn add_newline(&mut self) {
        self.add_newlines(NonZeroUsize::new(1).expect("unreachable"));
    }

    pub fn add_newlines(&mut self, count: NonZeroUsize) {
        if self.skip_whitespaces {
            self.last_skipped_whitespace = Some(Item::Newline { count });
            return;
        }
        self.item.add_newlines(count);
    }

    pub fn add_comment(&mut self, comment: CommentToken) {
        if !comment.is_trailing() {
            self.add_newline();
        }
        // Note that two spaces before trailing comments will be added just before writing them
        // in `Writer::write_trailing_comment()`.

        match comment.text(&self.ts.text()).parse() {
            Err(()) => {}
            Ok(Directive::FormatOn) => {
                log::warn!("Found a `@efmt:on` comment at line {} without a preceeding `@efmt:off` (just ignored).",
                           comment.start_position().line());
            }
            Ok(Directive::FormatOff) => {
                self.next_position = comment.start_position();
                self.skip_formatting();
                return;
            }
        }

        self.add_token(VisibleToken::Comment(comment));
        self.add_newline();
    }

    fn find_format_on_position(&self, current: Position) -> Position {
        self.ts
            .comments()
            .range(current..)
            .find(|c| matches!(c.text(&self.ts.text()).parse(), Ok(Directive::FormatOn)))
            .map(|c| c.end_position())
            .unwrap_or_else(|| Position::new(self.ts.text().len(), usize::MAX, usize::MAX))
    }

    pub fn subregion<F>(&mut self, indent: Indent, newline: Newline, f: F)
    where
        F: FnOnce(&mut Self),
    {
        let prev = std::mem::replace(
            &mut self.item,
            Item::Region {
                indent,
                newline,
                items: Vec::new(),
            },
        );
        f(self);
        let new = std::mem::replace(&mut self.item, prev);

        // If the items that were added during this subregion only contain macro expanded tokens,
        // `item.is_empty()` returns `true`.
        if !new.is_empty() {
            self.item.add_region(new);
        }
    }

    pub(crate) fn token_stream_mut(&mut self) -> &mut TokenStream {
        &mut self.ts
    }

    pub fn format(mut self, max_columns: usize) -> String {
        self.add_macros_and_comments(Position::new(usize::MAX - 1, usize::MAX, usize::MAX));
        ItemWriter::new(&self.ts.text(), max_columns).write_to_string(&self.item)
    }
}

#[derive(Debug)]
struct ItemWriter<'a> {
    writer: Writer,
    text: &'a str,
}

impl<'a> ItemWriter<'a> {
    fn new(text: &'a str, max_columns: usize) -> Self {
        Self {
            writer: Writer::new(max_columns),
            text,
        }
    }

    fn write_to_string(mut self, item: &Item) -> String {
        self.write_item(item).expect("bug");
        self.writer.finish()
    }

    fn write_item(&mut self, item: &Item) -> Result<()> {
        match item {
            Item::Region {
                indent,
                newline,
                items,
                ..
            } => self.write_region(indent, newline, items)?,
            Item::Span {
                start_position,
                end_position,
            } => self.write_span(*start_position, *end_position)?,
            Item::Token(x) => self.write_token(x)?,
            Item::Space => self.writer.write_space()?,
            Item::Newline { count } => self.writer.write_newlines(*count)?,
        }
        Ok(())
    }

    fn write_items(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            self.write_item(item)?;
        }
        Ok(())
    }

    fn write_token(&mut self, token: &VisibleToken) -> Result<()> {
        if token.is_trailing_comment() {
            self.writer.write_trailing_comment(self.text, token)
        } else {
            self.writer.write_span(self.text, token, token.is_comment())
        }
    }

    fn write_span(&mut self, start_position: Position, end_position: Position) -> Result<()> {
        self.writer
            .write_span(self.text, &(start_position, end_position), false)
    }

    fn write_region(&mut self, indent: &Indent, newline: &Newline, items: &[Item]) -> Result<()> {
        let indent = match indent {
            Indent::Offset(n) => self.writer.current_indent() + n,
            Indent::ParentOffset(n) => self.writer.parent_indent() + n,
            Indent::CurrentColumn => {
                if self.writer.current_column() == 0 {
                    self.writer.current_indent()
                } else {
                    self.writer.current_column()
                }
            }
            Indent::Absolute(n) => *n,
        };
        let mut needs_newline = false;
        let mut allow_multi_line = true;
        let mut allow_too_long_line = true;
        let parent_allow_multi_line = self.writer.is_multi_line_allowed();
        let mut force_noimprove_newline = false;
        match newline {
            Newline::Always => {
                needs_newline = true;
            }
            Newline::Never => {}
            Newline::IfTooLong => {
                allow_too_long_line = false;
            }
            Newline::IfTooLongOrMultiLine => {
                allow_too_long_line = false;
                allow_multi_line = false;
            }
            Newline::IfTooLongOrMultiLineParent | Newline::IfTooLongOrMultiLineParentForce => {
                if parent_allow_multi_line {
                    needs_newline = true;
                } else {
                    allow_too_long_line = false;
                    allow_multi_line = false;
                }
                if *newline == Newline::IfTooLongOrMultiLineParentForce {
                    force_noimprove_newline = true;
                }
            }
        };

        let check_multi_line = items.iter().any(|item| {
            matches!(
                item,
                Item::Region {
                    newline: Newline::IfTooLongOrMultiLineParent,
                    ..
                }
            )
        });
        if check_multi_line {
            allow_multi_line = false;
        }

        let config = RegionConfig {
            indent,
            allow_too_long_line,
            allow_multi_line,
        };
        let result = self.with_subregion(config, |this| {
            if needs_newline {
                this.writer.write_newline()?;
            }
            this.write_items(items)
        });
        if result.is_err() {
            if !parent_allow_multi_line {
                return Err(Error::MultiLine);
            }

            let (retry, needs_newline) = match &result {
                Err(Error::MultiLine) if check_multi_line => (true, needs_newline),
                Err(Error::MultiLine) if !allow_multi_line => (true, true),
                Err(Error::LineTooLong) if !allow_too_long_line => (true, true),
                _ => (false, false),
            };
            if retry {
                let config = RegionConfig {
                    indent,
                    allow_too_long_line: true,
                    allow_multi_line: true,
                };

                return self.with_subregion(config, |this| {
                    if needs_newline
                        && (force_noimprove_newline || indent < this.writer.current_column())
                    {
                        this.writer.write_newline()?;
                    }
                    this.write_items(items)
                });
            }
        }
        result
    }

    fn with_subregion<F>(&mut self, config: RegionConfig, f: F) -> Result<()>
    where
        F: FnOnce(&mut Self) -> Result<()>,
    {
        self.writer.start_subregion(config);
        let result = f(self);
        if result.is_ok() {
            self.writer.commit_subregion();
        } else {
            self.writer.abort_subregion();
        }
        result
    }
}

#[derive(Debug)]
enum Item {
    Token(VisibleToken),
    Span {
        start_position: Position,
        end_position: Position,
    },
    Space,
    Newline {
        count: NonZeroUsize,
    },
    Region {
        indent: Indent,
        newline: Newline,
        items: Vec<Item>,
    },
}

impl Item {
    fn new() -> Self {
        Self::Region {
            indent: Indent::CurrentColumn,
            newline: Newline::Never,
            items: Vec::new(),
        }
    }

    fn add_token(&mut self, token: VisibleToken) {
        if let Self::Region { items, .. } = self {
            if let Some(last @ Self::Region { .. }) = items.last_mut() {
                last.add_token(token);
            } else {
                items.push(Self::Token(token));
            }
        } else {
            unreachable!();
        }
    }

    fn add_span(&mut self, span: &impl Span) {
        if let Self::Region { items, .. } = self {
            items.push(Self::Span {
                start_position: span.start_position(),
                end_position: span.end_position(),
            });
        } else {
            unreachable!();
        }
    }

    fn add_region(&mut self, region: Item) {
        if let Self::Region { items, .. } = self {
            items.push(region);
        } else {
            unreachable!();
        }
    }

    fn add_space(&mut self) {
        if let Self::Region { items, .. } = self {
            if let Some(last @ Self::Region { .. }) = items.last_mut() {
                last.add_space();
            } else {
                items.push(Self::Space);
            }
        } else {
            unreachable!();
        }
    }

    fn add_newlines(&mut self, count: NonZeroUsize) {
        if let Self::Region { items, .. } = self {
            items.push(Self::Newline { count });
        } else {
            unreachable!();
        }
    }

    fn is_empty(&self) -> bool {
        if let Self::Region { items, .. } = self {
            items.iter().all(|item| match item {
                Self::Region { items, .. } => items.is_empty(),
                Self::Space | Self::Newline { .. } => true,
                _ => false,
            })
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Indent {
    CurrentColumn,
    Offset(usize),
    ParentOffset(usize),
    Absolute(usize),
}

impl Indent {
    pub const fn inherit() -> Self {
        Self::Offset(0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Newline {
    Always,
    Never,
    IfTooLong,
    IfTooLongOrMultiLine,
    IfTooLongOrMultiLineParent,
    IfTooLongOrMultiLineParentForce,
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

#[cfg(test)]
mod tests {
    use crate::items::Module;

    #[test]
    fn directives_works() {
        let texts = [(
            indoc::indoc! {"
            foo()->foo.

            %% @efmt:off
            bar()->bar.
            %% @efmt:on

            -spec baz() -> list().
            baz()->
                [1,
                 %% @efmt:off
                 2,3,4,
                 %% @efmt:on
                 5,6]."},
            indoc::indoc! {"
            foo() ->
                foo.


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
