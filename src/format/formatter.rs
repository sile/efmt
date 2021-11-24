use crate::format::writer::{Error, RegionConfig, RegionWriter, Result};
use crate::format::Format;
use crate::items::tokens::{CommentKind, CommentToken, VisibleToken};
use crate::parse::TokenStream;
use crate::span::{Position, Span};

#[derive(Debug)]
pub struct Formatter {
    ts: TokenStream,
    item: RootItem,
    last_comment_or_macro_position: Option<Position>, // TODO: rename
    next_position: Position,
    last_token: Option<VisibleToken>,
    skip_whitespaces: bool,
    last_skipped_whitespace: Option<Item>,
}

impl Formatter {
    pub fn new(ts: TokenStream) -> Self {
        Self {
            ts,
            item: RootItem::new(),
            last_comment_or_macro_position: None,
            next_position: Position::new(0, 0, 0),
            last_token: None,
            skip_whitespaces: false,
            last_skipped_whitespace: None,
        }
    }

    pub fn add_token(&mut self, token: VisibleToken) {
        self.add_token_or_trailing_comment(token, false);
    }

    fn add_token_or_trailing_comment(&mut self, token: VisibleToken, trailing_comment: bool) {
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
            Some(Item::Space { .. }) => self.add_space(),
            Some(Item::Newline) => self.add_newline(),
            None => {}
            _ => unreachable!(),
        }

        if let Some(last) = &self.last_token {
            if last.needs_space(&token) {
                self.add_space();
            }
        }

        self.last_token = Some(token.clone());
        if trailing_comment {
            self.item.add_trailing_comment(token);
        } else {
            self.item.add_token(token);
        }

        assert!(self.next_position <= end_position);
        self.next_position = end_position;
    }

    pub fn add_span(&mut self, span: &impl Span) {
        let next_macro_start = self.next_macro_start();
        if next_macro_start != span.start_position() {
            self.add_macros_and_comments(span.start_position());
        }

        self.item.add_span(span);
        self.last_token = None;
        assert!(self.next_position <= span.end_position());
        self.next_position = span.end_position();
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

    pub fn add_spaces(&mut self, n: usize) {
        if self.skip_whitespaces {
            self.last_skipped_whitespace = Some(Item::Space(n));
            return;
        }
        self.item.add_space(n);
    }

    pub fn add_space(&mut self) {
        if self.skip_whitespaces {
            self.last_skipped_whitespace = Some(Item::Space(1));
            return;
        }
        self.item.add_space(1);
    }

    pub fn add_newline(&mut self) {
        if self.skip_whitespaces {
            self.last_skipped_whitespace = Some(Item::Newline);
            return;
        }
        self.item.add_newline();
    }

    pub fn add_comment(&mut self, comment: CommentToken) {
        match comment.kind() {
            CommentKind::Post => {
                self.add_newline();
                self.add_token(VisibleToken::Comment(comment));
                self.add_newline();
            }
            CommentKind::Trailing => {
                self.add_token_or_trailing_comment(VisibleToken::Comment(comment), true);
                self.add_newline();
            }
        }
    }

    pub fn subregion<F>(&mut self, indent: Indent, newline: Newline, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.item.enter_subregion(indent, newline);
        f(self);
        self.item.leave_subregion();
    }

    pub fn token_stream_mut(&mut self) -> &mut TokenStream {
        &mut self.ts
    }

    pub fn format(mut self, max_columns: usize) -> String {
        self.add_macros_and_comments(Position::new(usize::MAX - 1, usize::MAX, usize::MAX));

        let root = std::mem::replace(&mut self.item, RootItem::new());
        ItemToString::new(self, max_columns).into_string(&root.item)
    }
}

#[derive(Debug)]
struct ItemToString {
    writer: RegionWriter,
    max_columns: usize,
    fmt: Formatter,
}

impl ItemToString {
    fn new(fmt: Formatter, max_columns: usize) -> Self {
        Self {
            writer: RegionWriter::new(),
            max_columns,
            fmt,
        }
    }

    fn into_string(mut self, item: &Item) -> String {
        self.format_item(item).expect("TODO: bug");
        self.writer.formatted_text().to_owned()
    }

    fn format_item(&mut self, item: &Item) -> Result<()> {
        match item {
            Item::Token(x) => self.format_token(x)?,
            Item::Span {
                start_position,
                end_position,
            } => self.format_span(*start_position, *end_position)?,
            Item::Space(n) => self.format_space(*n)?,
            Item::Newline => self.format_newline()?,
            Item::Region {
                indent,
                newline,
                items,
                ..
            } => self.format_region(indent, newline, items)?,
        }
        Ok(())
    }

    fn format_items(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            self.format_item(item)?;
        }
        Ok(())
    }

    fn format_token(&mut self, token: &VisibleToken) -> Result<()> {
        let is_comment = matches!(token, VisibleToken::Comment(_));
        self.writer
            .write_item(&self.fmt.ts.text(), token, is_comment)
    }

    fn format_span(&mut self, start_position: Position, end_position: Position) -> Result<()> {
        self.writer
            .write_item(&self.fmt.ts.text(), &(start_position, end_position), false)
    }

    fn format_space(&mut self, n: usize) -> Result<()> {
        self.writer.write_space(n)
    }

    fn format_newline(&mut self) -> Result<()> {
        self.writer.write_newline()
    }

    fn format_region(&mut self, indent: &Indent, newline: &Newline, items: &[Item]) -> Result<()> {
        let indent = match indent {
            Indent::Offset(n) => self.writer.config().indent + n,
            Indent::ParentOffset(n) => self.writer.parent_indent() + n,
            Indent::CurrentColumn => {
                if self.writer.current_column() == 0 {
                    self.writer.config().indent
                } else {
                    self.writer.current_column()
                }
            }
        };
        let mut needs_newline = false;
        let mut allow_multi_line = true;
        let mut allow_too_long_line = true;
        match newline {
            Newline::Always => {
                needs_newline = true;
            }
            Newline::Never => {}
            Newline::If(cond) => {
                allow_multi_line = !cond.multi_line;
                allow_too_long_line = !cond.too_long;

                if cond.multi_line_parent && self.writer.config().allow_multi_line {
                    needs_newline = true;
                }
            }
        };

        let check_multi_line = items.iter().any(|item| {
            if let Item::Region {
                newline:
                    Newline::If(NewlineIf {
                        multi_line_parent: true,
                        ..
                    }),
                ..
            } = item
            {
                return true;
            }
            false
        });
        if check_multi_line {
            allow_multi_line = false;
        }

        let config = RegionConfig {
            indent,
            max_columns: if allow_too_long_line {
                None
            } else {
                Some(self.max_columns)
            },
            allow_multi_line,
        };
        let result = self.with_subregion(config, |this| {
            if needs_newline {
                this.writer.write_newline()?;
            }
            this.format_items(items)
        });
        if result.is_err() {
            let (retry, needs_newline) = match &result {
                Err(Error::MultiLine) if check_multi_line => (true, needs_newline),
                Err(Error::MultiLine) if !allow_multi_line => (true, true),
                Err(Error::LineTooLong) if !allow_too_long_line => (true, true),
                _ => (false, false),
            };
            if retry {
                let config = RegionConfig {
                    indent,
                    max_columns: None,
                    allow_multi_line: true,
                };

                return self.with_subregion(config, |this| {
                    if needs_newline {
                        let column_before_newline = this.writer.current_column();
                        if indent < column_before_newline {
                            this.writer.write_newline()?;
                        }
                    }
                    this.format_items(items)
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
struct RootItem {
    item: Item,
    prev: Option<Box<Self>>,
}

impl RootItem {
    fn new() -> Self {
        Self {
            item: Item::new(),
            prev: None,
        }
    }

    fn enter_subregion(&mut self, indent: Indent, newline: Newline) {
        let prev = std::mem::replace(
            self,
            Self {
                item: Item::Region {
                    indent,
                    newline,
                    items: Vec::new(),
                },
                prev: None,
            },
        );
        self.prev = Some(Box::new(prev));
    }

    fn leave_subregion(&mut self) {
        let prev = self.prev.take().expect("bug");
        let item = std::mem::replace(self, *prev).item;

        // If the items that were added during this subregion only contain macro expanded tokens,
        // `item.is_empty()` returns `true`.
        if !item.is_empty() {
            self.item.add_region(item);
        }
    }

    fn add_token(&mut self, token: VisibleToken) {
        self.item.add_token(token)
    }

    fn add_trailing_comment(&mut self, token: VisibleToken) {
        if let Err(token) = self.item.add_trailing_comment(token) {
            self.prev.as_mut().expect("bug").add_trailing_comment(token);
        }
    }

    fn add_span(&mut self, span: &impl Span) {
        self.item.add_span(span)
    }

    fn add_space(&mut self, n: usize) {
        self.item.add_space(n)
    }

    fn add_newline(&mut self) {
        self.item.add_newline()
    }
}

#[derive(Debug)]
enum Item {
    Token(VisibleToken),
    Span {
        start_position: Position,
        end_position: Position,
    },
    Space(usize),
    Newline,
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

    fn add_trailing_comment(
        &mut self,
        token: VisibleToken,
    ) -> std::result::Result<(), VisibleToken> {
        if self
            .end_position()
            .map_or(true, |p| token.end_position() < p)
        {
            return Err(token);
        }

        if let Self::Region { items, .. } = self {
            while matches!(items.last(), Some(Self::Space(_) | Self::Newline)) {
                items.pop();
            }
            match items.last_mut() {
                Some(last @ Self::Region { .. }) => last.add_trailing_comment(token),
                Some(Self::Token(_) | Self::Span { .. }) => {
                    items.push(Self::Space(2));
                    items.push(Self::Token(token));
                    Ok(())
                }
                Some(Self::Space(_) | Self::Newline) => unreachable!(),
                None => Err(token),
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

    fn add_space(&mut self, n: usize) {
        if let Self::Region { items, .. } = self {
            if let Some(last @ Self::Region { .. }) = items.last_mut() {
                last.add_space(n);
            } else {
                items.push(Self::Space(n));
            }
        } else {
            unreachable!();
        }
    }

    fn add_newline(&mut self) {
        if let Self::Region { items, .. } = self {
            items.push(Self::Newline);
        } else {
            unreachable!();
        }
    }

    fn is_empty(&self) -> bool {
        if let Self::Region { items, .. } = self {
            items.iter().all(|item| match item {
                Self::Region { items, .. } => items.is_empty(),
                Self::Space(_) | Self::Newline => true,
                _ => false,
            })
        } else {
            unreachable!();
        }
    }

    fn end_position(&self) -> Option<Position> {
        if let Self::Region { items, .. } = self {
            for item in items {
                match item {
                    region @ Self::Region { .. } => {
                        if let Some(position) = region.end_position() {
                            return Some(position);
                        }
                    }
                    Self::Span { start_position, .. } => return Some(*start_position),
                    Self::Token(x) => return Some(x.start_position()),
                    Self::Newline | Self::Space(_) => {}
                }
            }
            None
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
}

impl Indent {
    pub fn inherit() -> Self {
        Self::Offset(0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Newline {
    Always,
    Never,
    If(NewlineIf),
}

impl Newline {
    pub fn if_too_long_or_multi_line_parent() -> Self {
        Newline::If(NewlineIf {
            too_long: true,
            multi_line_parent: true,
            ..Default::default()
        })
    }

    pub fn if_too_long_or_multi_line() -> Self {
        Newline::If(NewlineIf {
            too_long: true,
            multi_line: true,
            ..Default::default()
        })
    }

    pub fn if_too_long() -> Self {
        Newline::If(NewlineIf {
            too_long: true,
            ..Default::default()
        })
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct NewlineIf {
    pub too_long: bool,
    pub multi_line: bool,
    pub multi_line_parent: bool,
}
