use crate::cst::consts::{CloseParen, OpenParen, Slash};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, TokenPosition, TokenRegion};
use efmt_derive::{Format, Parse, Region};
use std::io::Write;

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D> {
    items: Vec<T>,
    delimiters: Vec<D>,
}

impl<T, D> NonEmptyItems<T, D> {
    pub fn items(&self) -> &[T] {
        &self.items
    }

    pub fn delimiters(&self) -> &[D] {
        &self.delimiters
    }
}
impl<T, D> Region for NonEmptyItems<T, D>
where
    T: Region,
    D: Region,
{
    fn region(&self) -> TokenRegion {
        if let (Some(first), Some(last)) = (self.items.first(), self.items.last()) {
            TokenRegion::new(first.region().start(), last.region().end())
        } else {
            unreachable!()
        }
    }
}
impl<T, D> Parse for NonEmptyItems<T, D>
where
    T: Parse,
    D: Parse,
{
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let mut items = vec![parser.parse()?];
        let mut delimiters = Vec::new();
        while let Some(delimiter) = parser.try_parse() {
            delimiters.push(delimiter);
            items.push(parser.parse()?);
        }
        Ok(Self { items, delimiters })
    }
}

impl<T, D> Format for NonEmptyItems<T, D>
where
    T: Format,
    D: Format,
{
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_items(&self.items, &self.delimiters)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Items<T, D>(Maybe<NonEmptyItems<T, D>>);

impl<T, D> Items<T, D> {
    pub fn items(&self) -> &[T] {
        if let Some(x) = self.0.get() {
            x.items()
        } else {
            &[]
        }
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct NameAndArity<Name, Arity> {
    name: Name,
    slash: Slash,
    arity: Arity,
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct Parenthesized<T> {
    open: OpenParen,
    item: T,
    close: CloseParen,
}

impl<T> Parenthesized<T> {
    pub fn get(&self) -> &T {
        &self.item
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct EnterBlock<T>(T);

impl<T: Format> Format for EnterBlock<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.0)?;
        fmt.enter_block()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct LeaveBlock<T>(T);

impl<T: Format> Format for LeaveBlock<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.leave_block()?;
        fmt.format(&self.0)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct NeedNewline<T>(T);

impl<T> NeedNewline<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for NeedNewline<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.0)?;
        fmt.write_newline()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct NeedSpace<T>(T);

impl<T> NeedSpace<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for NeedSpace<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_space()?;
        fmt.format(&self.0)?;
        fmt.format_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct NeedRightSpace<T>(T);

impl<T> NeedRightSpace<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for NeedRightSpace<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.0)?;
        fmt.format_space()?;
        Ok(())
    }
}

#[derive(Debug, Clone, Region, Parse)]
pub struct NewlineIfMultiline<T>(T);

impl<T> NewlineIfMultiline<T> {
    pub fn get(&self) -> &T {
        &self.0
    }
}

impl<T: Format> Format for NewlineIfMultiline<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_with_newline_if_multiline(&self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Maybe<T> {
    item: Option<T>,
    prev_token_end: TokenPosition,
    next_token_start: TokenPosition,
}

impl<T> Maybe<T> {
    pub fn get(&self) -> Option<&T> {
        self.item.as_ref()
    }
}

impl<T: Region> Region for Maybe<T> {
    fn region(&self) -> TokenRegion {
        match &self.item {
            Some(x) => x.region(),
            None => {
                // TODO: add note comment
                TokenRegion::new(self.next_token_start, self.prev_token_end)
            }
        }
    }
}

impl<T: Parse> Parse for Maybe<T> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let prev_token_end = parser.current_position();
        let next_token_start = parser
            .peek_token()?
            .map(|x| x.region().start())
            .unwrap_or(prev_token_end);
        Ok(Self {
            item: parser.try_parse(),
            prev_token_end,
            next_token_start,
        })
    }
}

impl<T: Format> Format for Maybe<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        if let Some(x) = &self.item {
            fmt.format(x)?;
        }
        Ok(())
    }
}
