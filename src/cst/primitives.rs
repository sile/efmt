use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{Region, Symbol, SymbolToken, TokenPosition, TokenRegion};
use efmt_derive::Region;
use std::io::Write;

#[derive(Debug, Clone, Region)]
pub struct Comma(SymbolToken);

impl Parse for Comma {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Comma).map(Self)
    }
}

impl Format for Comma {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.0)
    }
}

#[derive(Debug, Clone, Region)]
pub struct Semicolon(SymbolToken);

impl Parse for Semicolon {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Semicolon).map(Self)
    }
}

impl Format for Semicolon {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.0)
    }
}

#[derive(Debug, Clone)]
pub struct NonEmptyItems<T, D> {
    items: Vec<T>,
    delimiters: Vec<D>,
}

impl<T, D> NonEmptyItems<T, D> {
    pub fn items(&self) -> &[T] {
        &self.items
    }
}
impl<T, D> Region for NonEmptyItems<T, D>
where
    T: Region,
    D: Region,
{
    fn region(&self) -> TokenRegion {
        TokenRegion::new(
            self.items[0].region().start(),
            self.items[self.items.len() - 1].region().end(),
        )
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
        fmt.format_children(&self.items, &self.delimiters)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Items<T, D> {
    items: Option<NonEmptyItems<T, D>>,
    start_position: TokenPosition,
}

impl<T, D> Items<T, D> {
    pub fn items(&self) -> &[T] {
        if let Some(x) = &self.items {
            x.items()
        } else {
            &[]
        }
    }
}

impl<T, D> Region for Items<T, D>
where
    T: Region,
    D: Region,
{
    fn region(&self) -> TokenRegion {
        self.items
            .as_ref()
            .map(|x| x.region())
            .unwrap_or(TokenRegion::new(self.start_position, self.start_position))
    }
}

impl<T, D> Parse for Items<T, D>
where
    T: Parse,
    D: Parse,
{
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start_position = parser.current_position();
        Ok(Self {
            items: parser.try_parse(),
            start_position,
        })
    }
}

impl<T, D> Format for Items<T, D>
where
    T: Format,
    D: Format,
{
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_option(&self.items)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct NameAndArity<Name, Arity> {
    name: Name,
    slash: SymbolToken,
    arity: Arity,
}

impl<Name, Arity> Region for NameAndArity<Name, Arity>
where
    Name: Region,
    Arity: Region,
{
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.name.region().start(), self.arity.region().end())
    }
}

impl<Name, Arity> Parse for NameAndArity<Name, Arity>
where
    Name: Parse,
    Arity: Parse,
{
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            name: parser.parse()?,
            slash: parser.expect(Symbol::Slash)?,
            arity: parser.parse()?,
        })
    }
}

impl<Name, Arity> Format for NameAndArity<Name, Arity>
where
    Name: Format,
    Arity: Format,
{
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.name)?;
        fmt.format(&self.slash)?;
        fmt.format(&self.arity)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Parenthesized<T> {
    open: SymbolToken,
    item: T,
    close: SymbolToken,
}

impl<T> Parenthesized<T> {
    pub fn get(&self) -> &T {
        &self.item
    }
}

impl<T> Region for Parenthesized<T> {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.open.region().start(), self.close.region().end())
    }
}

impl<T: Parse> Parse for Parenthesized<T> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            open: parser.expect(Symbol::OpenParen)?,
            item: parser.parse()?,
            close: parser.expect(Symbol::CloseParen)?,
        })
    }
}

impl<T: Format> Format for Parenthesized<T> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.open)?;
        fmt.format(&self.item)?;
        fmt.format(&self.close)?;
        Ok(())
    }
}
