use crate::cst::primitives::{Comma, Items, NameAndArity, NonEmptyItems, Parenthesized};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Either, Parse, Parser};
use crate::token::{
    AtomToken, CharToken, FloatToken, IntegerToken, Keyword, Region, StringToken, Symbol,
    SymbolToken, TokenRegion, VariableToken,
};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    String(StringToken),
    Variable(VariableToken),
    List(Box<List>),
    FunCall(Box<FunCall>),
    NameAndArity(NameAndArity<AtomToken, IntegerToken>), // For attributes such as `-export`.
}

impl Region for Expr {
    fn region(&self) -> TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Char(x) => x.region(),
            Self::Float(x) => x.region(),
            Self::Integer(x) => x.region(),
            Self::String(x) => x.region(),
            Self::Variable(x) => x.region(),
            Self::List(x) => x.region(),
            Self::FunCall(x) => x.region(),
            Self::NameAndArity(x) => x.region(),
        }
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let expr = if let Some(x) = parser.try_parse() {
            Self::NameAndArity(x)
        } else if let Some(x) = parser.try_parse() {
            Self::FunCall(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Atom(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Char(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Float(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Integer(x)
        } else if let Some(x) = parser.try_parse() {
            Self::String(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Variable(x)
        } else if let Some(x) = parser.try_parse() {
            Self::List(x)
        } else {
            let e = parser.take_last_error().expect("unreachable");
            return Err(e);
        };
        Ok(expr)
    }
}

impl Format for Expr {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Atom(x) => x.format(fmt),
            Self::Char(x) => x.format(fmt),
            Self::Float(x) => x.format(fmt),
            Self::Integer(x) => x.format(fmt),
            Self::String(x) => x.format(fmt),
            Self::Variable(x) => x.format(fmt),
            Self::List(x) => x.format(fmt),
            Self::FunCall(x) => x.format(fmt),
            Self::NameAndArity(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct List {
    open: SymbolToken,
    items: Items<Expr, Comma>,
    close: SymbolToken,
}

impl Region for List {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.open.region().start(), self.close.region().end())
    }
}

impl Parse for List {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            open: parser.expect(Symbol::OpenSquare)?,
            items: parser.parse()?,
            close: parser.expect(Symbol::CloseSquare)?,
        })
    }
}

impl Format for List {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.open)?;
        fmt.format(&self.items)?;
        fmt.format(&self.close)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunClause<Name> {
    name: Name,
    params: Parenthesized<Items<Expr, Comma>>,
    guard: Option<Guard>,
    arrow: SymbolToken,
    body: Body,
}

impl<Name: Region> Region for FunClause<Name> {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.name.region().start(), self.body.region().end())
    }
}

impl<Name: Parse> Parse for FunClause<Name> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            name: parser.parse()?,
            params: parser.parse()?,
            guard: parser.try_parse(),
            arrow: parser.expect(Symbol::RightArrow)?,
            body: parser.parse()?,
        })
    }
}

impl<Name: Format> Format for FunClause<Name> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.name)?;
        fmt.format(&self.params)?;
        fmt.format_option(&self.guard)?;
        fmt.format(&self.arrow)?;
        fmt.format(&self.body)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    exprs: NonEmptyItems<Expr, Comma>,
}

impl Region for Body {
    fn region(&self) -> TokenRegion {
        self.exprs.region()
    }
}

impl Parse for Body {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            exprs: parser.parse()?,
        })
    }
}

impl Format for Body {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.exprs)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Guard {}

impl Region for Guard {
    fn region(&self) -> TokenRegion {
        todo!()
    }
}

impl Parse for Guard {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::When)?;
        todo!()
    }
}

impl Format for Guard {
    fn format<W: Write>(&self, _fmt: &mut Formatter<W>) -> format::Result<()> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub struct FunCallName {
    name: Either<Parenthesized<Expr>, Either<AtomToken, VariableToken>>,
}

impl Region for FunCallName {
    fn region(&self) -> TokenRegion {
        self.name.region()
    }
}

impl Parse for FunCallName {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            name: parser.parse()?,
        })
    }
}

impl Format for FunCallName {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.name)
    }
}

#[derive(Debug, Clone)]
pub struct FunCallModule {
    module: Either<Parenthesized<Expr>, Either<AtomToken, VariableToken>>,
    colon: SymbolToken,
}

impl Region for FunCallModule {
    fn region(&self) -> TokenRegion {
        TokenRegion::new(self.module.region().start(), self.colon.region().end())
    }
}

impl Parse for FunCallModule {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            module: parser.parse()?,
            colon: parser.expect(Symbol::Colon)?,
        })
    }
}

impl Format for FunCallModule {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.module)?;
        fmt.format(&self.colon)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunCall {
    module: Option<FunCallModule>,
    name: FunCallName,
    args: Parenthesized<Items<Expr, Comma>>,
}

impl Region for FunCall {
    fn region(&self) -> TokenRegion {
        let start = if let Some(x) = &self.module {
            x.region().start()
        } else {
            self.name.region().start()
        };
        TokenRegion::new(start, self.args.region().end())
    }
}

impl Parse for FunCall {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        Ok(Self {
            module: parser.try_parse(),
            name: parser.parse()?,
            args: parser.parse()?,
        })
    }
}

impl Format for FunCall {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_option(&self.module)?;
        fmt.format(&self.name)?;
        fmt.format(&self.args)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::test_parse_and_format;

    #[test]
    fn list_works() {
        for testname in ["atom"] {
            test_parse_and_format::<Expr>(&format!("cst/expressions/list-{}", testname))
                .expect(testname);
        }
    }
}
