use crate::cst::primitives::{Atom, Integer, NameAndArity, Parenthesized, String, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Either, Parse, Parser};
use crate::token::{Keyword, Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum Expr {
    Atom(Atom),
    Variable(Variable),
    String(String),
    Integer(Integer),
    List(Box<List>),
    FunCall(Box<FunCall>),
    NameAndArity(NameAndArity<Atom, Integer>), // For attributes such as `-export`.
}

impl Region for Expr {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
            Self::String(x) => x.region(),
            Self::Integer(x) => x.region(),
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
            Self::Variable(x)
        } else if let Some(x) = parser.try_parse() {
            Self::String(x)
        } else if let Some(x) = parser.try_parse() {
            Self::Integer(x)
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
            Self::Variable(x) => x.format(fmt),
            Self::String(x) => x.format(fmt),
            Self::Integer(x) => x.format(fmt),
            Self::List(x) => x.format(fmt),
            Self::FunCall(x) => x.format(fmt),
            Self::NameAndArity(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct List {
    items: Vec<Expr>,
    region: TokenRegion,
}

impl Region for List {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for List {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        parser.expect(Symbol::OpenSquare)?;
        let items = parser.parse_items(Symbol::Comma)?;
        parser.expect(Symbol::CloseSquare)?;
        Ok(Self {
            items,
            region: parser.region(start),
        })
    }
}

impl Format for List {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "[")?;
        fmt.format_children(&self.items, ",")?;
        write!(fmt, "]")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunClause<Name> {
    name: Name,
    params: Vec<Expr>,
    guard: Option<Guard>,
    body: Body,
    region: TokenRegion,
}

impl<Name> Region for FunClause<Name> {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl<Name: Parse> Parse for FunClause<Name> {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let name = parser.parse()?;
        parser.expect(Symbol::OpenParen)?;
        let params = parser.parse_items(Symbol::Comma)?;
        parser.expect(Symbol::CloseParen)?;
        let guard = parser.try_parse();
        parser.expect(Symbol::RightArrow)?;
        let body = parser.parse()?;
        Ok(Self {
            name,
            params,
            guard,
            body,
            region: parser.region(start),
        })
    }
}

impl<Name: Format> Format for FunClause<Name> {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.name)?;
        write!(fmt, "(")?;
        fmt.format_children(&self.params, ",")?;
        write!(fmt, ")")?;
        if let Some(guard) = &self.guard {
            fmt.format(guard)?;
        }
        write!(fmt, "->")?;
        fmt.format(&self.body)?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    exprs: Vec<Expr>,
    region: TokenRegion,
}

impl Region for Body {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Body {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let exprs = parser.parse_items(Symbol::Comma)?;
        Ok(Self {
            exprs,
            region: parser.region(start),
        })
    }
}

impl Format for Body {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format_children(&self.exprs, ",")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Guard {}

impl Region for Guard {
    fn region(&self) -> &TokenRegion {
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
    name: Either<Parenthesized<Expr>, Either<Atom, Variable>>,
}

impl Region for FunCallName {
    fn region(&self) -> &TokenRegion {
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
    module: Either<Parenthesized<Expr>, Either<Atom, Variable>>,
    region: TokenRegion,
}

impl Region for FunCallModule {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for FunCallModule {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let module = parser.parse()?;
        parser.expect(Symbol::Colon)?;
        Ok(Self {
            module,
            region: parser.region(start),
        })
    }
}

impl Format for FunCallModule {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.format(&self.module)?;
        write!(fmt, ":")?;
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FunCall {
    module: Option<FunCallModule>,
    name: FunCallName,
    args: Vec<Expr>,
    region: TokenRegion,
}

impl Region for FunCall {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for FunCall {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let module = parser.try_parse();
        let name = parser.parse()?;
        parser.expect(Symbol::OpenParen)?;
        let args = parser.parse_items(Symbol::Comma)?;
        parser.expect(Symbol::CloseParen)?;
        Ok(Self {
            module,
            name,
            args,
            region: parser.region(start),
        })
    }
}

impl Format for FunCall {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        if let Some(module) = &self.module {
            fmt.format(module)?;
        }
        fmt.format(&self.name)?;
        write!(fmt, "(")?;
        fmt.format_children(&self.args, ",")?;
        write!(fmt, ")")?;
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
