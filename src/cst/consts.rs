use crate::parse::{self, Parse, Parser};
use crate::token::{Keyword, KeywordToken, Symbol, SymbolToken, Token};
use efmt_derive::{Format, Region};

#[derive(Debug, Clone, Region, Format)]
pub struct Receive(KeywordToken);

impl Parse for Receive {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::Receive).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct End(KeywordToken);

impl Parse for End {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::End).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Case(KeywordToken);

impl Parse for Case {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::Case).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Of(KeywordToken);

impl Parse for Of {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::Of).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct After(KeywordToken);

impl Parse for After {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Keyword::After).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Hyphen(SymbolToken);

impl Parse for Hyphen {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Hyphen).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Dot(SymbolToken);

impl Parse for Dot {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Dot).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Colon(SymbolToken);

impl Parse for Colon {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Colon).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct RightArrow(SymbolToken);

impl Parse for RightArrow {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::RightArrow).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Question(SymbolToken);

impl Question {
    pub fn new(token: Token) -> std::result::Result<Self, Token> {
        match token {
            Token::Symbol(x) if x.value() == Symbol::Question => Ok(Self(x)),
            x => Err(x),
        }
    }
}

impl Parse for Question {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Question).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Slash(SymbolToken);

impl Parse for Slash {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Slash).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Semicolon(SymbolToken);

impl Parse for Semicolon {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Semicolon).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct Comma(SymbolToken);

impl Parse for Comma {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::Comma).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct OpenParen(SymbolToken);

impl Parse for OpenParen {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::OpenParen).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct CloseParen(SymbolToken);

impl Parse for CloseParen {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::CloseParen).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct OpenSquare(SymbolToken);

impl Parse for OpenSquare {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::OpenSquare).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct CloseSquare(SymbolToken);

impl Parse for CloseSquare {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::CloseSquare).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct OpenBrace(SymbolToken);

impl Parse for OpenBrace {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::OpenBrace).map(Self)
    }
}

#[derive(Debug, Clone, Region, Format)]
pub struct CloseBrace(SymbolToken);

impl Parse for CloseBrace {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        parser.expect(Symbol::CloseBrace).map(Self)
    }
}
