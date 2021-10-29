use crate::format::{self, Format, Formatter};
use crate::parse::{self, Expect, Parse, Parser};
use efmt_derive::Region;
use erl_tokenize::PositionRange;
use std::io::Write;

pub use erl_tokenize::values::{Keyword, Symbol};

// TODO: Span?
pub trait Region {
    fn region(&self) -> TokenRegion;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TokenRegion {
    start: TokenPosition,
    end: TokenPosition,
}

impl TokenRegion {
    pub const fn new(start: TokenPosition, end: TokenPosition) -> Self {
        Self { start, end }
    }

    pub const fn start(self) -> TokenPosition {
        self.start
    }

    pub const fn end(self) -> TokenPosition {
        self.end
    }
}

impl Region for TokenRegion {
    fn region(&self) -> TokenRegion {
        *self
    }
}

impl<T> From<&T> for TokenRegion
where
    T: PositionRange,
{
    fn from(x: &T) -> Self {
        Self::new(x.start_position().into(), x.end_position().into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenIndex(usize);

impl TokenIndex {
    pub const fn new(i: usize) -> Self {
        Self(i)
    }

    pub const fn get(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TokenPosition {
    offset: usize,
    line: usize,
    column: usize,
}

impl TokenPosition {
    pub const fn new(offset: usize, line: usize, column: usize) -> Self {
        Self {
            offset,
            line,
            column,
        }
    }

    pub const fn offset(self) -> usize {
        self.offset
    }

    pub const fn line(self) -> usize {
        self.line
    }
}

impl From<erl_tokenize::Position> for TokenPosition {
    fn from(x: erl_tokenize::Position) -> Self {
        Self::new(x.offset(), x.line(), x.column())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, Region)]
pub enum Token {
    Atom(AtomToken),
    Char(CharToken),
    Float(FloatToken),
    Integer(IntegerToken),
    Keyword(KeywordToken),
    String(StringToken),
    Symbol(SymbolToken),
    Variable(VariableToken),
}

impl From<AtomToken> for Token {
    fn from(x: AtomToken) -> Self {
        Self::Atom(x)
    }
}

impl From<CharToken> for Token {
    fn from(x: CharToken) -> Self {
        Self::Char(x)
    }
}

impl From<FloatToken> for Token {
    fn from(x: FloatToken) -> Self {
        Self::Float(x)
    }
}

impl From<IntegerToken> for Token {
    fn from(x: IntegerToken) -> Self {
        Self::Integer(x)
    }
}

impl From<KeywordToken> for Token {
    fn from(x: KeywordToken) -> Self {
        Self::Keyword(x)
    }
}

impl From<StringToken> for Token {
    fn from(x: StringToken) -> Self {
        Self::String(x)
    }
}

impl From<SymbolToken> for Token {
    fn from(x: SymbolToken) -> Self {
        Self::Symbol(x)
    }
}

impl From<VariableToken> for Token {
    fn from(x: VariableToken) -> Self {
        Self::Variable(x)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct AtomToken {
    value: String,
    region: TokenRegion,
}

impl AtomToken {
    pub fn new(value: &str, region: TokenRegion) -> Self {
        Self {
            value: value.to_owned(),
            region,
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl Region for AtomToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for AtomToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Atom(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(parser, token, "AtomToken")),
        }
    }
}

impl Format for AtomToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CharToken {
    region: TokenRegion,
}

impl CharToken {
    pub const fn new(region: TokenRegion) -> Self {
        Self { region }
    }
}

impl Region for CharToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for CharToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Char(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(parser, token, "CharToken")),
        }
    }
}

impl Format for CharToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FloatToken {
    region: TokenRegion,
}

impl FloatToken {
    pub const fn new(region: TokenRegion) -> Self {
        Self { region }
    }
}

impl Region for FloatToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for FloatToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Float(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(parser, token, "FloatToken")),
        }
    }
}

impl Format for FloatToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntegerToken {
    region: TokenRegion,
}

impl IntegerToken {
    pub const fn new(region: TokenRegion) -> Self {
        Self { region }
    }
}

impl Region for IntegerToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for IntegerToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Integer(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(
                parser,
                token,
                "IntegerToken",
            )),
        }
    }
}

impl Format for IntegerToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct KeywordToken {
    value: Keyword,
    region: TokenRegion,
}

impl KeywordToken {
    pub const fn new(value: Keyword, region: TokenRegion) -> Self {
        Self { value, region }
    }

    pub const fn value(&self) -> Keyword {
        self.value
    }
}

impl Region for KeywordToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for KeywordToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Keyword(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(
                parser,
                token,
                "KeywordToken",
            )),
        }
    }
}

impl Expect for Keyword {
    type Token = KeywordToken;

    fn expect(self, parser: &mut Parser) -> parse::Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == self {
            Ok(token)
        } else {
            Err(parse::Error::unexpected_token(
                parser,
                token.into(),
                &format!("{:?}", self),
            ))
        }
    }
}

impl Format for KeywordToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StringToken {
    region: TokenRegion,
}

impl StringToken {
    pub const fn new(region: TokenRegion) -> Self {
        Self { region }
    }
}

impl Region for StringToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for StringToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::String(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(parser, token, "StringToken")),
        }
    }
}

impl Format for StringToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolToken {
    value: Symbol,
    region: TokenRegion,
}

impl SymbolToken {
    pub const fn new(value: Symbol, region: TokenRegion) -> Self {
        Self { value, region }
    }

    pub const fn value(&self) -> Symbol {
        self.value
    }
}

impl Region for SymbolToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for SymbolToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Symbol(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(parser, token, "SymbolToken")),
        }
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(self, parser: &mut Parser) -> parse::Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == self {
            Ok(token)
        } else {
            Err(parse::Error::unexpected_token(
                parser,
                token.into(),
                &format!("{:?}", self),
            ))
        }
    }
}

impl Format for SymbolToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }

    fn need_spaces_if_macro(&self) -> bool {
        true
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableToken {
    value: String,
    region: TokenRegion,
}

impl VariableToken {
    pub fn new(value: &str, region: TokenRegion) -> Self {
        Self {
            value: value.to_owned(),
            region,
        }
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl Region for VariableToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Parse for VariableToken {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        match parser.read_token()? {
            Token::Variable(token) => Ok(token),
            token => Err(parse::Error::unexpected_token(
                parser,
                token,
                "VariableToken",
            )),
        }
    }
}

impl Format for VariableToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CommentToken {
    region: TokenRegion,
}

impl CommentToken {
    pub const fn new(region: TokenRegion) -> Self {
        Self { region }
    }
}

impl Region for CommentToken {
    fn region(&self) -> TokenRegion {
        self.region
    }
}

impl Format for CommentToken {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        fmt.noformat(self)?;
        Ok(())
    }
}
