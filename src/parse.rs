use crate::lex::{self, Lexer};
use crate::token::{
    AtomToken, LexicalToken, Region, StringToken, Symbol, SymbolToken, TokenPosition, TokenRegion,
    VariableToken,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedTokenValue {
        token: LexicalToken,
        expected: String,
    },

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedToken {
        token: LexicalToken,
        expected: &'static str,
    },

    #[error(transparent)]
    LexError(#[from] lex::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    last_error: Option<(TokenPosition, Error)>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Self {
            lexer,
            last_error: None,
        }
    }

    pub fn current_position(&self) -> TokenPosition {
        self.lexer.current_position()
    }

    pub fn region(&self, start: TokenPosition) -> TokenRegion {
        TokenRegion::new(start, self.current_position())
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        T::parse(self)
    }

    pub fn try_parse<T: Parse>(&mut self) -> Option<T> {
        let start = self.lexer.current_position();
        match self.parse() {
            Ok(x) => Some(x),
            Err(e) => {
                let end = self.lexer.current_position();
                if self.last_error.as_ref().map_or(true, |(p, _)| *p < end) {
                    self.last_error = Some((end, e));
                }
                self.lexer.set_position(start).expect("unreachable");
                None
            }
        }
    }

    pub fn parse_items<T: Parse>(&mut self, delimiter: Symbol) -> Result<Vec<T>> {
        let mut items = if let Some(item) = self.try_parse() {
            vec![item]
        } else {
            return Ok(Vec::new());
        };
        while self.try_expect(delimiter).is_some() {
            let item = self.parse()?;
            items.push(item);
        }
        Ok(items)
    }

    pub fn parse_non_empty_items<T: Parse>(&mut self, delimiter: Symbol) -> Result<Vec<T>> {
        let mut items = Vec::new();
        loop {
            let item = self.parse()?;
            items.push(item);
            if self.try_expect(delimiter).is_none() {
                break;
            }
        }
        Ok(items)
    }

    pub fn expect<T: Expect>(&mut self, expected: T) -> Result<T::Token> {
        expected.expect(self)
    }

    pub fn try_expect<T: Expect>(&mut self, expected: T) -> Option<T::Token> {
        let start = self.lexer.current_position();
        match self.expect(expected) {
            Ok(x) => Some(x),
            Err(e) => {
                let end = self.lexer.current_position();
                if self.last_error.as_ref().map_or(true, |(p, _)| *p < end) {
                    self.last_error = Some((end, e));
                }
                self.lexer.set_position(start).expect("unreachable");
                None
            }
        }
    }

    pub fn take_last_error(&mut self) -> Option<(TokenPosition, Error)> {
        self.last_error.take()
    }

    fn read_token(&mut self) -> Result<LexicalToken> {
        self.lexer.read_token()?.ok_or(Error::UnexpectedEof)
    }
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait ResumeParse<T>: Parse
where
    T: Region,
{
    fn resume_parse(lexer: &mut Lexer, parsed: T) -> Result<Self>;
}

pub trait Expect {
    type Token: Into<LexicalToken>;

    fn expect(&self, parser: &mut Parser) -> Result<Self::Token>;
}

impl Parse for AtomToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Atom(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                token,
                expected: "AtomToken",
            }),
        }
    }
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(&self, parser: &mut Parser) -> Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for SymbolToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Symbol(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                token,
                expected: "SymbolToken",
            }),
        }
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(&self, parser: &mut Parser) -> Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for VariableToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Variable(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                token,
                expected: "VariableToken",
            }),
        }
    }
}

impl Parse for StringToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::String(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                token,
                expected: "StringToken",
            }),
        }
    }
}
