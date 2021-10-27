use crate::lex::{self, Lexer};
use crate::token::{
    AtomToken, IntegerToken, LexicalToken, StringToken, Symbol, SymbolToken, TokenIndex,
    TokenPosition, TokenRegion, VariableToken,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedToken {
        index: TokenIndex,
        token: LexicalToken,
        expected: String,
    },

    #[error(transparent)]
    LexError(#[from] lex::Error),
}

impl Error {
    fn index(&self) -> TokenIndex {
        match self {
            Self::UnexpectedEof => TokenIndex::new(std::usize::MAX),
            Self::LexError(_) => TokenIndex::new(std::usize::MAX),
            Self::UnexpectedToken { index, .. } => *index,
        }
    }

    pub fn unexpected_token(parser: &Parser, token: LexicalToken, expected: &str) -> Self {
        Self::UnexpectedToken {
            index: parser.lexer.current_token_index(),
            token,
            expected: expected.to_owned(),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    last_error: Option<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Self {
            lexer,
            last_error: None,
        }
    }

    pub fn is_eof(&mut self) -> Result<bool> {
        Ok(self.lexer.is_eof()?)
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

    pub fn resume_parse<T, U>(&mut self, args: U) -> Result<T>
    where
        T: ResumeParse<U>,
    {
        T::resume_parse(self, args)
    }

    pub fn try_parse<T: Parse>(&mut self) -> Option<T> {
        let transaction = self.lexer.start_transaction();
        match self.parse() {
            Ok(x) => {
                self.lexer.commit(transaction).expect("unreachable");
                Some(x)
            }
            Err(e) => {
                if self
                    .last_error
                    .as_ref()
                    .map_or(true, |x| x.index() < e.index())
                {
                    self.last_error = Some(e);
                }
                self.lexer.rollback(transaction).expect("unreachable");
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
        let transaction = self.lexer.start_transaction();
        match self.expect(expected) {
            Ok(x) => {
                self.lexer.commit(transaction).expect("unreachable");
                Some(x)
            }
            Err(e) => {
                if self
                    .last_error
                    .as_ref()
                    .map_or(true, |x| x.index() < e.index())
                {
                    self.last_error = Some(e);
                }
                self.lexer.rollback(transaction).expect("unreachable");
                None
            }
        }
    }

    pub fn peek_expect<T: Expect>(&mut self, expected: T) -> Option<T::Token> {
        let transaction = self.lexer.start_transaction();
        let result = self.expect(expected).ok();
        self.lexer.rollback(transaction).expect("unreachable");
        result
    }

    pub fn take_last_error(&mut self) -> Option<Error> {
        self.last_error.take()
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
        self.lexer.read_token()?.ok_or(Error::UnexpectedEof)
    }
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait ResumeParse<T>: Parse {
    fn resume_parse(lexer: &mut Parser, args: T) -> Result<Self>;
}

pub trait Expect {
    type Token; //TODO: rename (Item?)

    fn expect(self, parser: &mut Parser) -> Result<Self::Token>;
}

impl Parse for AtomToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Atom(token) => Ok(token),
            token => Err(Error::unexpected_token(parser, token, "AtomToken")),
        }
    }
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == self {
            Ok(token)
        } else {
            Err(Error::unexpected_token(
                parser,
                token.into(),
                &format!("{:?}", self),
            ))
        }
    }
}

impl Parse for IntegerToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Integer(token) => Ok(token),
            token => Err(Error::unexpected_token(parser, token, "IntegerToken")),
        }
    }
}

impl Parse for SymbolToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Symbol(token) => Ok(token),
            token => Err(Error::unexpected_token(parser, token, "SymbolToken")),
        }
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        let token = parser.parse::<Self::Token>()?;
        if token.value() == self {
            Ok(token)
        } else {
            Err(Error::unexpected_token(
                parser,
                token.into(),
                &format!("{:?}", self),
            ))
        }
    }
}

impl Parse for VariableToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::Variable(token) => Ok(token),
            token => Err(Error::unexpected_token(parser, token, "VariableToken")),
        }
    }
}

impl Parse for StringToken {
    fn parse(parser: &mut Parser) -> Result<Self> {
        match parser.read_token()? {
            LexicalToken::String(token) => Ok(token),
            token => Err(Error::unexpected_token(parser, token, "StringToken")),
        }
    }
}

impl<T: Parse> Parse for Box<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse().map(Box::new)
    }
}

#[derive(Debug, Clone)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> From<Either<A, B>> for LexicalToken
where
    A: Into<LexicalToken>,
    B: Into<LexicalToken>,
{
    fn from(x: Either<A, B>) -> Self {
        match x {
            Either::A(x) => x.into(),
            Either::B(x) => x.into(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Or<A, B>(pub A, pub B);

impl<A, B> Expect for Or<A, B>
where
    A: Expect,
    B: Expect,
{
    type Token = Either<A::Token, B::Token>;

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        if let Some(x) = parser.try_expect(self.0) {
            Ok(Either::A(x))
        } else {
            // TODO: improve error message
            parser.expect(self.1).map(Either::B)
        }
    }
}

impl<A, B> Expect for (A, B)
where
    A: Expect,
    B: Expect,
{
    type Token = (A::Token, B::Token);

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        let a = parser.expect(self.0)?;
        let b = parser.expect(self.1)?;
        Ok((a, b))
    }
}

#[derive(Debug, Clone)]
pub struct AnyToken;

impl Expect for AnyToken {
    type Token = LexicalToken;

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        parser.read_token()
    }
}
