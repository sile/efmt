use crate::token::{
    AtomToken, LexicalToken, Region, Symbol, SymbolToken, TokenIndex, TokenRegion, VariableToken,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedTokenValue {
        index: TokenIndex,
        token: LexicalToken,
        expected: String,
    },

    #[error("expected {expected}, but got {token:?}")]
    UnexpectedToken {
        index: TokenIndex,
        token: LexicalToken,
        expected: &'static str,
    },

    #[error("invalid token region: start={start:?}, end={end:?}")]
    InvaildRegion { start: TokenIndex, end: TokenIndex },
}

impl Error {
    fn is_high_priority_than(&self, other: &Self) -> bool {
        fn token_index(e: &Error) -> TokenIndex {
            match e {
                Error::UnexpectedTokenValue { index, .. } => *index,
                Error::UnexpectedToken { index, .. } => *index,
                _ => unreachable!(),
            }
        }

        match (self, other) {
            (Self::InvaildRegion { .. }, _) => true, // This is a bug.
            (Self::UnexpectedEof, _) => true,
            (a, b) => token_index(a) >= token_index(b),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct TokenReader {
    tokens: Vec<LexicalToken>,
    current: TokenIndex,
    last_error: Option<Error>,
}

impl TokenReader {
    pub fn new(tokens: Vec<LexicalToken>) -> Self {
        Self {
            tokens,
            current: TokenIndex::new(0),
            last_error: None,
        }
    }

    pub fn with_transaction<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let index = self.current;
        match f(self) {
            Err(e) => {
                self.current = index;
                if self
                    .last_error
                    .as_ref()
                    .map_or(true, |last| e.is_high_priority_than(last))
                {
                    self.last_error = Some(e);
                }
                None
            }
            Ok(v) => Some(v),
        }
    }

    pub fn take_last_error(&mut self) -> Option<Error> {
        self.last_error.take()
    }

    pub fn read_token(&mut self) -> Result<LexicalToken> {
        if let Some(token) = self.tokens.get(self.current.get()).cloned() {
            self.current = TokenIndex::new(self.current.get() + 1);
            Ok(token)
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    pub fn current_index(&self) -> TokenIndex {
        self.current
    }

    pub fn region(&self, start: TokenIndex) -> Result<TokenRegion> {
        let end = self.current;
        TokenRegion::new(start, end).ok_or(Error::InvaildRegion { start, end })
    }
}

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenReader) -> Result<Self>;

    fn try_parse(tokens: &mut TokenReader) -> Option<Self> {
        tokens.with_transaction(|tokens| Self::parse(tokens))
    }

    fn parse_items(tokens: &mut TokenReader, delimiter: Symbol) -> Result<Vec<Self>> {
        let mut items = if let Some(item) = Self::try_parse(tokens) {
            vec![item]
        } else {
            return Ok(Vec::new());
        };
        while delimiter.try_expect(tokens).is_some() {
            let item = Self::parse(tokens)?;
            items.push(item);
        }
        Ok(items)
    }

    fn parse_non_empty_items(tokens: &mut TokenReader, delimiter: Symbol) -> Result<Vec<Self>> {
        let mut items = Vec::new();
        loop {
            let item = Self::parse(tokens)?;
            items.push(item);
            if delimiter.try_expect(tokens).is_none() {
                break;
            }
        }
        Ok(items)
    }
}

pub trait ResumeParse<T>: Parse
where
    T: Region,
{
    fn resume_parse(tokens: &mut TokenReader, parsed: T) -> Result<Self>;

    fn try_resume_parse(tokens: &mut TokenReader, parsed: T) -> Option<Self> {
        tokens.with_transaction(|tokens| Self::resume_parse(tokens, parsed))
    }
}

pub trait Expect {
    type Token: Into<LexicalToken>;

    fn expect(&self, tokens: &mut TokenReader) -> Result<Self::Token>;

    fn try_expect(&self, tokens: &mut TokenReader) -> Option<Self::Token> {
        tokens.with_transaction(|tokens| self.expect(tokens))
    }
}

impl Parse for AtomToken {
    fn parse(tokens: &mut TokenReader) -> Result<Self> {
        match tokens.read_token()? {
            LexicalToken::Atom(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(tokens.current_index().get() - 1),
                token,
                expected: "AtomToken",
            }),
        }
    }
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(&self, tokens: &mut TokenReader) -> Result<Self::Token> {
        let token = Self::Token::parse(tokens)?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                index: TokenIndex::new(tokens.current_index().get() - 1),
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for SymbolToken {
    fn parse(tokens: &mut TokenReader) -> Result<Self> {
        match tokens.read_token()? {
            LexicalToken::Symbol(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(tokens.current_index().get() - 1),
                token,
                expected: "SymbolToken",
            }),
        }
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(&self, tokens: &mut TokenReader) -> Result<Self::Token> {
        let token = Self::Token::parse(tokens)?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                index: TokenIndex::new(tokens.current_index().get() - 1),
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for VariableToken {
    fn parse(tokens: &mut TokenReader) -> Result<Self> {
        match tokens.read_token()? {
            LexicalToken::Variable(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(tokens.current_index().get() - 1),
                token,
                expected: "VariableToken",
            }),
        }
    }
}
