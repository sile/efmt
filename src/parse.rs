use crate::lex::{self, Lexer};
use crate::token::{
    AtomToken, LexicalToken, Region, StringToken, Symbol, SymbolToken, TokenIndex, VariableToken,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    // TODO: delete
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

    #[error(transparent)]
    LexError(#[from] lex::Error),
}

impl Error {
    fn is_high_priority_than(&self, other: &Self) -> bool {
        // TODO: rewrite
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
pub struct Parser {
    lexer: Lexer,
    last_error: Option<Error>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            lexer,
            last_error: None,
        }
    }
}

pub trait Parse: Sized {
    fn parse(lexer: &mut Lexer) -> Result<Self>;

    fn try_parse(lexer: &mut Lexer) -> Option<Self> {
        lexer.with_transaction(|lexer| Self::parse(lexer))
    }

    fn parse_items(lexer: &mut Lexer, delimiter: Symbol) -> Result<Vec<Self>> {
        let mut items = if let Some(item) = Self::try_parse(lexer) {
            vec![item]
        } else {
            return Ok(Vec::new());
        };
        while delimiter.try_expect(lexer).is_some() {
            let item = Self::parse(lexer)?;
            items.push(item);
        }
        Ok(items)
    }

    fn parse_non_empty_items(lexer: &mut Lexer, delimiter: Symbol) -> Result<Vec<Self>> {
        let mut items = Vec::new();
        loop {
            let item = Self::parse(lexer)?;
            items.push(item);
            if delimiter.try_expect(lexer).is_none() {
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
    fn resume_parse(lexer: &mut Lexer, parsed: T) -> Result<Self>;

    fn try_resume_parse(lexer: &mut Lexer, parsed: T) -> Option<Self> {
        lexer.with_transaction(|lexer| Self::resume_parse(lexer, parsed))
    }
}

pub trait Expect {
    type Token: Into<LexicalToken>;

    fn expect(&self, lexer: &mut Lexer) -> Result<Self::Token>;

    fn try_expect(&self, lexer: &mut Lexer) -> Option<Self::Token> {
        lexer.with_transaction(|lexer| self.expect(lexer))
    }
}

impl Parse for AtomToken {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_token()? {
            LexicalToken::Atom(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token,
                expected: "AtomToken",
            }),
        }
    }
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(&self, lexer: &mut Lexer) -> Result<Self::Token> {
        let token = Self::Token::parse(lexer)?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for SymbolToken {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_token()? {
            LexicalToken::Symbol(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token,
                expected: "SymbolToken",
            }),
        }
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(&self, lexer: &mut Lexer) -> Result<Self::Token> {
        let token = Self::Token::parse(lexer)?;
        if token.value() == *self {
            Ok(token)
        } else {
            Err(Error::UnexpectedTokenValue {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token: token.into(),
                expected: format!("{:?}", self),
            })
        }
    }
}

impl Parse for VariableToken {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_token()? {
            LexicalToken::Variable(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token,
                expected: "VariableToken",
            }),
        }
    }
}

impl Parse for StringToken {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_token()? {
            LexicalToken::String(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                index: TokenIndex::new(lexer.current_index().get() - 1),
                token,
                expected: "StringToken",
            }),
        }
    }
}
