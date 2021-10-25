use crate::token::{AtomToken, LexicalToken, Region, TokenReader};

#[derive(Debug, thiserror::Error)]
pub enum Error {
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
    TokenError(#[from] crate::token::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(tokens: &mut TokenReader) -> Result<Self>;

    fn try_parse(tokens: &mut TokenReader) -> Option<Self> {
        tokens.with_transaction(|tokens| Self::parse(tokens)).ok()
    }
}

pub trait ResumeParse<T>: Parse
where
    T: Region,
{
    fn resume_parse(tokens: &mut TokenReader, parsed: T) -> Result<Self>;

    fn try_resume_parse(tokens: &mut TokenReader, parsed: T) -> Option<Self> {
        tokens
            .with_transaction(|tokens| Self::resume_parse(tokens, parsed))
            .ok()
    }
}

pub trait Expect {
    type Token: Into<LexicalToken>;

    fn expect(&self, tokens: &mut TokenReader) -> Result<Self::Token>;
}

impl Parse for AtomToken {
    fn parse(tokens: &mut TokenReader) -> Result<Self> {
        tokens.with_transaction(|tokens| match tokens.read_token()? {
            LexicalToken::Atom(token) => Ok(token),
            token => Err(Error::UnexpectedToken {
                token,
                expected: "AtomToken",
            }),
        })
    }
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(&self, tokens: &mut TokenReader) -> Result<Self::Token> {
        tokens.with_transaction(|tokens| {
            let token = Self::Token::parse(tokens)?;
            if token.value() == *self {
                Ok(token)
            } else {
                Err(Error::UnexpectedTokenValue {
                    token: token.into(),
                    expected: format!("{} (atom)", self),
                })
            }
        })
    }
}
