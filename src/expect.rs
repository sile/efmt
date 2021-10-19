use erl_tokenize::tokens::{AtomToken, SymbolToken};
use erl_tokenize::values::Symbol;
use erl_tokenize::LexicalToken;

pub trait Expect: std::fmt::Debug {
    type Token;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken>;
}

impl Expect for &str {
    type Token = AtomToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        ExpectAtom.expect(token).and_then(|token| {
            if token.value() == *self {
                Ok(token)
            } else {
                Err(token.into())
            }
        })
    }
}

impl Expect for Symbol {
    type Token = SymbolToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        match token {
            LexicalToken::Symbol(token) if token.value() == *self => Ok(token),
            _ => Err(token),
        }
    }
}

#[derive(Debug)]
pub struct ExpectAtom;

impl Expect for ExpectAtom {
    type Token = AtomToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        if let LexicalToken::Atom(token) = token {
            Ok(token)
        } else {
            Err(token)
        }
    }
}
