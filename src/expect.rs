use erl_tokenize::tokens::{AtomToken, IntegerToken, SymbolToken};
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

#[derive(Debug)]
pub struct ExpectNonNegInteger;

impl Expect for ExpectNonNegInteger {
    type Token = IntegerToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        match token {
            LexicalToken::Integer(token) if token.value() >= &0u32.into() => Ok(token),
            _ => Err(token),
        }
    }
}

#[derive(Debug)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

#[derive(Debug)]
pub struct Or<A, B>(pub A, pub B);

impl<A, B> Expect for Or<A, B>
where
    A: Expect,
    B: Expect,
{
    type Token = Either<A::Token, B::Token>;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        self.0
            .expect(token)
            .map(Either::A)
            .or_else(|token| self.1.expect(token).map(Either::B))
    }
}
