use erl_tokenize::tokens::{
    AtomToken, IntegerToken, KeywordToken, StringToken, SymbolToken, VariableToken,
};
use erl_tokenize::values::{Keyword, Symbol};
use erl_tokenize::LexicalToken;

pub trait Expect: std::fmt::Debug {
    type Token: Into<LexicalToken>;

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

impl Expect for Keyword {
    type Token = KeywordToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        match token {
            LexicalToken::Keyword(token) if token.value() == *self => Ok(token),
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
pub struct ExpectSymbol;

impl Expect for ExpectSymbol {
    type Token = SymbolToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        if let LexicalToken::Symbol(token) = token {
            Ok(token)
        } else {
            Err(token)
        }
    }
}

#[derive(Debug)]
pub struct ExpectVariable;

impl Expect for ExpectVariable {
    type Token = VariableToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        if let LexicalToken::Variable(token) = token {
            Ok(token)
        } else {
            Err(token)
        }
    }
}

#[derive(Debug)]
pub struct ExpectString;

impl Expect for ExpectString {
    type Token = StringToken;

    fn expect(&self, token: LexicalToken) -> Result<Self::Token, LexicalToken> {
        if let LexicalToken::String(token) = token {
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

#[derive(Debug, Clone)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<T> Either<T, T> {
    pub fn into_token(self) -> T {
        match self {
            Self::A(x) => x,
            Self::B(x) => x,
        }
    }
}

impl<A, B> Either<A, B> {
    pub fn is_a(&self) -> bool {
        matches!(self, Either::A(_))
    }

    pub fn is_b(&self) -> bool {
        matches!(self, Either::B(_))
    }
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
