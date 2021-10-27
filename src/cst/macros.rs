use crate::cst::expressions::Expr;
use crate::cst::primitives::{Atom, Either, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, Parse, Parser};
use crate::token::{LexicalToken, Region, Symbol, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum MacroName {
    Atom(Atom),
    Variable(Variable),
}

impl Region for MacroName {
    fn region(&self) -> &TokenRegion {
        match self {
            Self::Atom(x) => x.region(),
            Self::Variable(x) => x.region(),
        }
    }
}

impl Parse for MacroName {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::Atom(x))
        } else if let Some(x) = parser.try_parse() {
            Ok(Self::Variable(x))
        } else {
            let token = parser.read_token()?;
            Err(parse::Error::UnexpectedToken {
                token,
                expected: "AtomToken or VariableToken",
            })
        }
    }
}

impl Format for MacroName {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match self {
            Self::Atom(x) => x.format(fmt),
            Self::Variable(x) => x.format(fmt),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Replacement {
    replacement: Either<Expr, Vec<LexicalToken>>,
    region: TokenRegion,
}

impl Region for Replacement {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Replacement {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let mut replacement = None;
        if let Some(expr) = parser.try_parse::<Expr>() {
            if parser.try_expect(Symbol::CloseParen).is_some()
                && parser.try_expect(Symbol::Dot).is_some()
            {
                parser.set_position(expr.region().end())?;
                replacement = Some(Either::A(expr));
            } else {
                parser.set_position(&start)?;
            }
        }
        if replacement.is_none() {
            let mut tokens = Vec::new();
            let mut last_position = parser.current_position();
            loop {
                let token = parser.read_token()?;
                match &token {
                    LexicalToken::Symbol(x) if x.value() == Symbol::CloseParen => {
                        if parser.try_expect(Symbol::Dot).is_some() {
                            parser.set_position(&last_position)?;
                            break;
                        }
                    }
                    LexicalToken::Symbol(x) if x.value() == Symbol::Dot => {
                        return Err(parse::Error::UnexpectedToken {
                            token: token.clone(),
                            expected: "').'",
                        });
                    }
                    _ => {}
                }
                tokens.push(token);
                last_position = parser.current_position();
            }
            replacement = Some(Either::B(tokens));
        }

        Ok(Self {
            replacement: replacement.expect("unreachable"),
            region: parser.region(start),
        })
    }
}

impl Format for Replacement {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        match &self.replacement {
            Either::A(x) => fmt.format(x),
            Either::B(_) => fmt.noformat(self),
        }
    }
}
