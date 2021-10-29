use crate::cst::consts::{Comma, Question};
use crate::cst::primitives::{Items, Maybe, Parenthesized};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, AnyToken, Or, Parse, Parser, ResumeParse};
use crate::token::{
    AtomToken, Keyword, Region, Symbol, Token, TokenPosition, TokenRegion, VariableToken,
};
use efmt_derive::{Format, Parse, Region};
use std::io::Write;

#[derive(Debug, Clone, Region, Format)]
pub enum MacroName {
    Atom(AtomToken),
    Variable(VariableToken),
}

impl MacroName {
    pub fn get(&self) -> &str {
        match self {
            Self::Atom(x) => x.value(),
            Self::Variable(x) => x.value(),
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
            Err(parse::Error::unexpected_token(
                parser,
                token,
                "AtomToken or VariableToken",
            ))
        }
    }
}

#[derive(Debug, Clone)]
pub struct Replacement {
    tokens: Vec<Token>,
    start_position: TokenPosition,
}

impl Replacement {
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
}

impl Region for Replacement {
    fn region(&self) -> TokenRegion {
        if self.tokens.is_empty() {
            TokenRegion::new(self.start_position, self.start_position)
        } else {
            TokenRegion::new(
                self.tokens[0].region().start(),
                self.tokens[self.tokens.len() - 1].region().end(),
            )
        }
    }
}

impl Parse for Replacement {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start_position = parser.current_position();
        let mut tokens = Vec::new();
        while parser
            .peek_expect([Symbol::CloseParen, Symbol::Dot])
            .is_none()
        {
            let token = parser.read_token()?;
            match &token {
                Token::Symbol(x) if x.value() == Symbol::Dot => {
                    return Err(parse::Error::unexpected_token(
                        parser,
                        token.clone(),
                        "').'",
                    ));
                }
                _ => {}
            }
            tokens.push(token);
        }

        Ok(Self {
            tokens,
            start_position,
        })
    }
}

impl Format for Replacement {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        // TODO: try parse and format
        fmt.noformat(self)
    }
}

#[derive(Debug, Clone, Region, Parse, Format)]
pub struct MacroCall {
    question: Question,
    name: MacroName,
    args: Maybe<Parenthesized<Items<MacroArg, Comma>>>,
}

impl MacroCall {
    pub fn macro_name(&self) -> &str {
        self.name.get()
    }

    pub fn args(&self) -> Option<&[MacroArg]> {
        self.args.get().map(|x| x.get().items())
    }
}

impl ResumeParse<(Question, MacroName, Option<usize>)> for MacroCall {
    fn resume_parse(
        parser: &mut Parser,
        (question, name, _arity): (Question, MacroName, Option<usize>),
    ) -> parse::Result<Self> {
        // TODO: check arity
        Ok(Self {
            question,
            name,
            args: parser.parse()?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct MacroArg {
    tokens: Vec<Token>,
}

impl MacroArg {
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }
}

impl Region for MacroArg {
    fn region(&self) -> TokenRegion {
        if let (Some(first), Some(last)) = (self.tokens.first(), self.tokens.last()) {
            TokenRegion::new(first.region().start(), last.region().end())
        } else {
            unreachable!()
        }
    }
}

impl Parse for MacroArg {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        #[derive(Debug, Default, PartialEq, Eq)]
        struct Level {
            paren: usize,
            brace: usize,
            square: usize,
            bits: usize,
            block: usize,
        }

        impl Level {
            fn is_toplevel(&self) -> bool {
                *self == Self::default()
            }
        }

        let mut tokens = Vec::new();
        let mut level = Level::default();
        while tokens.is_empty()
            || !level.is_toplevel()
            || parser
                .peek_expect(Or(Symbol::Comma, Symbol::CloseParen))
                .is_none()
        {
            let token = parser.read_token()?;
            if parser.is_macro_expanded(&token) {
                tokens.push(token);
                continue;
            }

            match &token {
                Token::Symbol(x) => match x.value() {
                    Symbol::OpenParen => {
                        level.paren += 1;
                    }
                    Symbol::CloseParen => {
                        if level.paren == 0 {
                            todo!()
                        }
                        level.paren -= 1;
                    }
                    Symbol::OpenBrace => {
                        level.brace += 1;
                    }
                    Symbol::CloseBrace => {
                        if level.brace == 0 {
                            todo!();
                        }
                        level.brace -= 1;
                    }
                    Symbol::OpenSquare => {
                        level.square += 1;
                    }
                    Symbol::CloseSquare => {
                        if level.square == 0 {
                            todo!();
                        }
                        level.square -= 1;
                    }
                    Symbol::DoubleLeftAngle => {
                        level.bits += 1;
                    }
                    Symbol::DoubleRightAngle => {
                        if level.bits == 0 {
                            todo!();
                        }
                        level.bits -= 1;
                    }
                    _ => {}
                },
                Token::Keyword(x) => match x.value() {
                    Keyword::Begin | Keyword::Try | Keyword::Case | Keyword::If => {
                        level.block += 1;
                    }
                    Keyword::Fun => {
                        if parser.peek_expect(Symbol::OpenParen).is_some()
                            || parser.peek_expect((AnyToken, Symbol::OpenParen)).is_some()
                        {
                            level.block += 1;
                        }
                    }
                    Keyword::End => {
                        if level.block == 0 {
                            todo!();
                        }
                        level.block -= 1;
                    }
                    _ => {}
                },
                _ => {}
            }
            tokens.push(token);
        }

        Ok(Self { tokens })
    }
}

impl Format for MacroArg {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        // TODO: try parse and format
        fmt.noformat(self)
    }
}
