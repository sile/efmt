use crate::cst::primitives::{Atom, Variable};
use crate::format::{self, Format, Formatter};
use crate::parse::{self, AnyToken, Or, Parse, Parser, ResumeParse};
use crate::token::{Keyword, LexicalToken, Region, Symbol, TokenPosition, TokenRegion};
use std::io::Write;

#[derive(Debug, Clone)]
pub enum MacroName {
    Atom(Atom),
    Variable(Variable),
}

impl MacroName {
    pub fn get(&self) -> &str {
        match self {
            Self::Atom(x) => x.token().value(),
            Self::Variable(x) => x.token().value(),
        }
    }
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
    tokens: Vec<LexicalToken>,
    region: TokenRegion,
}

impl Replacement {
    pub fn tokens(&self) -> &[LexicalToken] {
        &self.tokens
    }
}

impl Region for Replacement {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for Replacement {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();
        let mut tokens = Vec::new();
        while parser
            .peek_expect((Symbol::CloseParen, Symbol::Dot))
            .is_none()
        {
            let token = parser.read_token()?;
            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Dot => {
                    return Err(parse::Error::UnexpectedToken {
                        token: token.clone(),
                        expected: "').'",
                    });
                }
                _ => {}
            }
            tokens.push(token);
        }

        Ok(Self {
            tokens,
            region: parser.region(start),
        })
    }
}

impl Format for Replacement {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        // TODO: try parse and format
        fmt.noformat(self)
    }
}

#[derive(Debug, Clone)]
pub struct MacroCall {
    name: MacroName,
    args: Option<Vec<MacroArg>>,
    region: TokenRegion,
}

impl MacroCall {
    pub fn macro_name(&self) -> &str {
        self.name.get()
    }

    pub fn args(&self) -> Option<&[MacroArg]> {
        self.args.as_ref().map(|x| x.as_slice())
    }
}

impl Region for MacroCall {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for MacroCall {
    fn parse(_parser: &mut Parser) -> parse::Result<Self> {
        todo!("don't call this method.");
    }
}

impl ResumeParse<(TokenPosition, MacroName, Option<usize>)> for MacroCall {
    fn resume_parse(
        parser: &mut Parser,
        (start, name, arity): (TokenPosition, MacroName, Option<usize>),
    ) -> parse::Result<Self> {
        let args = if let Some(arity) = arity {
            parser.expect(Symbol::OpenParen)?;
            let args = parser.parse_items(Symbol::Comma)?;
            parser.expect(Symbol::CloseParen)?;
            if args.len() != arity {
                todo!();
            }
            Some(args)
        } else {
            None
        };
        Ok(Self {
            name,
            args,
            region: parser.region(start),
        })
    }
}

impl Format for MacroCall {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        write!(fmt, "?")?;
        fmt.format(&self.name)?;
        if let Some(args) = &self.args {
            write!(fmt, "(")?;
            fmt.format_children(args, ",")?;
            write!(fmt, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MacroArg {
    tokens: Vec<LexicalToken>,
    region: TokenRegion,
}

impl MacroArg {
    pub fn tokens(&self) -> &[LexicalToken] {
        &self.tokens
    }
}

impl Region for MacroArg {
    fn region(&self) -> &TokenRegion {
        &self.region
    }
}

impl Parse for MacroArg {
    fn parse(parser: &mut Parser) -> parse::Result<Self> {
        let start = parser.current_position();

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
            match &token {
                LexicalToken::Symbol(x) => match x.value() {
                    Symbol::OpenParen => {
                        level.paren += 1;
                    }
                    Symbol::CloseParen => {
                        if level.paren == 0 {
                            todo!();
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
                LexicalToken::Keyword(x) => match x.value() {
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

        Ok(Self {
            tokens,
            region: parser.region(start),
        })
    }
}

impl Format for MacroArg {
    fn format<W: Write>(&self, fmt: &mut Formatter<W>) -> format::Result<()> {
        // TODO: try parse and format
        fmt.noformat(self)
    }
}
