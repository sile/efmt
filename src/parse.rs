use crate::lex::{self, Lexer};
use crate::token::{AtomToken, Region, Symbol, Token, TokenIndex, TokenPosition, TokenRegion};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("expected {expected}, but got {token:?}\n\n{place}\n")]
    UnexpectedToken {
        index: TokenIndex,
        token: Token,
        expected: String,
        place: String,
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

    pub fn unexpected_token(parser: &Parser, token: Token, expected: &str) -> Self {
        let place = parser.generate_error_place(&token);
        Self::UnexpectedToken {
            index: parser.lexer.current_token_index(),
            token,
            expected: expected.to_owned(),
            place,
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

    fn generate_error_place(&self, unexpected_token: &Token) -> String {
        use std::fmt::Write;

        let line = unexpected_token.region().start().line();
        let column = unexpected_token.region().start().column();
        let file = self
            .lexer
            .filepath()
            .and_then(|x| x.to_str().map(|x| x.to_owned()))
            .unwrap_or_else(|| "<anonymous>".to_owned());
        let line_string = self.get_line_string(unexpected_token);

        let mut m = String::new();
        writeln!(&mut m, "--> {}:{}:{}", file, line, column).unwrap();
        writeln!(&mut m, "{} | {}", line, line_string).unwrap();
        writeln!(
            &mut m,
            "{:line_width$} | {:>token_column$} unexpected token",
            "",
            "^",
            line_width = line.to_string().len(),
            token_column = column
        )
        .unwrap();
        m
    }

    fn get_line_string(&self, token: &Token) -> &str {
        let text = self.lexer.text();
        let offset = token.region().start().offset();
        let line_start = (&text[..offset]).rfind("\n").unwrap_or(0);
        let line_end = (&text[offset..])
            .find('\n')
            .map(|x| x + offset)
            .unwrap_or_else(|| text.len());
        (&text[line_start..line_end]).trim_matches(char::is_control)
    }

    // pub fn save_checkpoint() // parser cannot rolback before the checkpoint (this could improve error message)

    pub fn is_eof(&mut self) -> Result<bool> {
        Ok(self.lexer.is_eof()?)
    }

    pub fn current_position(&self) -> TokenPosition {
        self.lexer.current_position()
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

    pub fn try_resume_parse<T, U>(&mut self, args: U) -> Option<T>
    where
        T: ResumeParse<U>,
    {
        let transaction = self.lexer.start_transaction();
        match self.resume_parse(args) {
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

    pub fn read_token(&mut self) -> Result<Token> {
        self.lexer.read_token()?.ok_or(Error::UnexpectedEof)
    }

    pub fn peek_token(&mut self) -> Result<Option<Token>> {
        let result = self.lexer.read_token()?;
        if result.is_some() {
            self.lexer.unread_token();
        }
        Ok(result)
    }

    pub fn is_macro_expanded(&self, token: &Token) -> bool {
        self.lexer.is_macro_expanded(token)
    }
}

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

pub trait ResumeParse<T>: Parse {
    fn resume_parse(lexer: &mut Parser, args: T) -> Result<Self>;
}

// TODO: remove
pub trait Expect {
    type Token; //TODO: rename (Item?)

    fn expect(self, parser: &mut Parser) -> Result<Self::Token>;
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

impl<T: Parse> Parse for Box<T> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse().map(Box::new)
    }
}

impl<T, U> ResumeParse<U> for Box<T>
where
    T: ResumeParse<U>,
{
    fn resume_parse(parser: &mut Parser, args: U) -> Result<Self> {
        parser.resume_parse(args).map(Box::new)
    }
}

#[derive(Debug, Clone)]
pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> From<Either<A, B>> for Token
where
    A: Into<Token>,
    B: Into<Token>,
{
    fn from(x: Either<A, B>) -> Self {
        match x {
            Either::A(x) => x.into(),
            Either::B(x) => x.into(),
        }
    }
}

impl<A, B> Parse for Either<A, B>
where
    A: Parse,
    B: Parse,
{
    fn parse(parser: &mut Parser) -> Result<Self> {
        if let Some(x) = parser.try_parse() {
            Ok(Self::A(x))
        } else {
            parser.parse().map(Self::B)
        }
    }
}

impl<A, B> Region for Either<A, B>
where
    A: Region,
    B: Region,
{
    fn region(&self) -> TokenRegion {
        match self {
            Self::A(x) => x.region(),
            Self::B(x) => x.region(),
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

impl<T, const N: usize> Expect for [T; N]
where
    T: Expect,
    Token: From<T::Token>,
{
    type Token = [Token; N];

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        let result = self
            .into_iter()
            .map(|x| parser.expect(x).map(|t| t.into()))
            .collect::<Result<Vec<_>>>()?
            .try_into();
        match result {
            Err(_) => unreachable!(),
            Ok(x) => Ok(x),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnyToken;

impl Expect for AnyToken {
    type Token = Token;

    fn expect(self, parser: &mut Parser) -> Result<Self::Token> {
        parser.read_token()
    }
}
