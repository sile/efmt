use crate::items::macros::Macro;
use crate::items::tokens::{CommentToken, Token};
use crate::lex::{self, Lexer};
use crate::span::{Position, Span as _};
use std::collections::BTreeMap;

pub use efmt_derive::Parse;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unexpected EOF")]
    UnexpectedEof,

    #[error("{message}")]
    UnexpectedToken { token: Token, message: String },

    #[error(transparent)]
    LexError(#[from] lex::Error),
}

impl Error {
    pub fn unexpected_token(parser: &mut Parser, token: Token) -> Self {
        let message = parser.generate_error_place(&token);
        Self::UnexpectedToken { token, message }
    }

    // TODO
    fn position(&self) -> Option<Position> {
        if let Self::UnexpectedToken { token, .. } = self {
            Some(token.start_position())
        } else {
            None
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

impl Parse for Token {
    fn parse(parser: &mut Parser) -> Result<Self> {
        let token = parser.lexer.read_token()?.ok_or(Error::UnexpectedEof)?;
        Ok(token)
    }
}

impl<A: Parse> Parse for Box<A> {
    fn parse(parser: &mut Parser) -> Result<Self> {
        parser.parse().map(Box::new)
    }
}

impl<A: Parse, B: Parse> Parse for (A, B) {
    fn parse(parser: &mut Parser) -> Result<Self> {
        Ok((parser.parse()?, parser.parse()?))
    }
}

// TODO: delete?
pub trait ResumeParse<T>: Parse {
    fn resume_parse(lexer: &mut Parser, args: T) -> Result<Self>;
}

impl<T, U> ResumeParse<U> for Box<T>
where
    T: ResumeParse<U>,
{
    fn resume_parse(parser: &mut Parser, args: U) -> Result<Self> {
        parser.resume_parse(args).map(Box::new)
    }
}

#[cfg(test)]
pub fn parse_text<T: Parse>(text: &str) -> anyhow::Result<T> {
    let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
    let mut lexer = Lexer::new(tokenizer);
    let mut parser = Parser::new(&mut lexer);
    let item = parser.parse()?;
    anyhow::ensure!(parser.is_eof()?, "there are unconsumed tokens");
    Ok(item)
}

// TODO: remove this class?
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

    pub fn is_eof(&mut self) -> Result<bool> {
        self.lexer.is_eof().map_err(Error::from)
    }

    pub fn text(&self) -> &str {
        self.lexer.text()
    }

    pub fn comments(&self) -> &BTreeMap<Position, CommentToken> {
        self.lexer.comments()
    }

    pub fn macros(&self) -> &BTreeMap<Position, Macro> {
        self.lexer.macros()
    }

    pub fn prev_token_end_position(&mut self) -> Result<Position> {
        self.lexer.prev_token_end_position().map_err(Error::from)
    }

    pub fn next_token_start_position(&mut self) -> Result<Position> {
        self.lexer.next_token_start_position().map_err(Error::from)
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

    fn try_do<F, T>(&mut self, f: F) -> Option<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        let transaction = self.lexer.start_transaction();
        match f(self) {
            Ok(x) => {
                self.lexer.commit(transaction).expect("unreachable");
                Some(x)
            }
            Err(e) => {
                if self
                    .last_error
                    .as_ref()
                    .map_or(true, |x| x.position() < e.position())
                {
                    self.last_error = Some(e);
                }
                self.lexer.rollback(transaction).expect("unreachable");
                None
            }
        }
    }

    pub fn try_parse<T: Parse>(&mut self) -> Option<T> {
        self.try_do(|this| this.parse())
    }

    pub fn try_resume_parse<T, U>(&mut self, args: U) -> Option<T>
    where
        T: ResumeParse<U>,
    {
        self.try_do(|this| this.resume_parse(args))
    }

    pub fn peek<T: Parse>(&mut self) -> bool {
        let transaction = self.lexer.start_transaction();
        let ok = self.parse::<T>().is_ok();
        self.lexer.rollback(transaction).expect("unreachable");
        ok
    }

    pub fn take_last_error(&mut self) -> Option<Error> {
        self.last_error.take()
    }

    fn generate_error_place(&self, unexpected_token: &Token) -> String {
        use std::fmt::Write;

        let line = unexpected_token.start_position().line();
        let column = unexpected_token.start_position().column();
        let file = self
            .lexer
            .filepath()
            .and_then(|x| x.to_str().map(|x| x.to_owned()))
            .unwrap_or_else(|| "<anonymous>".to_owned());
        let line_string = self.get_line_string(unexpected_token);

        let mut m = String::new();
        writeln!(&mut m).unwrap();
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
        let offset = token.start_position().offset();
        let line_start = (&text[..offset]).rfind('\n').unwrap_or(0);
        let line_end = (&text[offset..])
            .find('\n')
            .map(|x| x + offset)
            .unwrap_or_else(|| text.len());
        (&text[line_start..line_end]).trim_matches(char::is_control)
    }
}
