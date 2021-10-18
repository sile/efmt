use crate::ast::Ast;
use crate::Result;
use erl_tokenize::Lexer;

pub trait Parse: Sized {
    fn parse(parser: &mut Parser) -> Result<Self>;
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer<String>,
}

impl Parser {
    pub fn new(source_code: impl AsRef<str>) -> Self {
        Self {
            lexer: Lexer::new(source_code.as_ref().to_owned()),
        }
    }
}

impl Iterator for Parser {
    type Item = Result<Ast>;

    fn next(&mut self) -> Option<Self::Item> {
        todo!()
    }
}
