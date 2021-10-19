use crate::ast::Ast;
use crate::lexer::Lexer;
use crate::Result;

pub trait Parse: Sized {
    fn parse(lexer: &mut Lexer) -> Result<Self>;
}

#[derive(Debug)]
pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(source_code: impl AsRef<str>) -> Self {
        Self {
            lexer: Lexer::new(source_code),
        }
    }
}

impl Iterator for Parser {
    type Item = Result<Ast>;

    fn next(&mut self) -> Option<Self::Item> {
        // TODO: handle eof
        match Ast::parse(&mut self.lexer) {
            Err(e) => Some(Err(e)),
            Ok(a) => Some(Ok(a)),
        }
    }
}
