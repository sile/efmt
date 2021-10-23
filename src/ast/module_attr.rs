use crate::expect::ExpectAtom;
use crate::lexer::{Lexer, Region};
use crate::parser::Parse;
use crate::Result;
use erl_tokenize::tokens::AtomToken;
use erl_tokenize::values::Symbol;

/// `-` `module` `(` `AtomToken` `)` `.`
#[derive(Debug, Clone)]
pub struct ModuleAttr {
    module_name: AtomToken,
    pub region: Region,
}

impl ModuleAttr {
    pub fn module_name(&self) -> &str {
        self.module_name.value()
    }
}

impl Parse for ModuleAttr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("module")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let module_name = lexer.read_expect(ExpectAtom)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        let end = lexer.current_position();
        Ok(Self {
            module_name,
            region: Region::new(start, end),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_works() {
        let mut lexer = Lexer::new("-module(foo).");
        let module_attr = ModuleAttr::parse(&mut lexer).unwrap();
        assert_eq!(module_attr.module_name(), "foo");
    }
}
