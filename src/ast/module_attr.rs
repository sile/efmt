use crate::lexer::{Lexer, Region};
use crate::parser::Parse;
use crate::Result;
use erl_tokenize::tokens::AtomToken;
use erl_tokenize::values::Symbol;

/// `-` `module` `(` `AtomToken` `)` `.`
#[derive(Debug, Clone)]
pub struct ModuleAttr {
    module_name: AtomToken,
    region: Region,
}

impl ModuleAttr {
    pub fn module_name(&self) -> &str {
        self.module_name.value()
    }
}

impl Parse for ModuleAttr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.expect_symbol_value(Symbol::Hyphen)?;
        let _ = lexer.expect_atom_value("module")?;
        let _ = lexer.expect_symbol_value(Symbol::OpenParen)?;
        let module_name = lexer.expect_atom()?;
        let _ = lexer.expect_symbol_value(Symbol::CloseParen)?;
        let _ = lexer.expect_symbol_value(Symbol::Dot)?;
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
