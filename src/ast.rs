use crate::lexer::Lexer;
use crate::parser::Parse;
use crate::Result;
use erl_tokenize::values::Symbol;

pub mod module_attr;

#[derive(Debug, Clone)]
pub enum Ast {
    ModuleAttr(self::module_attr::ModuleAttr),
}

impl Parse for Ast {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let tokens = lexer.peek_tokens(2)?;
        if let (Some(token0), Some(token1)) =
            (tokens[0].as_symbol_token(), tokens[1].as_atom_token())
        {
            match (token0.value(), token1.value()) {
                (Symbol::Hyphen, "module") => {
                    return self::module_attr::ModuleAttr::parse(lexer).map(Self::ModuleAttr);
                }
                _ => {
                    todo!()
                }
            }
        } else {
        }

        todo!()
    }
}
