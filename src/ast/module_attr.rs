use crate::parser::{Parse, Parser};
use crate::Result;
use erl_tokenize::tokens::{AtomToken, SymbolToken};

/// `-` `module` `(` `AtomToken` `)` `.`
#[derive(Debug, Clone)]
pub struct ModuleAttr {
    pub _hyphen: SymbolToken,
    pub _module: AtomToken,
    pub _open: SymbolToken,
    pub module_name: AtomToken,
    pub _close: SymbolToken,
    pub _dot: SymbolToken,
}

impl Parse for ModuleAttr {
    fn parse(parser: &mut Parser) -> Result<Self> {
        todo!()
    }
}
