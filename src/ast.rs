use crate::lexer::Lexer;
use crate::parser::Parse;
use crate::Result;
use erl_tokenize::values::Symbol;

pub mod export_attr;
pub mod export_type_attr;
pub mod function;
pub mod module_attr;
pub mod ty;
pub mod type_decl;

#[derive(Debug, Clone)]
pub enum Ast {
    ModuleAttr(self::module_attr::ModuleAttr),
    ExportAttr(self::export_attr::ExportAttr),
    ExportTypeAttr(self::export_type_attr::ExportTypeAttr),
    TypeDecl(self::type_decl::TypeDecl),
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
                (Symbol::Hyphen, "export") => {
                    return self::export_attr::ExportAttr::parse(lexer).map(Self::ExportAttr);
                }
                (Symbol::Hyphen, "export_type") => {
                    return self::export_type_attr::ExportTypeAttr::parse(lexer)
                        .map(Self::ExportTypeAttr);
                }
                (Symbol::Hyphen, "type") => {
                    return self::type_decl::TypeDecl::parse(lexer).map(Self::TypeDecl);
                }
                _ => {
                    todo!("{:?}", tokens);
                }
            }
        } else {
        }

        todo!("{:?}", tokens);
    }
}
