use crate::expect::{Either, ExpectAtom, ExpectVariable, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
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
    IfdefDirective(IfdefDirective),
    IfndefDirective(IfndefDirective),
    ElseDirective(ElseDirective),
    EndifDirective(EndifDirective),
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
                (Symbol::Hyphen, "type" | "opaque") => {
                    return self::type_decl::TypeDecl::parse(lexer).map(Self::TypeDecl);
                }
                (Symbol::Hyphen, "ifdef") => {
                    return IfdefDirective::parse(lexer).map(Self::IfdefDirective)
                }
                (Symbol::Hyphen, "ifndef") => {
                    return IfndefDirective::parse(lexer).map(Self::IfndefDirective)
                }
                (Symbol::Hyphen, "else") => {
                    return ElseDirective::parse(lexer).map(Self::ElseDirective)
                }
                (Symbol::Hyphen, "endif") => {
                    return EndifDirective::parse(lexer).map(Self::EndifDirective)
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

#[derive(Debug, Clone)]
pub enum MacroName {
    Atom(AtomToken),
    Variable(VariableToken),
}

impl Parse for MacroName {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match lexer.read_expect(Or(ExpectAtom, ExpectVariable))? {
            Either::A(x) => Ok(Self::Atom(x)),
            Either::B(x) => Ok(Self::Variable(x)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct IfdefDirective {
    macro_name: MacroName,
    region: Region,
}

impl Parse for IfdefDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("ifdef")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let macro_name = MacroName::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            macro_name,
            region: Region::new(start, lexer.current_position()),
        })
    }
}

#[derive(Debug, Clone)]
pub struct IfndefDirective {
    macro_name: MacroName,
    region: Region,
}

impl Parse for IfndefDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("ifndef")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let macro_name = MacroName::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            macro_name,
            region: Region::new(start, lexer.current_position()),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ElseDirective {
    region: Region,
}

impl Parse for ElseDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("else")?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            region: Region::new(start, lexer.current_position()),
        })
    }
}

#[derive(Debug, Clone)]
pub struct EndifDirective {
    region: Region,
}

impl Parse for EndifDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("endif")?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            region: Region::new(start, lexer.current_position()),
        })
    }
}
