use self::function::Expr;
use self::ty::Type;
use crate::expect::{Either, ExpectAtom, ExpectVariable, Or};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::values::Symbol;
use erl_tokenize::LexicalToken;

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
    DefineDirective(DefineDirective),
    WildDirective(WildDirective),
    FunSpec(self::function::FunSpec),
    FunDecl(self::function::FunDecl),
    RecordDecl(RecordDecl),
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
                (Symbol::Hyphen, "define") => {
                    return DefineDirective::parse(lexer).map(Self::DefineDirective)
                }
                (Symbol::Hyphen, "spec") => return Parse::parse(lexer).map(Self::FunSpec),
                (Symbol::Hyphen, "record") => return Parse::parse(lexer).map(Self::RecordDecl),
                (Symbol::Hyphen, _) => return Parse::parse(lexer).map(Self::WildDirective),
                _ => {
                    todo!("{:?}", tokens);
                }
            }
        }
        return Parse::parse(lexer).map(Self::FunDecl);
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

#[derive(Debug, Clone)]
pub struct WildDirective {
    name: AtomToken,
    value: self::function::Expr,
    region: Region,
}

impl Parse for WildDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let name = lexer.read_expect(ExpectAtom)?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let value = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            name,
            value,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct DefineDirective {
    name: Either<AtomToken, VariableToken>,
    args: Vec<VariableToken>,
    replacement: Vec<LexicalToken>,
    region: Region,
}

impl Parse for DefineDirective {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("define")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let name = lexer.read_expect(Or(ExpectAtom, ExpectVariable))?;

        let mut args = Vec::new();
        if lexer.try_read_expect(Symbol::OpenParen).is_some() {
            loop {
                let arg = lexer.read_expect(ExpectVariable)?;
                args.push(arg);
                if lexer.try_read_expect(Symbol::Comma).is_none() {
                    break;
                }
            }
            let _ = lexer.read_expect(Symbol::CloseParen)?;
        }
        let _ = lexer.read_expect(Symbol::Comma)?;

        let mut replacement = Vec::new();
        let mut level = 0;
        loop {
            let token = lexer.read_token()?;
            if token.as_symbol_token().map(|x| x.value()) == Some(Symbol::OpenParen) {
                level += 1;
            }
            if token.as_symbol_token().map(|x| x.value()) == Some(Symbol::CloseParen) {
                if level == 0 {
                    break;
                }
                level -= 1;
            }
            replacement.push(token);
        }
        let _ = lexer.read_expect(Symbol::Dot)?;

        Ok(Self {
            name,
            args,
            replacement,
            region: Region::new(start, lexer.current_position()),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordDecl {
    name: AtomToken,
    fields: Vec<RecordFieldDecl>,
    region: Region,
}

impl Parse for RecordDecl {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("record")?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let name = lexer.read_expect(ExpectAtom)?;
        let _ = lexer.read_expect(Symbol::Comma)?;
        let _ = lexer.read_expect(Symbol::OpenBrace)?;
        let fields = self::function::parse_comma_delimited_items(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseBrace)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            name,
            fields,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldDecl {
    name: AtomToken,
    default: Option<RecordFieldDefault>,
    ty: Option<RecordFieldType>,
    region: Region,
}

impl Parse for RecordFieldDecl {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Parse::parse(lexer)?;
        let default = Parse::try_parse(lexer);
        let ty = Parse::try_parse(lexer);
        Ok(Self {
            name,
            default,
            ty,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldDefault {
    value: Expr,
    region: Region,
}

impl Parse for RecordFieldDefault {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Match)?;
        let value = Expr::parse(lexer)?;
        Ok(Self {
            value,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldType {
    ty: Type,
    region: Region,
}

impl Parse for RecordFieldType {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::DoubleColon)?;
        let ty = Type::parse(lexer)?;
        Ok(Self {
            ty,
            region: lexer.region(start),
        })
    }
}
