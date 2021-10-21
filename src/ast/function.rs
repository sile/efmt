use crate::ast::ty::Type;
use crate::expect::{ExpectAtom, ExpectNonNegInteger, ExpectVariable};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, IntegerToken, VariableToken};
use erl_tokenize::values::Symbol;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameAndArity<Name = AtomToken, Arity = IntegerToken> {
    name: Name,
    arity: Arity,
    region: Region,
}

impl Parse for NameAndArity<AtomToken, IntegerToken> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = lexer.read_expect(ExpectAtom)?;
        let _ = lexer.read_expect(Symbol::Slash)?;
        let arity = lexer.read_expect(ExpectNonNegInteger)?;
        let end = lexer.current_position();
        Ok(Self {
            name,
            arity,
            region: Region::new(start, end),
        })
    }
}

/// `Name` `:`
#[derive(Debug, Clone)]
pub struct ModulePrefix<Name> {
    name: Name,
    region: Region,
}

impl<Name> Parse for ModulePrefix<Name>
where
    Name: Parse,
{
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Name::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Colon)?;
        let end = lexer.current_position();
        Ok(Self {
            name,
            region: Region::new(start, end),
        })
    }
}

/// `(` `Arg` `,` ... `)`
#[derive(Debug, Clone)]
pub struct Args<Arg> {
    args: Vec<Arg>,
    region: Region,
}

impl<Arg> Parse for Args<Arg>
where
    Arg: Parse,
{
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let mut args = Vec::new();
        while let Some(arg) = Arg::try_parse(lexer) {
            args.push(arg);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let end = lexer.current_position();
        Ok(Self {
            args,
            region: Region::new(start, end),
        })
    }
}

/// `Option<ModulePrefix<T>>` `T` `Args<A>`
#[derive(Debug, Clone)]
pub struct Call<Name, Arg> {
    module: Option<ModulePrefix<Name>>,
    name: Name,
    args: Args<Arg>,
    region: Region,
}

impl<Name, Arg> Parse for Call<Name, Arg>
where
    Name: Parse,
    Arg: Parse,
{
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let module = ModulePrefix::try_parse(lexer);
        let name = Name::parse(lexer)?;
        let args = Args::parse(lexer)?;
        let end = lexer.current_position();
        Ok(Self {
            module,
            name,
            args,
            region: Region::new(start, end),
        })
    }
}

/// `-` `spec` `Option<ModulePrefix>` `AtomToken` `Clauses<SpecClause>` `.`
#[derive(Debug, Clone)]
pub struct FunSpec {
    module: Option<ModulePrefix<AtomToken>>,
    name: AtomToken,
    clauses: Vec<SpecClause>,
    region: Region,
}

impl Parse for FunSpec {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Hyphen)?;
        let _ = lexer.read_expect("spec")?;
        let module = ModulePrefix::try_parse(lexer);
        let name = AtomToken::parse(lexer)?;

        let mut clauses = Vec::new();
        loop {
            let clause = SpecClause::parse(lexer)?;
            clauses.push(clause);
            if lexer.try_read_expect(Symbol::Semicolon).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            module,
            name,
            clauses,
            region: lexer.region(start),
        })
    }
}

/// `Args<Type>` `->` `Type` `Option<Constraints>`
#[derive(Debug, Clone)]
pub struct SpecClause {
    args: Vec<Type>,
    ret: Type,
    region: Region,
}

impl Parse for SpecClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();

        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let mut args = Vec::new();
        while let Some(arg) = Type::try_parse(lexer) {
            args.push(arg);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::CloseParen)?;

        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let ret = Type::parse(lexer)?;

        if lexer.try_read_expect("when").is_some() {
            todo!()
        }

        Ok(Self {
            args,
            ret,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct FunDecl {
    clauses: Vec<FunDeclClause>,
    region: Region,
}

impl Parse for FunDecl {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let mut clauses = Vec::new();
        loop {
            let clause = FunDeclClause::parse(lexer)?;
            clauses.push(clause);
            if lexer.try_read_expect(Symbol::Semicolon).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::Dot)?;
        Ok(Self {
            clauses,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct FunDeclClause {
    name: AtomToken,
    args: Vec<Pattern>,
    // Optional<WhenGuard>
    body: Body,
    region: Region,
}

impl Parse for FunDeclClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = lexer.read_expect(ExpectAtom)?;
        let args = Vec::<Pattern>::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Body::parse(lexer)?;
        Ok(Self {
            name,
            args,
            body,
            region: lexer.region(start),
        })
    }
}

impl Parse for Vec<Pattern> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let mut args = Vec::new();
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        while let Some(arg) = Pattern::try_parse(lexer) {
            args.push(arg);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        Ok(args)
    }
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(VariableToken),
}

impl Parse for Pattern {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = lexer.try_read_expect(ExpectVariable) {
            return Ok(Self::Variable(x));
        }
        todo!("next: {:?}", lexer.read_token()?);
    }
}

#[derive(Debug, Clone)]
pub struct Body {
    expressions: Vec<Expr>,
    region: Region,
}

impl Parse for Body {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let mut expressions = Vec::new();
        loop {
            let expr = Expr::parse(lexer)?;
            expressions.push(expr);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        Ok(Self {
            expressions,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    FunCall(Box<Call<IdLikeExpr, Expr>>),
    Atom(AtomToken),
    Variable(VariableToken),
    List(Box<List>),
}

impl Parse for Expr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = Call::try_parse(lexer) {
            return Ok(Self::FunCall(Box::new(x)));
        }
        if let Some(x) = List::try_parse(lexer) {
            return Ok(Self::List(Box::new(x)));
        }
        if let Some(x) = AtomToken::try_parse(lexer) {
            return Ok(Self::Atom(x));
        }
        if let Some(x) = VariableToken::try_parse(lexer) {
            return Ok(Self::Variable(x));
        }
        Err(anyhow::anyhow!("not yet implemented: next={:?}", lexer.read_token()?).into())
    }
}

#[derive(Debug, Clone)]
pub enum List {
    Proper { elements: Vec<Expr>, region: Region },
}

impl Parse for List {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenSquare)?;
        let mut elements = Vec::new();
        while let Some(element) = Expr::try_parse(lexer) {
            elements.push(element);
            if lexer.try_read_expect(Symbol::Comma).is_none() {
                break;
            }
        }
        let _ = lexer.read_expect(Symbol::CloseSquare)?;
        Ok(Self::Proper {
            elements,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub enum IdLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    // TODO: Parenthesized
}

impl Parse for IdLikeExpr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = AtomToken::try_parse(lexer) {
            return Ok(Self::Atom(x));
        }
        if let Some(x) = VariableToken::try_parse(lexer) {
            return Ok(Self::Variable(x));
        }
        if lexer.try_read_expect(Symbol::OpenParen).is_some() {
            todo!();
        }
        Err(anyhow::anyhow!("unsupported expression").into())
    }
}
