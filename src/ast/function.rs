use crate::ast::ty::Type;
use crate::expect::{Either, ExpectAtom, ExpectNonNegInteger, ExpectVariable};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, IntegerToken, VariableToken};
use erl_tokenize::values::{Keyword, Symbol};

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
    Atom(AtomToken),
    Variable(VariableToken),
    Tuple(Box<Tuple<Self>>),
    List(Box<List<Self>>),
}

impl Parse for Pattern {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = Tuple::try_parse(lexer) {
            return Ok(Self::Tuple(Box::new(x)));
        }
        if let Some(x) = List::try_parse(lexer) {
            return Ok(Self::List(Box::new(x)));
        }
        if let Some(x) = lexer.try_read_expect(ExpectVariable) {
            return Ok(Self::Variable(x));
        }
        if let Some(x) = lexer.try_read_expect(ExpectAtom) {
            return Ok(Self::Atom(x));
        }
        Err(anyhow::anyhow!("next: {:?}", lexer.read_token()?).into())
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
    Try(Box<Try>),
    Match(Box<Match>),
}

impl Parse for Expr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if lexer.try_peek_expect(Keyword::Try).is_some() {
            return Parse::parse(lexer).map(|x| Self::Try(Box::new(x)));
        }
        if let Some(x) = Match::try_parse(lexer) {
            return Ok(Self::Match(Box::new(x)));
        }
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
pub enum List<T = Expr> {
    Proper { elements: Vec<T>, region: Region },
}

impl<T: Parse> Parse for List<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenSquare)?;
        let elements = parse_comma_delimited_items::<T>(lexer)?;
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

/// `try` `Body` `Option<TryOf>` `Option<TryCatch>` `Option<TryAfter>` `end`
#[derive(Debug, Clone)]
pub struct Try {
    body: Body,
    // branch: Option<TryOf>,
    catch: Option<TryCatch>,
    // after: Option<TryAfter>,
    region: Region,
}

impl Parse for Try {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Try)?;
        let body = Body::parse(lexer)?;
        let catch = TryCatch::try_parse(lexer);
        Ok(Self {
            body,
            catch,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct TryCatch {
    clauses: Vec<CatchClause>,
    region: Region,
}

impl Parse for TryCatch {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Catch)?;
        let clauses = parse_clauses(lexer)?;
        Ok(Self {
            clauses,
            region: lexer.region(start),
        })
    }
}

/// `Option<CatchClass>` `Pattern` `Option<CatchStacktrace>` `Option<WhenGuard>` `->` `Body`
#[derive(Debug, Clone)]
pub struct CatchClause {
    class: Option<CatchClass>,
    pattern: Pattern,
    body: Body,
    region: Region,
}

impl Parse for CatchClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let class = CatchClass::try_parse(lexer);
        let pattern = Pattern::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Body::parse(lexer)?;
        Ok(Self {
            class,
            pattern,
            body,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct CatchClass {
    class: Either<AtomToken, VariableToken>,
    region: Region,
}

impl Parse for CatchClass {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let class = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Colon)?;
        Ok(Self {
            class,
            region: lexer.region(start),
        })
    }
}

/// `Pattern` `=` `Expr`
#[derive(Debug, Clone)]
pub struct Match {
    pattern: Pattern,
    expr: Expr,
}

impl Parse for Match {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let pattern = Pattern::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Match)?;
        let expr = Expr::parse(lexer)?;
        Ok(Self { pattern, expr })
    }
}

#[derive(Debug, Clone)]
pub struct Tuple<T> {
    items: Vec<T>,
    region: Region,
}

impl<T: Parse> Parse for Tuple<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenBrace)?;
        let items = parse_comma_delimited_items::<T>(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseBrace)?;
        Ok(Self {
            items,
            region: lexer.region(start),
        })
    }
}

fn parse_comma_delimited_items<T: Parse>(lexer: &mut Lexer) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while let Some(item) = T::try_parse(lexer) {
        items.push(item);
        if lexer.try_read_expect(Symbol::Comma).is_none() {
            break;
        }
    }
    Ok(items)
}

fn parse_clauses<T: Parse>(lexer: &mut Lexer) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while let Some(item) = T::try_parse(lexer) {
        items.push(item);
        if lexer.try_read_expect(Symbol::Semicolon).is_none() {
            break;
        }
    }
    Ok(items)
}
