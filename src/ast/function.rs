use crate::ast::ty::Type;
use crate::expect::{Either, ExpectAtom, ExpectVariable, Or};
use crate::{Lexer, Parse, Region, Result, ResumeParse};
use erl_tokenize::tokens::{AtomToken, CharToken, IntegerToken, StringToken, VariableToken};
use erl_tokenize::values::{Keyword, Symbol};
use erl_tokenize::LexicalToken;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NameAndArity<Name = AtomToken, Arity = IntegerToken> {
    name: Name,
    arity: Arity,
    region: Region,
}

impl<Name, Arity> Parse for NameAndArity<Name, Arity>
where
    Name: Parse,
    Arity: Parse,
{
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Name::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Slash)?;
        let arity = Arity::parse(lexer)?;
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

#[derive(Debug, Clone)]
pub struct DirectiveFunSpec {
    name: AtomToken,
    arity: IntegerToken,
    region: Region,
}

impl Parse for DirectiveFunSpec {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Slash)?;
        let arity = Parse::parse(lexer)?;
        Ok(Self {
            name,
            arity,
            region: lexer.region(start),
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
    guard: Option<CaseGuard>,
    body: Body,
    region: Region,
}

impl Parse for FunDeclClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = lexer.read_expect(ExpectAtom)?;
        let args = Vec::<Pattern>::parse(lexer)?;
        let guard = Parse::try_parse(lexer);
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Body::parse(lexer)?;
        Ok(Self {
            name,
            args,
            guard,
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
    Integer(IntegerToken),
    Char(CharToken),
    String(StringToken),
    Tuple(Box<Tuple<Self>>),
    List(Box<List<Self>>),
    Match(Box<Match>),
    Record(Box<RecordPattern>),
    Bits(Box<Bits<Self>>),
    BinaryOp(Box<BinaryOp<Self>>),
}

impl Parse for Pattern {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let pattern = if let Some(x) = Tuple::try_parse(lexer) {
            Self::Tuple(Box::new(x))
        } else if let Some(x) = List::try_parse(lexer) {
            Self::List(Box::new(x))
        } else if let Some(x) = RecordPattern::try_parse(lexer) {
            Self::Record(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::Bits(Box::new(x))
        } else if let Some(_) = Call::<IdLikeExpr, Expr>::try_parse(lexer) {
            // TODO
            return Err(anyhow::anyhow!("next: {:?}", lexer.read_token()?).into());
        } else if let Some(x) = lexer.try_read_expect(ExpectVariable) {
            Self::Variable(x)
        } else if let Some(x) = lexer.try_read_expect(ExpectAtom) {
            Self::Atom(x)
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::Integer(x)
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::Char(x)
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::String(x)
        } else {
            return Err(anyhow::anyhow!("next: {:?}", lexer.read_token()?).into());
        };

        if let Some(x) = ResumeParse::try_resume_parse(lexer, start, pattern.clone()) {
            return Ok(Self::BinaryOp(Box::new(x)));
        }

        let result = lexer.with_transaction(|lexer| {
            let _ = lexer.read_expect(Symbol::Match)?;
            Expr::parse(lexer) // TODO
        });
        if let Ok(right) = result {
            Ok(Self::Match(Box::new(Match {
                pattern,
                expr: right,
            })))
        } else {
            Ok(pattern)
        }
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
    Fun(Box<Fun>),
    DirectiveFunSpec(DirectiveFunSpec),
    Atom(AtomToken),
    Variable(VariableToken),
    Integer(IntegerToken),
    Char(CharToken),
    String(StringToken),
    List(Box<List>),
    Tuple(Box<Tuple>),
    Try(Box<Try>),
    Match(Box<Match>),
    Bits(Box<Bits>),
    Case(Box<Case>),
    If(Box<If>),
    BinaryOp(Box<BinaryOp>),
    UnaryOp(Box<UnaryOp>),
    RecordPattern(Box<RecordPattern>), // TODO
    Parenthesized(Box<Parenthesized<Self>>),
    RecordFieldAccess(Box<RecordFieldAccess>),
    RecordFieldUpdate(Box<RecordFieldUpdate>),
}

impl Parse for Expr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let expr = if lexer.try_peek_expect(Keyword::Try).is_some() {
            Parse::parse(lexer).map(|x| Self::Try(Box::new(x)))?
        } else if let Some(x) = Match::try_parse(lexer) {
            Self::Match(Box::new(x))
        } else if let Some(x) = Parenthesized::try_parse(lexer) {
            Self::Parenthesized(Box::new(x))
        } else if let Some(x) = Case::try_parse(lexer) {
            Self::Case(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::If(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::RecordFieldAccess(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::RecordPattern(Box::new(x))
        } else if let Some(x) = Call::try_parse(lexer) {
            Self::FunCall(Box::new(x))
        } else if let Some(x) = Fun::try_parse(lexer) {
            Self::Fun(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::DirectiveFunSpec(x)
        } else if let Some(x) = List::try_parse(lexer) {
            Self::List(Box::new(x))
        } else if let Some(x) = Tuple::try_parse(lexer) {
            Self::Tuple(Box::new(x))
        } else if let Some(x) = Bits::try_parse(lexer) {
            Self::Bits(Box::new(x))
        } else if let Some(x) = Parse::try_parse(lexer) {
            Self::UnaryOp(Box::new(x))
        } else if let Some(x) = AtomToken::try_parse(lexer) {
            Self::Atom(x)
        } else if let Some(x) = VariableToken::try_parse(lexer) {
            Self::Variable(x)
        } else if let Some(x) = StringToken::try_parse(lexer) {
            Self::String(x)
        } else if let Some(x) = IntegerToken::try_parse(lexer) {
            Self::Integer(x)
        } else if let Some(x) = CharToken::try_parse(lexer) {
            Self::Char(x)
        } else {
            return Err(anyhow::anyhow!(
                "[{}] not yet implemented: next={:?}",
                line!(),
                lexer.read_token()?
            )
            .into());
        };
        if let Some(x) = ResumeParse::try_resume_parse(lexer, start, expr.clone()) {
            return Ok(Self::BinaryOp(Box::new(x)));
        } else if let Some(x) = ResumeParse::try_resume_parse(lexer, start, expr.clone()) {
            return Ok(Self::RecordFieldUpdate(Box::new(x)));
        }

        Ok(expr)
    }
}

#[derive(Debug, Clone)]
pub struct ProperList<T> {
    elements: Vec<T>,
    region: Region,
}

impl<T: Parse> Parse for ProperList<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenSquare)?;
        let elements = parse_comma_delimited_items::<T>(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseSquare)?;
        Ok(Self {
            elements,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct ImproperList<T> {
    elements: Vec<T>,
    last: T,
    region: Region,
}

impl<T: Parse> Parse for ImproperList<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenSquare)?;
        let mut elements = Vec::new();
        loop {
            elements.push(T::parse(lexer)?);
            if lexer
                .read_expect(Or(Symbol::Comma, Symbol::VerticalBar))?
                .is_b()
            {
                let last = T::parse(lexer)?;
                let _ = lexer.read_expect(Symbol::CloseSquare)?;
                return Ok(Self {
                    elements,
                    last,
                    region: lexer.region(start),
                });
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum List<T = Expr> {
    Proper(ProperList<T>),
    Improper(ImproperList<T>),
}

impl<T: Parse> Parse for List<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        match Either::<ProperList<T>, ImproperList<T>>::parse(lexer)? {
            Either::A(x) => Ok(Self::Proper(x)),
            Either::B(x) => Ok(Self::Improper(x)),
        }
    }
}

#[derive(Debug, Clone)]
pub enum IdLikeExpr {
    Atom(AtomToken),
    Variable(VariableToken),
    Parenthesized(Expr),
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
            let expr = Expr::parse(lexer)?;
            lexer.read_expect(Symbol::CloseParen)?;
            return Ok(Self::Parenthesized(expr));
        }
        Err(anyhow::anyhow!("unsupported expression").into())
    }
}

#[derive(Debug, Clone)]
pub enum IntegerLikeExpr {
    Integer(IntegerToken),
    Variable(VariableToken),
    // TODO: Parenthesized
}

impl Parse for IntegerLikeExpr {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = IntegerToken::try_parse(lexer) {
            return Ok(Self::Integer(x));
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
    branch: Option<TryOf>,
    catch: Option<TryCatch>,
    // after: Option<TryAfter>,
    region: Region,
}

impl Parse for Try {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Try)?;
        let body = Body::parse(lexer)?;
        let branch = Parse::try_parse(lexer);
        let catch = TryCatch::try_parse(lexer);
        let _ = lexer.read_expect(Keyword::End)?;
        Ok(Self {
            body,
            branch,
            catch,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct TryOf {
    clauses: Vec<CaseClause>,
    region: Region,
}

impl Parse for TryOf {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Of)?;
        let clauses = parse_clauses(lexer)?;
        Ok(Self {
            clauses,
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
    stacktrace: Option<CatchStacktrace>,
    body: Body,
    region: Region,
}

impl Parse for CatchClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let class = CatchClass::try_parse(lexer);
        let pattern = Pattern::parse(lexer)?;
        let stacktrace = CatchStacktrace::try_parse(lexer);
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Body::parse(lexer)?;
        Ok(Self {
            class,
            pattern,
            stacktrace,
            body,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct CatchStacktrace {
    var: VariableToken,
    region: Region,
}

impl Parse for CatchStacktrace {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Colon)?;
        let var = VariableToken::parse(lexer)?;
        Ok(Self {
            var,
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
        if let Pattern::Match(x) = pattern {
            Ok(Self {
                pattern: x.pattern.clone(),
                expr: x.expr.clone(),
            })
        } else {
            let _ = lexer.read_expect(Symbol::Match)?;
            let expr = Expr::parse(lexer)?;
            Ok(Self { pattern, expr })
        }
    }
}

#[derive(Debug, Clone)]
pub struct Tuple<T = Expr> {
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

pub fn parse_comma_delimited_items<T: Parse>(lexer: &mut Lexer) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while let Some(item) = T::try_parse(lexer) {
        items.push(item);
        if lexer.try_read_expect(Symbol::Comma).is_none() {
            break;
        }
    }
    Ok(items)
}

pub fn parse_items<T: Parse>(lexer: &mut Lexer, delimiter: Symbol) -> Result<Vec<T>> {
    let mut items = Vec::new();
    while let Some(item) = T::try_parse(lexer) {
        items.push(item);
        if lexer.try_read_expect(delimiter).is_none() {
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

#[derive(Debug, Clone)]
pub enum Fun {
    Defined(DefinedFun),
    Anonymous(AnonymousFun),
}

impl Parse for Fun {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        if let Some(x) = Parse::try_parse(lexer) {
            Ok(Self::Defined(x))
        } else {
            Ok(Self::Anonymous(Parse::parse(lexer)?))
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnonymousFun {
    args: Vec<Pattern>,
    body: Body,
    region: Region,
}

impl Parse for AnonymousFun {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Fun)?;
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let args = parse_comma_delimited_items(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Keyword::End)?;
        Ok(Self {
            args,
            body,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct DefinedFun {
    module: Option<ModulePrefix<Either<AtomToken, VariableToken>>>,
    name_and_arity:
        NameAndArity<Either<AtomToken, VariableToken>, Either<IntegerToken, VariableToken>>,
    region: Region,
}

impl Parse for DefinedFun {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Fun)?;
        let module = ModulePrefix::try_parse(lexer);
        let name_and_arity = Parse::parse(lexer)?;
        Ok(Self {
            module,
            name_and_arity,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Bits<T = Expr> {
    elementes: Vec<BitsElement<T>>,
    region: Region,
}

impl<T: Parse> Parse for Bits<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::DoubleLeftAngle)?;
        let elementes = parse_comma_delimited_items(lexer)?;
        let _ = lexer.read_expect(Symbol::DoubleRightAngle)?;
        Ok(Self {
            elementes,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct BitsElement<T> {
    value: T,
    size: Option<BitsElementSize>,
    specs: Option<BitsElementSpecs>,
    region: Region,
}

impl<T: Parse> Parse for BitsElement<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let value = Parse::parse(lexer)?;
        let size = Parse::try_parse(lexer);
        let specs = Parse::try_parse(lexer);
        Ok(Self {
            value,
            size,
            specs,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct BitsElementSize {
    size: IntegerLikeExpr,
    region: Region,
}

impl Parse for BitsElementSize {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Colon)?;
        let size = Parse::parse(lexer)?;
        Ok(Self {
            size,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct BitsElementSpecs {
    specs: Vec<AtomToken>, // TODO
    region: Region,
}

impl Parse for BitsElementSpecs {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Slash)?;
        let specs = parse_items(lexer, Symbol::Hyphen)?;
        Ok(Self {
            specs,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    value: Expr,
    clauses: Vec<CaseClause>,
    region: Region,
}

impl Parse for Case {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::Case)?;
        let value = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Keyword::Of)?;
        let clauses = parse_clauses(lexer)?;
        let _ = lexer.read_expect(Keyword::End)?;
        Ok(Self {
            value,
            clauses,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pattern: Pattern,
    guard: Option<CaseGuard>,
    body: Body,
    region: Region,
}

impl Parse for CaseClause {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let pattern = Parse::parse(lexer)?;
        let guard = Parse::try_parse(lexer);
        let _ = lexer.read_expect(Symbol::RightArrow)?;
        let body = Parse::parse(lexer)?;
        Ok(Self {
            pattern,
            guard,
            body,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub enum GuardCondition {
    Or(Expr),
    And(Expr),
    Last(Expr),
}

#[derive(Debug, Clone)]
pub struct CaseGuard {
    exprs: Vec<GuardCondition>,
    region: Region,
}

impl Parse for CaseGuard {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::When)?;
        let mut exprs = Vec::new();
        loop {
            let expr = Expr::parse(lexer)?;
            if lexer.try_read_expect(Symbol::Semicolon).is_some() {
                exprs.push(GuardCondition::Or(expr));
            } else if lexer.try_read_expect(Symbol::Comma).is_some() {
                exprs.push(GuardCondition::And(expr));
            } else {
                exprs.push(GuardCondition::Last(expr));
                break;
            }
        }
        Ok(Self {
            exprs,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordPattern {
    name: AtomToken,
    fields: Vec<RecordFieldPattern>,
    region: Region,
}

impl Parse for RecordPattern {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::Sharp)?;
        let name = AtomToken::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::OpenBrace)?;
        let fields = parse_items(lexer, Symbol::Comma)?;
        let _ = lexer.read_expect(Symbol::CloseBrace)?;
        Ok(Self {
            name,
            fields,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldPattern {
    name: AtomToken,
    value: Pattern,
    region: Region,
}

impl Parse for RecordFieldPattern {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Match)?;
        let value = Parse::parse(lexer)?;
        Ok(Self {
            name,
            value,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct Parenthesized<T> {
    item: T,
    region: Region,
}

impl<T: Parse> Parse for Parenthesized<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Symbol::OpenParen)?;
        let item = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseParen)?;
        Ok(Self {
            item,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldAccess {
    value: Expr,
    record_name: AtomToken,
    field_name: AtomToken,
    region: Region,
}

impl Parse for RecordFieldAccess {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        lexer.check_visited(start, "record-field-access", 0)?;
        let value = Parse::parse(lexer)?;
        lexer.clear_visited(start, "record-field-access", 0);
        let _ = lexer.read_expect(Symbol::Sharp)?;
        let record_name = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Dot)?;
        let field_name = Parse::parse(lexer)?;
        Ok(Self {
            value,
            record_name,
            field_name,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct BinaryOp<T = Expr> {
    left: T,
    op: LexicalToken, // TODO
    right: Expr,
}

impl<T: Parse> Parse for BinaryOp<T> {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        lexer.check_visited(start, "binary-op", 0)?;
        let left = Parse::parse(lexer)?;
        lexer.clear_visited(start, "binary-op", 0);
        let op = lexer
            .read_expect(Or(Keyword::Andalso, Or(Keyword::Orelse, Symbol::GreaterEq)))?
            .into();
        let right = Parse::parse(lexer)?;
        Ok(Self { left, op, right })
    }
}

impl<T: Parse> ResumeParse<T> for BinaryOp<T> {
    fn resume_parse(lexer: &mut Lexer, _start: crate::lexer::Position, left: T) -> Result<Self> {
        let op = lexer
            .read_expect(Or(
                Or(
                    Or(Or(Keyword::Andalso, Symbol::Multiply), Symbol::Greater),
                    Or(Or(Keyword::Orelse, Symbol::Slash), Symbol::GreaterEq),
                ),
                Or(
                    Or(Or(Symbol::Hyphen, Symbol::Plus), Keyword::Div),
                    Or(
                        Or(Keyword::Rem, Symbol::ExactEq),
                        Or(
                            Or(Symbol::ExactNotEq, Keyword::Band),
                            Or(Or(Symbol::Less, Symbol::LessEq), Keyword::Bsr),
                        ),
                    ),
                ),
            ))?
            .into();
        let right = Parse::parse(lexer)?;
        Ok(Self { left, op, right })
    }
}

#[derive(Debug, Clone)]
pub struct UnaryOp {
    op: LexicalToken,
    value: Expr,
    region: Region,
}

impl Parse for UnaryOp {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let op = lexer
            .read_expect(Or(Symbol::Hyphen, Or(Symbol::Plus, Keyword::Not)))?
            .into();
        let value = Parse::parse(lexer)?;
        Ok(Self {
            op,
            value,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct If {
    clauses: Vec<CaseClause>,
    region: Region,
}

impl Parse for If {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let _ = lexer.read_expect(Keyword::If)?;
        let clauses = parse_clauses(lexer)?;
        let _ = lexer.read_expect(Keyword::End)?;
        Ok(Self {
            clauses,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct RecordFieldUpdate {
    value: Expr,
    name: AtomToken,
    fields: Vec<UpdateField>,
    region: Region,
}

impl Parse for RecordFieldUpdate {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let value = Parse::parse(lexer)?;
        Self::resume_parse(lexer, start, value)
    }
}

impl ResumeParse<Expr> for RecordFieldUpdate {
    fn resume_parse(lexer: &mut Lexer, start: crate::lexer::Position, value: Expr) -> Result<Self> {
        let _ = lexer.read_expect(Symbol::Sharp);
        let name = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::OpenBrace)?;
        let fields = parse_comma_delimited_items(lexer)?;
        let _ = lexer.read_expect(Symbol::CloseBrace)?;
        Ok(Self {
            value,
            name,
            fields,
            region: lexer.region(start),
        })
    }
}

#[derive(Debug, Clone)]
pub struct UpdateField {
    name: AtomToken,
    value: Expr,
    region: Region,
}

impl Parse for UpdateField {
    fn parse(lexer: &mut Lexer) -> Result<Self> {
        let start = lexer.current_position();
        let name = Parse::parse(lexer)?;
        let _ = lexer.read_expect(Symbol::Match)?;
        let value = Parse::parse(lexer)?;
        Ok(Self {
            name,
            value,
            region: lexer.region(start),
        })
    }
}
