use crate::expect::{ExpectAtom, ExpectNonNegInteger};
use crate::{Lexer, Parse, Region, Result};
use erl_tokenize::tokens::{AtomToken, IntegerToken};
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
