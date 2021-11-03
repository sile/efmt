use crate::format::Format;
use crate::items::atoms::{
    CallbackAtom, DefineAtom, IncludeAtom, IncludeLibAtom, OpaqueAtom, RecordAtom, SpecAtom,
    TypeAtom,
};
use crate::items::expressions::functions::FunctionClause;
use crate::items::expressions::Expr;
use crate::items::generics::{Clauses, Either, Items, Maybe, NonEmptyItems, Parenthesized};
use crate::items::keywords::{IfKeyword, WhenKeyword};
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CommaSymbol, DotSymbol, DoubleColonSymbol, HyphenSymbol,
    MatchSymbol, OpenBraceSymbol, OpenParenSymbol, RightArrowSymbol, SemicolonSymbol,
};
use crate::items::tokens::{AtomToken, StringToken, Token, VariableToken};
use crate::items::types::Type;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Form {
    Define(DefineDirective),
    Include(IncludeDirective),
    FunSpec(FunSpec),
    FunDecl(FunDecl),
    TypeDecl(TypeDecl),
    RecordDecl(RecordDecl),
    Attr(Attr),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordDecl {
    hyphen: HyphenSymbol,
    record: RecordAtom,
    open: OpenParenSymbol,
    name: AtomToken,
    comma: CommaSymbol,
    field_start: OpenBraceSymbol,
    fields: Items<RecordField, CommaSymbol>,
    field_end: CloseBraceSymbol,
    close: CloseParenSymbol,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordField {
    name: AtomToken,
    default: Maybe<(MatchSymbol, Expr)>,
    r#type: Maybe<(DoubleColonSymbol, Type)>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TypeDecl {
    hyphen: HyphenSymbol,
    kind: Either<TypeAtom, OpaqueAtom>,
    name: AtomToken,
    params: Parenthesized<Items<VariableToken, CommaSymbol>>,
    delimiter: DoubleColonSymbol,
    r#type: Type,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunSpec {
    hyphen: HyphenSymbol,
    kind: Either<SpecAtom, CallbackAtom>,
    function_name: AtomToken,
    clauses: NonEmptyItems<SpecClause, SemicolonSymbol>,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct SpecClause {
    params: Parenthesized<Items<Type, CommaSymbol>>,
    arrow: RightArrowSymbol,
    r#return: Type,
    constraint: Maybe<Constraint>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Constraint {
    when: WhenKeyword,
    constraints: NonEmptyItems<(VariableToken, (DoubleColonSymbol, Type)), CommaSymbol>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunDecl {
    clauses: Clauses<FunDeclClause>,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunDeclClause {
    name: AtomToken,
    clause: FunctionClause,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Attr {
    hyphen: HyphenSymbol,
    name: Either<AtomToken, IfKeyword>,
    items: Maybe<Parenthesized<Items<Expr, CommaSymbol>>>,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct DefineDirective {
    hyphen: HyphenSymbol,
    define: DefineAtom,
    open: OpenParenSymbol,
    macro_name: MacroName,
    variables: Maybe<Parenthesized<Items<VariableToken, CommaSymbol>>>,
    comma: CommaSymbol,
    replacement: MacroReplacement,
    close: CloseParenSymbol,
    dot: DotSymbol,
}

impl DefineDirective {
    pub fn macro_name(&self) -> &str {
        self.macro_name.value()
    }

    pub fn variables(&self) -> Option<&[VariableToken]> {
        self.variables.get().map(|x| x.get().get())
    }

    pub fn replacement(&self) -> &[Token] {
        self.replacement.tokens()
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct IncludeDirective {
    hyphen: HyphenSymbol,
    include: Either<IncludeAtom, IncludeLibAtom>,
    open: OpenParenSymbol,
    file: StringToken,
    close: CloseParenSymbol,
    dot: DotSymbol,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse_text;

    #[test]
    fn define_directive_works() {
        let texts = [
            "-define(FOO,).",
            "-define(bar, 1 + 2).",
            "-define(Baz(A, B), A), {B).",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Define(_)));
        }
    }

    #[test]
    fn include_directive_works() {
        let texts = [
            r#"-include("path/to/hrl")."#,
            r#"-include_lib("path/to/hrl")."#,
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Include(_)));
        }
    }

    #[test]
    fn other_directive_works() {
        let texts = [
            "-ifdef(foo).",
            "-ifndef(foo).",
            "-undef(foo).",
            "-else.",
            "-endif.",
            "-if(true).",
            "-elif(true).",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Attr(_)));
        }
    }

    #[test]
    fn attr_works() {
        let texts = ["-export([foo/0, bar/1])."];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Attr(_)));
        }
    }

    #[test]
    fn record_decl_works() {
        let texts = [
            "-record(foo, {}).",
            "-record(rec, {field1 = [] :: Type1, field2, field3 = 42 :: Type3}).",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::RecordDecl(_)));
        }
    }

    #[test]
    fn fun_decl_works() {
        let texts = [
            "foo() -> bar.",
            "foo(A, {B, [C]}) -> bar; foo(_, _) -> baz.",
            "foo(A) when is_atom(A)-> bar, baz; foo(_) -> qux.",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::FunDecl(_)));
        }
    }

    #[test]
    fn fun_spec_works() {
        let texts = [
            "-spec foo(T1, T2) -> T3; (T4, T5) -> T6.",
            "-spec id(X) -> X when X :: tuple().",
            "-callback foo(atom()) -> {atom(), atom()}.",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::FunSpec(_)));
        }
    }

    #[test]
    fn type_decl_works() {
        let texts = [
            "-type height() :: pos_integer().",
            "-opaque orddict(Key, Val) :: [{Key, Val}].",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::TypeDecl(_)));
        }
    }
}
