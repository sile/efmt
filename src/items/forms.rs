use crate::format::Format;
use crate::items::atoms::{DefineAtom, IncludeAtom, IncludeLibAtom};
use crate::items::expressions::functions::FunctionClause;
use crate::items::expressions::Expr;
use crate::items::generics::{Either, Items, Maybe, NonEmptyItems, Parenthesized};
use crate::items::keywords::IfKeyword;
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, HyphenSymbol, OpenParenSymbol, SemicolonSymbol,
};
use crate::items::tokens::{AtomToken, StringToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Form {
    Define(DefineDirective),
    Include(IncludeDirective),
    Attr(Attr),
    FunDecl(FunDecl),
    // RecordDecl
    // FunSpec
    // TypeDecl
    // CallbackSpec
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunDecl {
    clauses: NonEmptyItems<FunDeclClause, SemicolonSymbol>,
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
}
