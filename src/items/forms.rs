use crate::format::Format;
use crate::items::atoms::{
    CallbackAtom, DefineAtom, IncludeAtom, IncludeLibAtom, OpaqueAtom, RecordAtom, SpecAtom,
    TypeAtom,
};
use crate::items::expressions::functions::FunctionClause;
use crate::items::expressions::Expr;
use crate::items::generics::{
    Clauses, Either, Elements, Items, Maybe, NonEmptyItems, Parenthesized,
};
use crate::items::keywords::{IfKeyword, WhenKeyword};
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::styles::{ColumnIndent, RightSpace, Space};
use crate::items::symbols::{
    CloseBraceSymbol, CloseParenSymbol, CommaSymbol, DotSymbol, DoubleColonSymbol, HyphenSymbol,
    MatchSymbol, OpenBraceSymbol, OpenParenSymbol, RightArrowSymbol,
};
use crate::items::tokens::{AtomToken, StringToken, Token, VariableToken};
use crate::items::types::Type;
use crate::parse::Parse;
use crate::span::Span;
use std::path::{Path, PathBuf};

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
    comma: RightSpace<CommaSymbol>,
    field_start: OpenBraceSymbol,
    fields: Elements<RecordField>,
    field_end: CloseBraceSymbol,
    close: CloseParenSymbol,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordField {
    name: AtomToken,
    default: Maybe<(Space<MatchSymbol>, Expr)>,
    r#type: Maybe<(Space<DoubleColonSymbol>, Type)>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TypeDecl {
    hyphen: HyphenSymbol,
    kind: RightSpace<Either<TypeAtom, OpaqueAtom>>,
    name: AtomToken,
    params: Parenthesized<Items<VariableToken>>,
    delimiter: Space<DoubleColonSymbol>,
    r#type: ColumnIndent<Type>,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunSpec {
    hyphen: HyphenSymbol,
    kind: RightSpace<Either<SpecAtom, CallbackAtom>>,
    function_name: AtomToken,
    clauses: Clauses<SpecClause>,
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct SpecClause {
    params: Parenthesized<Items<Type>>,
    arrow: Space<RightArrowSymbol>,
    r#return: Type,
    constraint: Maybe<Constraint>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Constraint {
    when: Space<WhenKeyword>,
    constraints: NonEmptyItems<(VariableToken, (Space<DoubleColonSymbol>, Type)), CommaSymbol>,
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
    comma: RightSpace<CommaSymbol>,
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

impl IncludeDirective {
    pub fn get_include_path<P: AsRef<Path>>(&self, code_paths: &[P]) -> Option<PathBuf> {
        let path = self.get_var_substituted_path();
        if matches!(self.include, Either::B(_)) {
            let app_name = if let std::path::Component::Normal(name) = path.components().next()? {
                name.to_str()?
            } else {
                return None;
            };
            let prefix = format!("{}-", app_name);
            for code_path in code_paths {
                let entry = std::fs::read_dir(code_path)
                    .ok()
                    .into_iter()
                    .flatten()
                    .filter_map(|x| x.ok())
                    .filter(|x| x.path().is_dir())
                    .filter(|x| x.path().starts_with(&prefix))
                    .next();
                if let Some(root) = entry {
                    let mut target_path = root.path().to_path_buf();
                    target_path.extend(path.components().skip(1));
                    return Some(target_path);
                }
            }
            None
        } else {
            Some(path)
        }
    }

    fn get_var_substituted_path(&self) -> PathBuf {
        let path_str = self.file.value();
        let path: &Path = path_str.as_ref();
        if !path_str.starts_with('$') {
            return path.to_path_buf();
        }

        let mut expanded_path = PathBuf::new();
        for (i, c) in path.components().enumerate() {
            if i == 0 {
                if let Some(expanded) = c
                    .as_os_str()
                    .to_str()
                    .and_then(|name| std::env::var(name.split_at(1).1).ok())
                {
                    expanded_path.push(expanded);
                    continue;
                }
            }
            expanded_path.push(c);
        }
        expanded_path
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::items::styles::Child;
    use crate::parse::parse_text;
    use crate::FormatOptions;

    fn format(text: &str) -> String {
        FormatOptions::<Child<Form>>::new()
            .max_columns(20)
            .format_text(text)
            .expect("parse or format failed")
    }

    #[test]
    fn define_directive_works() {
        let texts = [
            "-define(FOO, ).",
            "-define(bar, 1 + 2).",
            "-define(Baz(A, B), A), {B).",
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Define(_)));
            assert_eq!(format(text), text);
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
            assert_eq!(format(text), text);
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
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn attr_works() {
        let texts = [
            "-export([foo/0]).",
            concat!(
                "-export([foo/0,\n", //
                "         bar/1])."
            ),
            concat!(
                "-dialyzer({[no_return,\n",
                "            no_match],\n",
                "           [g/0,\n",
                "            h/0]})."
            ),
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::Attr(_)));
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn record_decl_works() {
        let texts = [
            "-record(foo, {}).",
            concat!(
                "-record(rec, {field1 = [] :: Type1,\n",
                "              field2,\n",
                "              field3 = 42 :: Type3})."
            ),
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::RecordDecl(_)));
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn fun_decl_works() {
        let texts = [
            concat!(
                "foo() ->\n", //
                "    bar."
            ),
            concat!(
                "foo(A, {B, [C]}) ->\n",
                "    bar;\n",
                "foo(_, _) ->\n",
                "    baz."
            ),
            concat!(
                "foo(A) when is_atom(A) ->\n",
                "    bar,\n",
                "    baz;\n",
                "foo(_) ->\n",
                "    qux."
            ),
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::FunDecl(_)));
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn fun_spec_works() {
        let texts = [
            concat!(
                "-spec foo(T1, T2) -> T3;\n", //
                "         (T4, T5) -> T6."
            ),
            "-spec id(X) -> X when X :: tuple().",
            concat!(
                "-callback foo(atom()) -> {atom(),\n",
                "                          atom()}."
            ),
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::FunSpec(_)));
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn type_decl_works() {
        let texts = [
            "-type height() :: pos_integer().",
            concat!(
                "-opaque orddict(Key,\n",
                "                Val) :: [{Key,\n",
                "                          Val}]."
            ),
        ];
        for text in texts {
            assert!(matches!(parse_text(text).unwrap(), Form::TypeDecl(_)));
            assert_eq!(format(text), text);
        }
    }
}
