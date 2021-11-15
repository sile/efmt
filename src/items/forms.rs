use crate::format::{self, Format};
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

#[derive(Debug, Clone, Span, Parse)]
pub struct SpecClause {
    params: Parenthesized<Items<Type>>,
    arrow: Space<RightArrowSymbol>,
    r#return: Type,
    constraint: Maybe<Constraint>,
}

impl Format for SpecClause {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.params.format(fmt)?;
        self.arrow.format(fmt)?;
        if fmt
            .subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .enter(|fmt| self.r#return.format(fmt))
            .is_err()
        {
            fmt.write_newline()?;
            if fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .indent_offset(1)
                .enter(|fmt| self.r#return.format(fmt))
                .is_err()
            {
                fmt.subregion()
                    .indent_offset(1)
                    .enter(|fmt| self.r#return.format(fmt))?;
            }
        }
        self.constraint.format(fmt)?;
        Ok(())
    }
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
    pub fn path(&self) -> &str {
        self.file.value()
    }

    pub fn get_include_path(&self, include_dirs: &[PathBuf]) -> Option<PathBuf> {
        let path = self.get_var_substituted_path();
        if matches!(self.include, Either::B(_)) && path.components().count() > 1 {
            let app_name = if let std::path::Component::Normal(name) = path.components().next()? {
                name.to_str()?
            } else {
                return None;
            };
            match crate::erl::erl_eval(&format!("code:lib_dir({})", app_name)) {
                Err(e) => {
                    log::warn!("{}", e);
                    None
                }
                Ok(base_dir) => {
                    let mut resolved_path = PathBuf::from(base_dir);
                    resolved_path.extend(path.components().skip(1));
                    log::debug!("Resolved include path: {:?}", resolved_path);
                    Some(resolved_path)
                }
            }
        } else if path.exists() {
            log::debug!("Resolved include path: {:?}", path);
            Some(path)
        } else {
            for dir in include_dirs {
                let candidate_path = dir.join(&path);
                if candidate_path.exists() {
                    log::debug!("Resolved include path: {:?}", candidate_path);
                    return Some(candidate_path);
                }
            }
            None
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

    #[test]
    fn define_directive_works() {
        let texts = [
            "-define(FOO, ).",
            "-define(bar, 1 + 2).",
            "-define(Baz(A, B), A), {B).",
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn include_directive_works() {
        let texts = [
            r#"-include("path/to/hrl")."#,
            r#"-include_lib("path/to/hrl")."#,
        ];
        for text in texts {
            crate::assert_format!(text, Form);
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
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn attr_works() {
        let texts = [
            "-export([foo/0]).",
            indoc::indoc! {"
            -export([foo/0,
                     bar/1])."},
            indoc::indoc! {"
            -dialyzer({[no_return,
                        no_match],
                       [g/0,
                        h/0]})."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn record_decl_works() {
        let texts = [
            "-record(foo, {}).",
            indoc::indoc! {"
            -record(rec, {field1 = [] :: Type1,
                          field2,
                          field3 = 42 :: Type3})."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn fun_decl_works() {
        let texts = [
            indoc::indoc! {"
            foo() ->
                bar."},
            indoc::indoc! {"
            foo(A, {B, [C]}) ->
                bar;
            foo(_, _) ->
                baz."},
            indoc::indoc! {"
            foo(A)
              when is_atom(A) ->
                bar,
                baz;
            foo(_) ->
                qux."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn fun_spec_works() {
        let texts = [
            indoc::indoc! {"
                -spec foo(X) -> X;
                         (Y) -> Y."},
            indoc::indoc! {"
                -spec foo(T1, T2) ->
                          T3;
                         (T4, T5) ->
                          T6."},
            "-spec id(X) -> X when X :: tuple().",
            indoc::indoc! {"
                -callback foo(atom()) ->
                              {atom(),
                               atom()}."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn type_decl_works() {
        let texts = [
            "-type height() :: pos_integer().",
            indoc::indoc! {"
            -opaque orddict(Key,
                            Val) :: [{Key,
                                      Val}]."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }
}
