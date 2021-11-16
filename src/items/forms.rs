use crate::format::{self, Format};
use crate::items::atoms::{
    CallbackAtom, DefineAtom, IncludeAtom, IncludeLibAtom, OpaqueAtom, RecordAtom, SpecAtom,
    TypeAtom,
};
use crate::items::expressions::functions::FunctionClause;
use crate::items::expressions::Expr;
use crate::items::generics::{Args, Clauses2, Either, Maybe, NonEmptyItems, Params, Tuple};
use crate::items::keywords::{IfKeyword, WhenKeyword};
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::styles::{RightSpace, Space, TrailingColumns};
use crate::items::symbols::{
    CloseParenSymbol, CommaSymbol, DotSymbol, DoubleColonSymbol, HyphenSymbol, MatchSymbol,
    OpenParenSymbol, RightArrowSymbol,
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

/// `-` `record` `(` `$NAME` `,` `{` `$FIELD`* `}` `)` `.`
///
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken] (`=` [Expr])? (`::` [Type])? `,`?
#[derive(Debug, Clone, Span, Parse)]
pub struct RecordDecl {
    hyphen: HyphenSymbol,
    record: RecordAtom,
    open: OpenParenSymbol,
    name: AtomToken,
    comma: RightSpace<CommaSymbol>,
    fields: Tuple<RecordField, 3>, // "})."
    close: CloseParenSymbol,
    dot: DotSymbol,
}

impl Format for RecordDecl {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.hyphen.format(fmt)?;
        self.record.format(fmt)?;
        self.open.format(fmt)?;
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.name.format(fmt)?;
            self.comma.format(fmt)?;
            if fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .enter(|fmt| self.fields.format(fmt))
                .is_err()
            {
                fmt.write_newline()?;
                self.fields.format(fmt)?;
            }
            Ok(())
        })?;
        self.close.format(fmt)?;
        self.dot.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct RecordField {
    name: AtomToken,
    default: Maybe<(Space<MatchSymbol>, Expr)>,
    r#type: Maybe<(Space<DoubleColonSymbol>, Type)>,
}

/// `-` (`type` | `opaque`) `$NAME` `(` (`$PARAM` `,`?)* `)` `::` `$TYPE` `.`
///
/// - $NAME: [AtomToken]
/// - $PARAM: [VariableToken]
/// - $TYPE: [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct TypeDecl {
    hyphen: HyphenSymbol,
    kind: RightSpace<Either<TypeAtom, OpaqueAtom>>,
    name: AtomToken,
    params: TrailingColumns<Params<VariableToken>, 3>, // " ::"
    delimiter: Space<DoubleColonSymbol>,
    r#type: TrailingColumns<Type, 1>, // "."
    dot: DotSymbol,
}

impl Format for TypeDecl {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.hyphen.format(fmt)?;
        self.kind.format(fmt)?;
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.name.format(fmt)?;
            self.params.format(fmt)?;
            self.delimiter.format(fmt)?;
            if fmt
                .subregion()
                .current_column_as_indent()
                .forbid_too_long_line()
                .enter(|fmt| self.r#type.format(fmt))
                .is_err()
            {
                fmt.subregion()
                    .indent_offset(2)
                    .enter_with_newline(|fmt| self.r#type.format(fmt))?;
            }
            Ok(())
        })?;
        self.dot.format(fmt)?;
        Ok(())
    }
}

/// `-` (`spec` | `callback`) `$NAME` (`(` (`$PARAM` `,`?)* `)` `->` `$RETURN` `;`?)+ `.`
///
/// - $NAME: [AtomToken]
/// - $PARAM: [Type]
/// - $RETURN: [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct FunSpec {
    hyphen: HyphenSymbol,
    kind: RightSpace<Either<SpecAtom, CallbackAtom>>,
    function_name: AtomToken,
    clauses: Clauses2<SpecClause, 1>, // trailing: "."
    dot: DotSymbol,
}

impl Format for FunSpec {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.hyphen.format(fmt)?;
        self.kind.format(fmt)?;
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.function_name.format(fmt)?;
            self.clauses.format(fmt)
        })?;
        self.dot.format(fmt)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct SpecClause {
    params: TrailingColumns<Params<Type>, 3>, // trailing: " ->"
    arrow: Space<RightArrowSymbol>,
    r#return: Type,
    constraint: Maybe<Constraint>,
}

impl Format for SpecClause {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion()
            .clear_trailing_columns(true)
            .enter(|fmt| self.params.format(fmt))?;
        self.arrow.format(fmt)?;

        if fmt
            .subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .check_trailing_columns(self.constraint.is_none())
            .enter(|fmt| self.r#return.format(fmt))
            .is_err()
        {
            fmt.write_newline()?;
            fmt.subregion()
                .parent_indent()
                .indent_offset(4)
                .enter(|fmt| self.r#return.format(fmt))?;
        }

        if fmt
            .subregion()
            .forbid_multi_line()
            .forbid_too_long_line()
            .check_trailing_columns(!self.constraint.is_none())
            .enter(|fmt| self.constraint.format(fmt))
            .is_err()
        {
            fmt.write_newline()?;
            fmt.subregion()
                .parent_indent()
                .indent_offset(8)
                .enter(|fmt| self.constraint.format(fmt))?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct Constraint {
    when: Space<WhenKeyword>,
    constraints: NonEmptyItems<(VariableToken, (Space<DoubleColonSymbol>, Type)), CommaSymbol>,
}

/// (`$NAME` `(` (`$PARAM` `,`?)* `)` (`when` `$GUARD`)? `->` `$BODY` `;`?)+ `.`
///
/// - $NAME: [AtomToken]
/// - $PARAM: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunDecl {
    clauses: Clauses2<FunDeclClause, 1>, // trailing: "."
    dot: DotSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct FunDeclClause {
    name: AtomToken,
    clause: FunctionClause,
}

/// `-` `$NAME` `$ARGS`? `.`
///
/// - $NAME: [AtomToken] | `if`
/// - $ARGS: `(` (`$ARG` `,`?)* `)`
/// - $ARG: [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Attr {
    hyphen: HyphenSymbol,
    name: Either<AtomToken, IfKeyword>,
    items: Maybe<TrailingColumns<Args<Expr>, 1>>, // "."
    dot: DotSymbol,
}

/// `-` `define` `(` `$NAME` `$VARS`? `,` `REPLACEMENT`* `)` `.`
///
/// - $NAME: [AtomToken] | [VariableToken]
/// - $VARS: `(` ([VariableToken] `,`?)* `)`
/// - $REPLACEMENT: [Token]
#[derive(Debug, Clone, Span, Parse)]
pub struct DefineDirective {
    hyphen: HyphenSymbol,
    define: DefineAtom,
    open: OpenParenSymbol,
    macro_name: MacroName,
    variables: Maybe<Params<VariableToken>>,
    comma: RightSpace<CommaSymbol>,
    replacement: TrailingColumns<MacroReplacement, 2>, // ")."
    close: CloseParenSymbol,
    dot: DotSymbol,
}

impl DefineDirective {
    pub fn macro_name(&self) -> &str {
        self.macro_name.value()
    }

    pub fn variables(&self) -> Option<&[VariableToken]> {
        self.variables.get().map(|x| x.get())
    }

    pub fn replacement(&self) -> &[Token] {
        self.replacement.get().tokens()
    }
}

impl Format for DefineDirective {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.hyphen.format(fmt)?;
        self.define.format(fmt)?;
        self.open.format(fmt)?;
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            self.macro_name.format(fmt)?;
            self.variables.format(fmt)?;
            self.comma.format(fmt)?;
            if fmt
                .subregion()
                .forbid_multi_line()
                .forbid_too_long_line()
                .enter(|fmt| self.replacement.format(fmt))
                .is_err()
            {
                fmt.write_newline()?;
                self.replacement.format(fmt)?;
            }
            Ok(())
        })?;
        self.close.format(fmt)?;
        self.dot.format(fmt)?;
        Ok(())
    }
}

/// `-` (`include` | `include_lib`) `(` `$PATH` `)` `.`
///
/// - $PATH: [StringToken]
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
            indoc::indoc! {"
            -define(Baz(A, B),
                    A), { B)."},
            indoc::indoc! {"
            -define(name,
                    begin
                        hello,
                        world
                    end)."},
            indoc::indoc! {"
            -define(foo(A,
                        B,
                        C,
                        D,
                        E), F)."},
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
            -import(foo,
                    [bar/0,
                     baz/1])."},
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
            "-record(foo, {foo}).",
            indoc::indoc! {"
            -record(foo,
                    {foo, bar})."},
            indoc::indoc! {"
            -record(rec,
                    {field1 = [] :: Type1,
                     field2,
                     field3 = 421})."},
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
            %---10---|%---20---|
            -spec foo(A, B) ->
                      C;
                     (T0, T1) ->
                      T2;
                     (XXX,
                      YYY) -> Z."},
            indoc::indoc! {"
            -spec id(X) -> X
                          when X :: tuple()."},
            indoc::indoc! {"
            -callback foobar(atom()) ->
                          {atom(),
                           atom()}."},
            indoc::indoc! {"
            %---10---|%---20---|
            -spec foobar(A) ->
                      {atom(),
                       atom()}
                          when A :: atom();
                        (a) ->
                      b."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn type_decl_works() {
        let texts = [
            "-type foo() :: a.",
            indoc::indoc! {"
            %---10---|%---20---|
            -type foo() :: bar |
                           baz."},
            indoc::indoc! {"
            %---10---|%---20---|
            -type foo() ::
                    barr |
                    bazz."}, // TODO: Should not be newlined
            indoc::indoc! {"
            -type height() ::
                    pos_integer()."},
            indoc::indoc! {"
            -opaque orddict(Key,
                            Val) ::
                      [{Key,
                        Val}]."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }
}
