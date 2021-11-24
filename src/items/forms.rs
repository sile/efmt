//! Erlang top-level components such as attributes, directives or declarations.
use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::atoms::{
    CallbackAtom, DefineAtom, IncludeAtom, IncludeLibAtom, OpaqueAtom, RecordAtom, SpecAtom,
    TypeAtom,
};
use crate::items::components::{
    Clauses, Either, Element, Items, Maybe, Params, Parenthesized, TupleLike, WithArrow, WithGuard,
};
use crate::items::expressions::components::FunctionClause;
use crate::items::keywords::IfKeyword;
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::symbols::{
    CloseParenSymbol, ColonSymbol, CommaSymbol, DotSymbol, DoubleColonSymbol, HyphenSymbol,
    MatchSymbol, OpenParenSymbol,
};
use crate::items::tokens::{AtomToken, LexicalToken, StringToken, VariableToken};
use crate::items::Expr;
use crate::items::Type;
use crate::parse::Parse;
use crate::span::Span;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Span, Parse, Format)]
pub(super) enum Form {
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
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordDecl(AttrLike<RecordAtom, RecordDeclValue>);

#[derive(Debug, Clone, Span, Parse)]
struct RecordDeclValue {
    name: AtomToken,
    comma: CommaSymbol,
    fields: TupleLike<RecordField>,
}

impl Format for RecordDeclValue {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.name.format(fmt);
            self.comma.format(fmt);
            fmt.add_space();
            fmt.subregion(
                Indent::inherit(),
                Newline::if_too_long_or_multi_line(),
                |fmt| self.fields.format(fmt),
            );
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Element)]
struct RecordField {
    name: AtomToken,
    default: Maybe<(MatchSymbol, Expr)>,
    r#type: Maybe<(DoubleColonSymbol, Type)>,
}

impl Format for RecordField {
    fn format(&self, fmt: &mut Formatter) {
        self.name.format(fmt);
        if let Some((x, y)) = self.default.get() {
            fmt.add_space();
            x.format(fmt);
            fmt.add_space();
            y.format(fmt);
        }
        if let Some((x, y)) = self.r#type.get() {
            fmt.add_space();
            x.format(fmt);
            fmt.add_space();
            y.format(fmt);
        }
    }
}

/// `-` (`type` | `opaque`) `$NAME` `(` (`$PARAM` `,`?)* `)` `::` `$TYPE` `.`
///
/// - $NAME: [AtomToken]
/// - $PARAM: [VariableToken]
/// - $TYPE: [Type]
///
/// Note that the parenthesized notation like `-type(foo() :: bar()).` is also acceptable
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TypeDecl(AttrLike<TypeDeclName, TypeDeclItem>);

type TypeDeclName = Either<TypeAtom, OpaqueAtom>;

#[derive(Debug, Clone, Span, Parse)]
struct TypeDeclItem {
    name: AtomToken,
    params: Params<VariableToken>,
    delimiter: DoubleColonSymbol,
    r#type: Type,
}

impl Format for TypeDeclItem {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.name.format(fmt);
            self.params.format(fmt);
            fmt.add_space();
            self.delimiter.format(fmt);
            fmt.add_space();
            fmt.subregion(Indent::Offset(2), Newline::if_too_long(), |fmt| {
                self.r#type.format(fmt)
            });
        });
    }
}

/// `-` (`spec` | `callback`) `$NAME` (`(` (`$PARAM` `,`?)* `)` `->` `$RETURN` `;`?)+ `.`
///
/// - $NAME: ([AtomToken] `:`)? [AtomToken]
/// - $PARAM: [Type]
/// - $RETURN: [Type]
///
/// Note that the parenthesized notation like `-spec(foo() -> bar()).` is also acceptable
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunSpec(AttrLike<FunSpecName, FunSpecItem>);

type FunSpecName = Either<SpecAtom, CallbackAtom>;

#[derive(Debug, Clone, Span, Parse)]
struct FunSpecItem {
    module_name: Maybe<(AtomToken, ColonSymbol)>,
    function_name: AtomToken,
    clauses: Clauses<SpecClause>,
}

impl Format for FunSpecItem {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.module_name.format(fmt);
            self.function_name.format(fmt);
            self.clauses.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct SpecClause {
    params: WithArrow<Params<Type>>,
    r#return: WithGuard<Type, Type, CommaSymbol>,
}

impl Format for SpecClause {
    fn format(&self, fmt: &mut Formatter) {
        self.params.format(fmt);
        fmt.subregion(
            Indent::ParentOffset(4),
            Newline::if_too_long_or_multi_line(),
            |fmt| self.r#return.format(fmt),
        );
    }
}

/// (`$NAME` `(` (`$PARAM` `,`?)* `)` (`when` `$GUARD`)? `->` `$BODY` `;`?)+ `.`
///
/// - $NAME: [AtomToken]
/// - $PARAM: [Expr]
/// - $GUARD: ([Expr] (`,` | `;`)?)+
/// - $BODY: ([Expr] `,`?)+
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunDecl {
    clauses: Clauses<FunctionClause<AtomToken>>,
    dot: DotSymbol,
}

/// `-` `$NAME` `$ARGS`? `.`
///
/// - $NAME: [AtomToken] | `if`
/// - $ARGS: `(` (`$ARG` `,`?)* `)`
/// - $ARG: [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Attr(AttrLike<AttrName, AttrValue>);

type AttrName = Either<AtomToken, IfKeyword>;
type AttrValue = Items<Expr>;

#[derive(Debug, Clone, Span, Parse)]
struct AttrLike<Name, Value> {
    hyphen: HyphenSymbol,
    name: Name,
    value: Either<Parenthesized<Value>, Value>,
    dot: DotSymbol,
}

impl<Name: Format, Value: Format> Format for AttrLike<Name, Value> {
    fn format(&self, fmt: &mut Formatter) {
        self.hyphen.format(fmt);
        self.name.format(fmt);
        if matches!(self.value, Either::B(_)) {
            // Note that `self.value` may be empty.
            // In that case, the additional space will be removed by the following `fmt.cancel_whitespaces()` call.
            fmt.add_space();
        }
        self.value.format(fmt);
        fmt.cancel_whitespaces();
        self.dot.format(fmt);
    }
}

/// `-` `define` `(` `$NAME` `$VARS`? `,` `REPLACEMENT`* `)` `.`
///
/// - $NAME: [AtomToken] | [VariableToken]
/// - $VARS: `(` ([VariableToken] `,`?)* `)`
/// - $REPLACEMENT: [LexicalToken]
#[derive(Debug, Clone, Span, Parse)]
pub struct DefineDirective {
    hyphen: HyphenSymbol,
    define: DefineAtom,
    open: OpenParenSymbol,
    macro_name: MacroName,
    variables: Maybe<Params<VariableToken>>,
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
        self.variables.get().map(|x| x.get())
    }

    pub fn replacement(&self) -> &[LexicalToken] {
        self.replacement.tokens()
    }
}

impl Format for DefineDirective {
    fn format(&self, fmt: &mut Formatter) {
        self.hyphen.format(fmt);
        self.define.format(fmt);
        self.open.format(fmt);
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.macro_name.format(fmt);
            self.variables.format(fmt);
            self.comma.format(fmt);
            fmt.add_space();
            fmt.subregion(
                Indent::inherit(),
                Newline::if_too_long_or_multi_line(),
                |fmt| self.replacement.format(fmt),
            );
        });
        self.close.format(fmt);
        self.dot.format(fmt);
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
            match crate::erl::code_lib_dir(app_name) {
                Err(e) => {
                    log::warn!("{}", e);
                    None
                }
                Ok(mut resolved_path) => {
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
            indoc::indoc! {"
            -export [foo/0,
                     bar/1]."},
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
            -spec id(X) ->
                      X when X :: tuple()."},
            indoc::indoc! {"
            -spec id(X) ->
                      X when is_subtype(X,
                                        atom()),
                             X :: atom()."},
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
            indoc::indoc! {"
            -spec foo:bar() ->
                      baz()."},
            indoc::indoc! {"
            -spec(foo:bar() ->
                      baz())."},
        ];
        for text in texts {
            crate::assert_format!(text, Form);
        }
    }

    #[test]
    fn type_decl_works() {
        let texts = [
            "-type foo() :: a.",
            "-type(foo() :: a).",
            indoc::indoc! {"
            %---10---|%---20---|
            -type foo() :: bar |
                           baz."},
            indoc::indoc! {"
            %---10---|%---20---|
            -type foo() ::
                    barr | bazz."},
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
