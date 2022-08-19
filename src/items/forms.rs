//! Erlang top-level components such as attributes, directives or declarations.
use super::components::Guard;
use super::symbols::RightArrowSymbol;
use crate::format::{Format, Formatter};
use crate::items::atoms::{
    CallbackAtom, DefineAtom, ExportAtom, ExportTypeAtom, IncludeAtom, IncludeLibAtom, ModuleAtom,
    OpaqueAtom, RecordAtom, SpecAtom, TypeAtom,
};
use crate::items::components::{
    Clauses, Either, Element, Items, Maybe, Never, NonEmptyItems, Null, Params, Parenthesized,
    RecordFieldsLike,
};
use crate::items::expressions::components::FunctionClause;
use crate::items::keywords::{ElseKeyword, IfKeyword};
use crate::items::macros::{MacroName, MacroReplacement};
use crate::items::symbols::{
    CloseParenSymbol, CloseSquareSymbol, ColonSymbol, CommaSymbol, DotSymbol, DoubleColonSymbol,
    HyphenSymbol, MatchSymbol, OpenParenSymbol, OpenSquareSymbol, SlashSymbol,
};
use crate::items::tokens::{AtomToken, IntegerToken, LexicalToken, StringToken, VariableToken};
use crate::items::Expr;
use crate::items::Type;
use crate::parse::{Parse, TokenStream};
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub(super) enum Form {
    Define(DefineDirective),
    Include(IncludeDirective),
    FunSpec(FunSpec),
    FunDecl(FunDecl),
    TypeDecl(TypeDecl),
    RecordDecl(RecordDecl),
    Export(ExportAttr),
    Module(ModuleAttr),
    Attr(Attr),
}

impl Format for Form {
    fn format(&self, fmt: &mut Formatter) {
        match self {
            Form::Define(x) => x.format(fmt),
            Form::Include(x) => x.format(fmt),
            Form::FunSpec(x) => x.format(fmt),
            Form::FunDecl(x) => x.format(fmt),
            Form::TypeDecl(x) => x.format(fmt),
            Form::RecordDecl(x) => x.format(fmt),
            Form::Export(x) => x.format(fmt),
            Form::Module(x) => x.format(fmt),
            Form::Attr(x) => x.format(fmt),
        }
        fmt.write_subsequent_comments();
    }
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
    fields: RecordFieldsLike<RecordField>,
}

impl Format for RecordDeclValue {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.name.format(fmt);
            self.comma.format(fmt);
            fmt.write_space();
            self.fields.format(fmt);
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
        fmt.with_scoped_indent(|fmt| {
            self.name.format(fmt);
            if let Some((x, y)) = self.default.get() {
                let newline = fmt.has_newline_until(&self.default.end_position());
                fmt.write_space();
                x.format(fmt);
                if newline {
                    fmt.set_indent(fmt.indent() + 4);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                y.format(fmt);
            }
            if let Some((x, y)) = self.r#type.get() {
                fmt.write_space();
                x.format(fmt);
                fmt.write_space();
                y.format(fmt);
            }
        });
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
        fmt.with_scoped_indent(|fmt| {
            let base_indent = fmt.column();
            self.name.format(fmt);
            self.params.format(fmt);
            fmt.write_space();
            self.delimiter.format(fmt);
            fmt.with_scoped_indent(|fmt| {
                if fmt.has_newline_until(&self.r#type) {
                    fmt.set_indent(base_indent + 2);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                self.r#type.format(fmt);
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
pub struct FunSpec(
    Either<AttrLike<SpecAtom, FunSpecItem<6>>, AttrLike<CallbackAtom, FunSpecItem<10>>>,
);

#[derive(Debug, Clone, Span, Parse)]
struct FunSpecItem<const INDENT: usize> {
    module_name: Maybe<(AtomToken, ColonSymbol)>,
    function_name: AtomToken,
    clauses: Clauses<SpecClause<INDENT>>,
}

impl<const INDENT: usize> Format for FunSpecItem<INDENT> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.module_name.format(fmt);
            self.function_name.format(fmt);
            self.clauses.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct SpecClause<const INDENT: usize> {
    params: Params<Type>,
    arrow: RightArrowSymbol,
    r#return: Type,
    guard: Maybe<Guard<Type, CommaSymbol>>,
}

impl<const INDENT: usize> Format for SpecClause<INDENT> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            // 'Params'
            self.params.format(fmt);
            fmt.write_space();

            // '->'
            let multiline = fmt.has_newline_until(&self.r#return);
            self.arrow.format(fmt);

            // 'Return'
            if multiline {
                fmt.set_indent(INDENT + 4);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }
            self.r#return.format(fmt);

            // 'Guard'
            if let Some(guard) = self.guard.get() {
                if fmt.has_newline_until(guard.conditions()) {
                    fmt.set_indent(INDENT + 8);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }
                guard.format(fmt);
            }
        });
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

/// `-` `module` `(` [AtomToken] `)` `.`
#[derive(Debug, Clone, Span, Format)]
pub struct ModuleAttr(AttrLike<ModuleAtom, AtomToken>);

impl Parse for ModuleAttr {
    fn parse(ts: &mut TokenStream) -> crate::parse::Result<Self> {
        let attr = AttrLike::<ModuleAtom, AtomToken>::parse(ts)?;
        if ts.filepath().is_none() {
            let module_name = attr.value().value();
            ts.set_filepath(format!("{module_name}.erl"));
        }
        Ok(ModuleAttr(attr))
    }
}

/// `-` `export|export_type` `$EXPORTS` `.`
///
/// - $EXPORTS: `(` (`$EXPORT`,`?)* `)`
/// - $EXPORT: [AtomToken] `/` [IntegerToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ExportAttr(AttrLike<Either<ExportAtom, ExportTypeAtom>, ExportItems>);

#[derive(Debug, Clone, Span, Parse)]
struct ExportItems {
    open: OpenSquareSymbol,
    items: Items<ExportItem, CommaSymbol>,
    close: CloseSquareSymbol,
}

impl Format for ExportItems {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);

        let items = self.items.items();
        let delimiters = self.items.delimiters();
        if !items.is_empty() {
            fmt.with_scoped_indent(|fmt| {
                fmt.set_indent(fmt.column());
                items.first().expect("unreachable").format(fmt);

                for (prev_item, (item, delimiter)) in items
                    .iter()
                    .zip(items.iter().skip(1).zip(delimiters.iter()))
                {
                    delimiter.format(fmt);

                    let is_same_group = prev_item.name.value() == item.name.value()
                        && prev_item.start_position().line() + 1 >= item.end_position().line();
                    if is_same_group {
                        fmt.write_space();
                        item.format(fmt);
                    } else {
                        fmt.write_newline();
                        item.format(fmt);
                    }
                }
            });
        }

        self.close.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ExportItem {
    name: AtomToken,
    slash: SlashSymbol,
    arity: IntegerToken,
}

/// `-` `$NAME` `$ARGS`? `.`
///
/// - $NAME: [AtomToken] | `if`
/// - $ARGS: `(` (`$ARG` `,`?)* `)`
/// - $ARG: [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Attr(AttrLike<AttrName, AttrValue, Null>);

type AttrName = Either<AtomToken, Either<IfKeyword, ElseKeyword>>;
type AttrValue = NonEmptyItems<Expr>;

#[derive(Debug, Clone, Span, Parse)]
struct AttrLike<Name, Value, Empty = Never> {
    hyphen: HyphenSymbol,
    name: Name,
    value: Either<Parenthesized<Value>, Either<Value, Empty>>,
    dot: DotSymbol,
}

impl<Name, Value> AttrLike<Name, Value> {
    fn value(&self) -> &Value {
        match &self.value {
            Either::A(x) => x.get(),
            Either::B(x) => {
                if let Either::A(x) = x {
                    x
                } else {
                    unreachable!();
                }
            }
        }
    }
}

impl<Name: Format, Value: Format, Empty: Format> Format for AttrLike<Name, Value, Empty> {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            self.hyphen.format(fmt);
            self.name.format(fmt);
            if matches!(self.value, Either::B(Either::A(_))) {
                fmt.write_space();
            }
            self.value.format(fmt);
            self.dot.format(fmt);
        };

        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        }
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

    pub fn format_with_indent(&self, fmt: &mut Formatter, replacement_indent: Option<usize>) {
        self.hyphen.format(fmt);
        self.define.format(fmt);
        self.open.format(fmt);
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());

            self.macro_name.format(fmt);
            self.variables.format(fmt);
            self.comma.format(fmt);
            if fmt.has_newline_until(&self.replacement) {
                fmt.write_newline();
            } else {
                let indent = replacement_indent.unwrap_or_else(|| fmt.column() + 1);
                fmt.write_spaces(indent - fmt.column());
            }

            self.replacement.format(fmt);
        });
        self.close.format(fmt);
        self.dot.format(fmt);
    }
}

impl Format for DefineDirective {
    fn format(&self, fmt: &mut Formatter) {
        self.format_with_indent(fmt, None);
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
            indoc::indoc! {"
            -record(foo, {
                      foo
                     })."},
            indoc::indoc! {"
            -record(foo, {
                      foo,
                      bar
                     })."},
            indoc::indoc! {"
            -record(foo, {
                      foo,
                      bar

                      %% baz
                     })."},
            indoc::indoc! {"
            -record(rec, {
                      field1 = [] :: Type1,
                      field2,
                      field3 = 421
                     })."},
            indoc::indoc! {"
            -record(rec, {
                      field1 =
                          [] :: Type1,
                      field2
                     })."},
            indoc::indoc! {"
            -record(rec, {field1 = [] :: Type1, field2, field3 = 421})."},
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
              when a,
                   b;
                   c ->
                d."},
            indoc::indoc! {"
            foo(A)
              when is_atom(A) ->
                bar,
                baz;
            foo(B) when is_integer(B);
                        is_float(B) ->
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
            -spec foobar(A) ->
                      {atom(),
                       atom()}
                          when A :: atom();
                        (a) ->
                      b."},
            indoc::indoc! {"
            -spec foobar(A) -> {atom(), atom()}
                          when A :: atom()."},
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
            -type foo() :: bar |
                           baz."},
            indoc::indoc! {"
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
