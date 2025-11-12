//! Erlang types.
//!
//! <https://www.erlang.org/doc/reference_manual/typespec.html>
use self::components::{BinaryOp, BitstringItem, UnaryOp};
use crate::format::{Format, Formatter};
use crate::items::Type;
use crate::items::components::{
    Args, BitstringLike, Either, Element, ListLike, MapLike, Maybe, NonEmptyItems, Params,
    Parenthesized, RecordLike, TupleLike,
};
use crate::items::keywords::FunKeyword;
use crate::items::symbols::{
    ColonSymbol, DoubleColonSymbol, RightArrowSymbol, SharpSymbol, TripleDotSymbol,
    VerticalBarSymbol,
};
use crate::items::tokens::{AtomToken, CharToken, IntegerToken, VariableToken};
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

pub mod components;

/// [Type] `|` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnionType(NonEmptyItems<NonUnionType, UnionDelimiter>);

impl UnionType {
    pub(crate) fn items(&self) -> &[NonUnionType] {
        self.0.items()
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct UnionDelimiter(VerticalBarSymbol);

impl Format for UnionDelimiter {
    fn format(&self, fmt: &mut Formatter) {
        fmt.write_space();
        self.0.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Format)]
pub enum NonUnionType {
    Base(BaseType),
    BinaryOp(Box<BinaryOpType>),
}

impl Parse for NonUnionType {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr: BaseType = ts.parse()?;
        if ts.peek::<BinaryOp>().is_some() {
            ts.resume_parse(expr).map(Self::BinaryOp)
        } else {
            Ok(Self::Base(expr))
        }
    }
}

// Non left-recursive type.
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BaseType {
    Mfargs(Box<MfargsType>),
    List(Box<ListType>),
    Tuple(Box<TupleType>),
    Map(Box<MapType>),
    Record(Box<RecordType>),
    Bitstring(Box<BitstringType>),
    Function(Box<FunctionType>),
    UnaryOp(Box<UnaryOpType>),
    Parenthesized(Box<Parenthesized<Type>>),
    Annotated(Box<AnnotatedVariableType>),
    Literal(LiteralType),
}

/// [VariableToken] `::` [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct AnnotatedVariableType {
    variable: VariableToken,
    colon: DoubleColonSymbol,
    ty: Type,
}

impl AnnotatedVariableType {
    pub fn variable(&self) -> &VariableToken {
        &self.variable
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

impl Format for AnnotatedVariableType {
    fn format(&self, fmt: &mut Formatter) {
        self.variable.format(fmt);
        fmt.write_space();
        self.colon.format(fmt);
        fmt.write_space();
        self.ty.format(fmt);
    }
}

/// [Type] [BinaryOp] [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpType {
    left: BaseType,
    op: BinaryOp,
    right: Type,
}

impl ResumeParse<BaseType> for BinaryOpType {
    fn resume_parse(ts: &mut parse::TokenStream, left: BaseType) -> parse::Result<Self> {
        Ok(Self {
            left,
            op: ts.parse()?,
            right: ts.parse()?,
        })
    }
}

impl Format for BinaryOpType {
    fn format(&self, fmt: &mut Formatter) {
        let needs_space = !matches!(self.op, BinaryOp::Range(_));

        self.left.format(fmt);
        if needs_space {
            fmt.write_space();
        }
        self.op.format(fmt);
        if needs_space {
            fmt.write_space();
        }
        self.right.format(fmt);
    }
}

/// [UnaryOp] [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct UnaryOpType {
    op: UnaryOp,
    ty: BaseType,
}

impl Format for UnaryOpType {
    fn format(&self, fmt: &mut Formatter) {
        let last = fmt.last_char().unwrap_or('\n');
        if !matches!(last, '\n' | ' ' | '.') {
            fmt.write_space();
        }

        self.op.format(fmt);
        if matches!(self.op, UnaryOp::Bnot(_)) {
            fmt.write_space();
        }
        self.ty.format(fmt);
    }
}

/// `fun` `(` (`$PARAMS` `->` `$RETURN`)? `)`
///
/// - $PARAMS: `(` `...` `)` | `(` ([Type] `,`?)* `)`
/// - $RETURN: [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct FunctionType {
    fun: FunKeyword,
    params_and_return: Parenthesized<Maybe<FunctionParamsAndReturn>>,
}

impl FunctionType {
    pub fn params(&self) -> impl Iterator<Item = &Type> {
        self.params_and_return
            .get()
            .get()
            .and_then(|x| match &x.params {
                FunctionParams::Any(_) => None,
                FunctionParams::Params(x) => Some(x.get()),
            })
            .into_iter()
            .flat_map(|x| x.iter())
    }

    pub fn return_type(&self) -> Option<&Type> {
        self.params_and_return.get().get().map(|x| &x.ty)
    }
}

impl Format for FunctionType {
    fn format(&self, fmt: &mut Formatter) {
        self.fun.format(fmt);
        self.params_and_return.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct FunctionParamsAndReturn {
    params: FunctionParams,
    arrow: RightArrowSymbol,
    ty: Type,
}

impl Format for FunctionParamsAndReturn {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            // 'Params'
            self.params.format(fmt);
            fmt.write_space();

            // '->'
            let multiline = fmt.has_newline_until(&self.ty);
            self.arrow.format(fmt);
            if multiline {
                fmt.set_indent(fmt.indent() + 8);
                fmt.write_newline();
            } else {
                fmt.write_space();
            }

            // 'Type'
            self.ty.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
enum FunctionParams {
    Any(Parenthesized<TripleDotSymbol>),
    Params(Params<Type>),
}

/// [AtomToken] | [CharToken] | [IntegerToken] | [VariableToken]
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum LiteralType {
    Atom(AtomToken),
    Char(CharToken),
    Integer(IntegerToken),
    Variable(VariableToken),
}

/// (`$MODULE` `:`)? `$NAME` `(` (`$ARG` `,`)* `)`
///
/// - $MODULE: [AtomToken]
/// - $NAME: [AtomToken]
/// - $ARG: [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MfargsType {
    module: Maybe<(AtomToken, ColonSymbol)>,
    name: AtomToken,
    args: Args<Type>,
}

impl MfargsType {
    pub fn module_name(&self) -> Option<&AtomToken> {
        self.module.get().map(|(name, _)| name)
    }

    pub fn type_name(&self) -> &AtomToken {
        &self.name
    }

    pub fn args(&self) -> &[Type] {
        self.args.get()
    }
}

/// `[` (`$ITEM` `,`?)* `]`
///
/// - $ITEM: [Type] | `...`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListType(ListLike<ListItem>);

impl ListType {
    pub fn item_type(&self) -> Option<&Type> {
        self.0.items().first().and_then(|item| match &item.0 {
            Either::A(x) => Some(x),
            Either::B(_) => None,
        })
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ListItem(Either<Type, TripleDotSymbol>);

impl Element for ListItem {
    fn is_packable(&self) -> bool {
        false
    }
}

/// `{` ([Type] `,`)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleType(TupleLike<TupleItem>);

impl TupleType {
    pub fn items(&self) -> (Option<&AtomToken>, impl Iterator<Item = &Type>) {
        let (tag, items) = self.0.items();
        (tag, items.iter().map(|item| &item.0))
    }
}

#[derive(Debug, Clone, Span, Parse, Format, Element)]
struct TupleItem(Type);

/// `#` `{` ([Type] (`:=` | `=>`) [Type] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapType(MapLike<SharpSymbol, Type>);

impl MapType {
    pub fn items(&self) -> impl Iterator<Item = (&Type, &Type)> {
        self.0.items()
    }
}

/// `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken] `::` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordType {
    record: RecordLike<(SharpSymbol, AtomToken), RecordItem>,
}

impl RecordType {
    pub fn name(&self) -> &AtomToken {
        &self.record.prefix().1
    }

    pub fn fields(&self) -> impl Iterator<Item = (&AtomToken, &Type)> {
        self.record.fields().map(|item| (&item.name, &item.ty))
    }
}

#[derive(Debug, Clone, Span, Parse, Element)]
struct RecordItem {
    name: AtomToken,
    colon: DoubleColonSymbol,
    ty: Type,
}

impl Format for RecordItem {
    fn format(&self, fmt: &mut Formatter) {
        self.name.format(fmt);
        fmt.write_space();
        self.colon.format(fmt);
        fmt.write_space();
        self.ty.format(fmt);
    }
}

/// `<<` `$BITS_SIZE`? `,`? `$UNIT_SIZE`? `>>`
///
/// - $BITS_SIZE: `_` `:` [Type]
/// - $UNIT_SIZE: `_` `:` `_` `*` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringType(BitstringLike<BitstringItem>);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mfargs_works() {
        let texts = [
            "foo()",
            "foo:bar(A, 1)",
            indoc::indoc! {"
            foo:bar(A,
                    BB,
                    baz())"},
            indoc::indoc! {"
            foo:bar(A,
                    B,
                    baz(12, 34),
                    qux())"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn list_works() {
        let texts = [
            "[]",
            "[foo()]",
            "[10, ...]",
            indoc::indoc! {"
            [fooooooooooo(),
             ...]"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn tuple_works() {
        let texts = [
            "{}",
            "{foo()}",
            "{atom, 1}",
            indoc::indoc! {"
            {foo(),
             bar(),
             [baz()]}"},
            indoc::indoc! {"
            {foo(),
             bar(),
             [baz(A, B)]}"},
            indoc::indoc! {"
            {foo, bar,
                  [baz(A, B)]}"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn map_works() {
        let expected = [
            "#{}",
            "#{a => b, 1 := 2}",
            indoc::indoc! {"
            #{
              atom() :=
                  integer()
             }"},
            indoc::indoc! {"
            #{
              atom() := {Aaa,
                         bbb,
                         ccc}
             }"},
            indoc::indoc! {"
            #{
              a => b,
              1 := 2,
              atom() := atom()
             }"},
            indoc::indoc! {"
            #{a => b, 1 := 2, atom() := atom()}"},
        ];
        for text in expected {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn record_works() {
        let texts = [
            "#foo{}",
            indoc::indoc! {"
            #foo{
              bar :: integer()
             }"},
            indoc::indoc! {"
            #foo{
              bar :: b,
              baz :: 2
             }"},
            indoc::indoc! {"
            #foo{bar :: b, baz :: 2}"},
            indoc::indoc! {"
            #foo{
              bar :: b,
              baz :: bb()
             }"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn function_works() {
        let texts = [
            "fun()",
            "fun(() -> integer())",
            indoc::indoc! {"
            fun((...) -> atom())"},
            indoc::indoc! {"
            fun((A, b, $c) ->
                        tuple())"},
            indoc::indoc! {"
            fun((A,
                 b,
                 $c,
                 {foo()}) ->
                        tuple())"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn unary_op_works() {
        let texts = ["-10", "+10", "bnot 100", "- - + +3"];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn binary_op_works() {
        let texts = [
            "-10 + 20 rem 3",
            indoc::indoc! {"
            foo |
            (3 + 10) |
            -1..+20"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn bitstring_works() {
        let texts = [
            "<<>>",
            "<<_:10>>",
            "<<_:_*8>>",
            "<<_:8, _:_*4>>",
            indoc::indoc! {"
            <<_:(1 + 3 + 4),
              _:_*4>>"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn annotated_variable_works() {
        let texts = [
            "Foo :: atom()",
            indoc::indoc! {"
            Foo :: [bar:baz(qux)]"},
            indoc::indoc! {"
            Foo :: atom() |
                   integer() |
                   bar"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }
}
