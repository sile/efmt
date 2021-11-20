//! Erlang types.
//!
//! <https://www.erlang.org/doc/reference_manual/typespec.html>
use crate::format::{self, Format};
use crate::format2::{Format2, Formatter2};
use crate::items::generics::{
    Args, BinaryOpLike, BinaryOpStyle, BitstringLike, Either, ListLike, Maybe, NonEmptyItems,
    Params, Parenthesized, TupleLike, UnaryOpLike,
};
use crate::items::keywords::{
    BandKeyword, BnotKeyword, BorKeyword, BslKeyword, BsrKeyword, BxorKeyword, DivKeyword,
    FunKeyword, RemKeyword,
};
use crate::items::symbols::{
    ColonSymbol, DoubleColonSymbol, DoubleDotSymbol, HyphenSymbol, MultiplySymbol, PlusSymbol,
    RightArrowSymbol, SharpSymbol, TripleDotSymbol, VerticalBarSymbol,
};
use crate::items::tokens::{AtomToken, CharToken, IntegerToken, VariableToken};
use crate::items::Type;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span, Format, Format2)]
enum NonUnionType {
    BinaryOp(Box<BinaryOpType>),
    NonLeftRecursive(NonLeftRecursiveType),
}

impl Parse for NonUnionType {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let expr: NonLeftRecursiveType = ts.parse()?;
        if ts.peek::<BinaryOp>().is_some() {
            ts.resume_parse(expr).map(Self::BinaryOp)
        } else {
            Ok(Self::NonLeftRecursive(expr))
        }
    }
}

/// [Type] `|` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnionType(NonEmptyItems<NonUnionType, UnionDelimiter>);

impl Format2 for UnionType {
    fn format2(&self, fmt: &mut Formatter2) {
        self.0.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct UnionDelimiter(VerticalBarSymbol);

impl Format for UnionDelimiter {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.write_space()?;
        self.0.format(fmt)?;
        Ok(())
    }
}

impl Format2 for UnionDelimiter {
    fn format2(&self, fmt: &mut Formatter2) {
        fmt.add_space();
        self.0.format2(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse, Format)] // TODO: derive Format2
enum NonLeftRecursiveType {
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

impl Format2 for NonLeftRecursiveType {
    fn format2(&self, fmt: &mut Formatter2) {
        match self {
            Self::Mfargs(x) => x.format2(fmt),
            Self::Literal(x) => x.format2(fmt),
            _ => todo!("{:?}", self),
        }
    }
}

/// [VariableToken] `::` [Type]
#[derive(Debug, Clone, Span, Parse)]
pub struct AnnotatedVariableType {
    variable: VariableToken,
    colon: DoubleColonSymbol,
    ty: Type,
}

impl Format for AnnotatedVariableType {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.variable.format(fmt)?;
        fmt.write_space()?;
        self.colon.format(fmt)?;
        fmt.write_space()?;
        self.ty.format(fmt)?;
        Ok(())
    }
}

/// [Type] `$OP` [Type]
///
/// - $OP: [BinaryOp]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BinaryOpType(BinaryOpLike<NonLeftRecursiveType, BinaryOp, Type>);

impl Format2 for BinaryOpType {
    fn format2(&self, fmt: &mut Formatter2) {
        todo!()
    }
}

impl ResumeParse<NonLeftRecursiveType> for BinaryOpType {
    fn resume_parse(
        ts: &mut parse::TokenStream,
        left: NonLeftRecursiveType,
    ) -> parse::Result<Self> {
        ts.resume_parse(left).map(Self)
    }
}

/// `*` | `+` | `-` | `div` | `rem` | `band` | `bor` | `bxor` | `bsl` | `bsr` | `..`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BinaryOp {
    Mul(MultiplySymbol),
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Div(DivKeyword),
    Rem(RemKeyword),
    Band(BandKeyword),
    Bor(BorKeyword),
    Bxor(BxorKeyword),
    Bsl(BslKeyword),
    Bsr(BsrKeyword),
    Range(DoubleDotSymbol),
}

impl BinaryOpStyle for BinaryOp {
    fn indent_offset(&self) -> usize {
        0
    }

    fn allow_newline(&self) -> bool {
        true
    }

    fn should_pack(&self) -> bool {
        true
    }
}

/// `$OP` [Type]
///
/// - $OP: [UnaryOp]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpType(UnaryOpLike<UnaryOp, NonLeftRecursiveType>);

/// `+` | `-` | `bnot`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Bnot(BnotKeyword),
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

impl Format for FunctionType {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        self.fun.format(fmt)?;
        fmt.write_space()?;
        self.params_and_return.format(fmt)?;
        Ok(())
    }
}

type FunctionParamsAndReturn = BinaryOpLike<FunctionParams, RightArrowDelimiter, Type>;

#[derive(Debug, Clone, Span, Parse, Format)]
struct RightArrowDelimiter(RightArrowSymbol);

impl BinaryOpStyle for RightArrowDelimiter {
    fn indent_offset(&self) -> usize {
        8
    }

    fn allow_newline(&self) -> bool {
        true
    }

    fn should_pack(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
enum FunctionParams {
    Any(Parenthesized<TripleDotSymbol>),
    Params(Params<Type>),
}

/// [AtomToken] | [CharToken] | [IntegerToken] | [VariableToken]
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
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
#[derive(Debug, Clone, Span, Parse, Format, Format2)]
pub struct MfargsType {
    module: Maybe<(AtomToken, ColonSymbol)>,
    name: AtomToken,
    args: Args<Type>,
}

/// `[` (`$ITEM` `,`?)* `]`
///
/// - $ITEM: [Type] | `...`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListType(ListLike<Either<Type, TripleDotSymbol>>);

/// `{` ([Type] `,`)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleType(TupleLike<Type>);

/// `#` `{` ([Type] (`:=` | `=>`) [Type] `,`?)* `}`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapType {
    sharp: SharpSymbol,
    items: TupleLike<MapItem>,
}

type MapItem = BinaryOpLike<Type, crate::items::expressions::maps::MapDelimiter, Type>;

/// `#` `$NAME` `{` (`$FIELD` `,`?)* `}`
///
/// - $NAME: [AtomToken]
/// - $FIELD: [AtomToken] `::` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordType {
    sharp: SharpSymbol,
    name: AtomToken,
    fields: TupleLike<RecordItem>,
}

type RecordItem = BinaryOpLike<AtomToken, DoubleColonDelimiter, Type>;

#[derive(Debug, Clone, Span, Parse, Format)]
struct DoubleColonDelimiter(DoubleColonSymbol);

impl BinaryOpStyle for DoubleColonDelimiter {
    fn indent_offset(&self) -> usize {
        4
    }

    fn allow_newline(&self) -> bool {
        true
    }

    fn should_pack(&self) -> bool {
        false
    }
}

/// `<<` `$BITS_SIZE`? `,`? `$UNIT_SIZE`? `>>`
///
/// - $BITS_SIZE: `_` `:` [Type]
/// - $UNIT_SIZE: `_` `:` `_` `*` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringType(BitstringLike<Either<BitstringUnitSize, BitstringBitsSize>>);

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringBitsSize {
    underscore: VariableToken,
    colon: ColonSymbol,
    size: Type,
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct BitstringUnitSize {
    underscore0: VariableToken,
    colon: ColonSymbol,
    underscore1: VariableToken,
    mul: MultiplySymbol,
    size: Type,
}

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
                    1,
                    baz(),
                    qux())"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
            crate::assert_format2!(text, Type);
        }
    }

    #[test]
    fn list_works() {
        let texts = [
            "[]",
            "[foo()]",
            "[10, ...]",
            indoc::indoc! {"
            %---10---|%---20---|
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
            %---10---|%---20---|
            #{atom() :=
                  integer()}"},
            indoc::indoc! {"
            %---10---|%---20---|
            #{atom() :=
                  {aaa,
                   bbb,
                   ccc}}"},
            indoc::indoc! {"
            #{a => b,
              1 := 2,
              atom() := atom()}"},
        ];
        for text in expected {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn record_works() {
        let texts = [
            "#foo{}",
            "#foo{bar :: atom()}",
            indoc::indoc! {"
            %---10---|%---20---|
            #foo{bar ::
                     integer()}"},
            indoc::indoc! {"
            #foo{bar :: b,
                 baz :: 2}"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn function_works() {
        let texts = [
            "fun ()",
            "fun (() -> integer())",
            indoc::indoc! {"
            %---10---|%---20---|
            fun ((...) ->
                         atom())"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun ((A, b, $c) ->
                         tuple())"},
            indoc::indoc! {"
            %---10---|%---20---|
            fun ((A,
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
        let texts = ["-10", "+10", "bnot 100", "- -+ +3"];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }

    #[test]
    fn binary_op_works() {
        let texts = [
            "-10 + 20 rem 3",
            indoc::indoc! {"
            %---10---|%---20---|
            foo |
            (3 + 10) |
            -1 .. +20"},
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
            %---10---|%---20---|
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
            %---10---|%---20---|
            Foo :: [bar:baz(qux)]"},
            indoc::indoc! {"
            %---10---|%---20---|
            Foo :: atom() |
                   integer() |
                   bar"},
        ];
        for text in texts {
            crate::assert_format!(text, Type);
        }
    }
}
