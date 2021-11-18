//! Erlang types.
//!
//! <https://www.erlang.org/doc/reference_manual/typespec.html>
use crate::format::Format;
use crate::items::generics::{
    Args2, BinaryOpLike, Either, ListLike, Maybe, NonEmptyItems2, Params, Parenthesized,
    Parenthesized2, TupleLike, UnaryOpLike,
};
use crate::items::keywords::{
    BandKeyword, BnotKeyword, BorKeyword, BslKeyword, BsrKeyword, BxorKeyword, DivKeyword,
    FunKeyword, RemKeyword,
};
use crate::items::styles::{RightSpace, Space, TrailingColumns};
use crate::items::symbols::{
    ColonSymbol, CommaSymbol, DoubleColonSymbol, DoubleDotSymbol, DoubleLeftAngleSymbol,
    DoubleRightAngleSymbol, DoubleRightArrowSymbol, HyphenSymbol, MapMatchSymbol, MultiplySymbol,
    PlusSymbol, RightArrowSymbol, SharpSymbol, TripleDotSymbol, VerticalBarSymbol,
};
use crate::items::tokens::{AtomToken, CharToken, IntegerToken, TokenStr, VariableToken};
use crate::items::Type;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

#[derive(Debug, Clone, Span, Format)]
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
pub struct UnionType(NonEmptyItems2<NonUnionType, Space<VerticalBarSymbol>>);

#[derive(Debug, Clone, Span, Parse, Format)]
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

/// [VariableToken] `::` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct AnnotatedVariableType {
    variable: VariableToken,
    colon: Space<DoubleColonSymbol>,
    ty: Type,
}

/// [Type] `$OP` [Type]
///
/// - $OP: [BinaryOp]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BinaryOpType(BinaryOpLike<NonLeftRecursiveType, BinaryOp, Type, 0>);

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
    Mul(Space<MultiplySymbol>),
    Plus(Space<PlusSymbol>),
    Minus(Space<HyphenSymbol>),
    Div(Space<DivKeyword>),
    Rem(Space<RemKeyword>),
    Band(Space<BandKeyword>),
    Bor(Space<BorKeyword>),
    Bxor(Space<BxorKeyword>),
    Bsl(Space<BslKeyword>),
    Bsr(Space<BsrKeyword>),
    Range(DoubleDotSymbol),
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
    Bnot(Space<BnotKeyword>),
}

impl TokenStr for UnaryOp {
    fn token_str(&self) -> &str {
        match self {
            Self::Plus(x) => x.token_str(),
            Self::Minus(x) => x.token_str(),
            Self::Bnot(x) => x.get().token_str(),
        }
    }
}

/// `fun` `(` (`$PARAMS` `->` `$RETURN`)? `)`
///
/// - $PARAMS: `(` `...` `)` | `(` ([Type] `,`?)* `)`
/// - $RETURN: [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionType {
    fun: Space<FunKeyword>,
    params_and_return: Parenthesized2<Maybe<FunctionParamsAndReturn>>,
}

type FunctionParamsAndReturn = BinaryOpLike<FunctionParams, Space<RightArrowSymbol>, Type, 8>;

#[derive(Debug, Clone, Span, Parse, Format)]
enum FunctionParams {
    Any(Parenthesized2<TripleDotSymbol>),
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
    args: Args2<Type>,
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

type MapItem = BinaryOpLike<Type, Space<Either<DoubleRightArrowSymbol, MapMatchSymbol>>, Type, 4>;

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

type RecordItem = BinaryOpLike<AtomToken, Space<DoubleColonSymbol>, Type, 4>;

/// `<<` `$BITS_SIZE`? `,`? `$UNIT_SIZE`? `>>`
///
/// - $BITS_SIZE: `_` `:` [Type]
/// - $UNIT_SIZE: `_` `:` `_` `*` [Type]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringType {
    open: DoubleLeftAngleSymbol,
    size: Maybe<TrailingColumns<BitstringSize, 2>>, // trailing: ">>"
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
enum BitstringSize {
    BitsAndUnit(
        Box<BinaryOpLike<BitstringBitsSize, RightSpace<CommaSymbol>, BitstringUnitSize, 0>>,
    ),
    Unit(Box<BitstringUnitSize>),
    Bits(Box<BitstringBitsSize>),
}

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
