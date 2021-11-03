use crate::format::Format;
use crate::items::generics::{Either, Items, Maybe, Parenthesized};
use crate::items::keywords::{
    BandKeyword, BnotKeyword, BorKeyword, BslKeyword, BsrKeyword, BxorKeyword, DivKeyword,
    FunKeyword, RemKeyword,
};
use crate::items::symbols::{
    CloseBraceSymbol, CloseSquareSymbol, ColonSymbol, CommaSymbol, DoubleColonSymbol,
    DoubleDotSymbol, DoubleLeftAngleSymbol, DoubleRightAngleSymbol, DoubleRightArrowSymbol,
    HyphenSymbol, MapMatchSymbol, MatchSymbol, MultiplySymbol, OpenBraceSymbol, OpenSquareSymbol,
    PlusSymbol, RightArrowSymbol, SharpSymbol, TripleDotSymbol, VerticalBarSymbol,
};
use crate::items::tokens::{AtomToken, CharToken, IntegerToken, VariableToken};
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum Type {
    BinaryOp(Box<BinaryOpType>),
    NonLeftRecursive(NonLeftRecursiveType),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum NonLeftRecursiveType {
    Mfargs(Box<MfargsType>),
    List(Box<ListType>),
    Tuple(Box<TupleType>),
    Map(Box<MapType>),
    Record(Box<RecordType>),
    Bitstring(Box<BitstringType>),
    Function(Box<FunctionType>),
    UnaryOp(Box<UnaryOpType>),
    Parenthesized(Box<Parenthesized<Type>>),
    Literal(LiteralType),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BinaryOpType {
    left: NonLeftRecursiveType,
    op: BinaryOp,
    right: Type,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BinaryOp {
    Mul(MultiplySymbol),
    Div(DivKeyword),
    Rem(RemKeyword),
    Band(BandKeyword),
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Bor(BorKeyword),
    Bxor(BxorKeyword),
    Bsl(BslKeyword),
    Bsr(BsrKeyword),
    Union(VerticalBarSymbol),
    Range(DoubleDotSymbol),
    Annotation(DoubleColonSymbol),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpType {
    op: UnaryOp,
    r#type: NonLeftRecursiveType,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(PlusSymbol),
    Minus(HyphenSymbol),
    Bnot(BnotKeyword),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionType {
    fun: FunKeyword,
    params_and_return: Parenthesized<Maybe<(FunctionParams, (RightArrowSymbol, Type))>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionParams {
    Any(Parenthesized<TripleDotSymbol>),
    Params(Parenthesized<Items<Type, CommaSymbol>>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum LiteralType {
    Atom(AtomToken),
    Char(CharToken),
    Integer(IntegerToken),
    Variable(VariableToken),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MfargsType {
    module: Maybe<(AtomToken, ColonSymbol)>,
    name: AtomToken,
    args: Parenthesized<Items<Type, CommaSymbol>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct ListType {
    open: OpenSquareSymbol,
    item: Maybe<(Type, Maybe<(CommaSymbol, TripleDotSymbol)>)>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleType {
    open: OpenBraceSymbol,
    items: Items<Type, CommaSymbol>,
    close: CloseBraceSymbol,
}

pub type MapItem = (Type, (Either<DoubleRightArrowSymbol, MapMatchSymbol>, Type));

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapType {
    sharp: SharpSymbol,
    open: OpenBraceSymbol,
    items: Items<MapItem, CommaSymbol>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordType {
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: Items<(AtomToken, (MatchSymbol, Type)), CommaSymbol>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringType {
    open: DoubleLeftAngleSymbol,
    size: Maybe<BitstringSize>,
    close: DoubleRightAngleSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BitstringSize {
    BinaryAndUnit(Box<(BitstringBinarySize, (CommaSymbol, BitstringUnitSize))>),
    Unit(Box<BitstringUnitSize>),
    Binary(Box<BitstringBinarySize>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringBinarySize {
    underscore: VariableToken,
    colon: ColonSymbol,
    size: Type,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct BitstringUnitSize {
    underscore0: VariableToken,
    colon: ColonSymbol,
    underscore1: VariableToken,
    mul: MultiplySymbol,
    size: Type,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse::parse_text;

    #[test]
    fn mfargs_works() {
        let texts = ["foo()", "foo:bar(A, 1)"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Mfargs(_))
            ));
        }
    }

    #[test]
    fn list_works() {
        let texts = ["[]", "[foo()]", "[10,...]"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::List(_))
            ));
        }
    }

    #[test]
    fn tuple_works() {
        let texts = ["{}", "{foo()}", "{atom, 1}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Tuple(_))
            ));
        }
    }

    #[test]
    fn map_works() {
        let texts = ["#{}", "#{atom() := integer()}", "#{a => b, 1 := 2}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Map(_))
            ));
        }
    }

    #[test]
    fn record_works() {
        let texts = ["#foo{}", "#foo{bar = integer()}", "#foo{bar = b, baz = 2}"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Record(_))
            ));
        }
    }

    #[test]
    fn function_works() {
        let texts = [
            "fun ()",
            "fun ((...) -> atom())",
            "fun (() -> integer())",
            "fun ((A, b, $c) -> tuple())",
        ];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Function(_))
            ));
        }
    }

    #[test]
    fn unary_op_works() {
        let texts = ["-10", "+10", "bnot 100"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::UnaryOp(_))
            ));
        }
    }

    #[test]
    fn binary_op_works() {
        let texts = ["-10 + 20 rem 3", "foo | (3 + 10) | -1..+20", "A :: atom()"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(x, Type::BinaryOp(_)));
        }
    }

    #[test]
    fn bitstring_works() {
        let texts = ["<<>>", "<<_:10>>", "<<_:_*8>>", "<<_:8, _:_*4>>"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Bitstring(_))
            ));
        }
    }
}
