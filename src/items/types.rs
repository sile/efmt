use crate::format::{self, Format};
use crate::items::generics::{Either, Elements, Items, Maybe, Parenthesized};
use crate::items::keywords::{
    BandKeyword, BnotKeyword, BorKeyword, BslKeyword, BsrKeyword, BxorKeyword, DivKeyword,
    FunKeyword, RemKeyword,
};
use crate::items::styles::{RightSpace, Space};
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

#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpType {
    left: NonLeftRecursiveType,
    op: BinaryOp,
    right: Type,
}

impl Format for BinaryOpType {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.format_item(&self.left)?;
        fmt.format_item(&self.op)?;
        if fmt.multiline_mode().is_recommended() && matches!(self.op, BinaryOp::Union(_)) {
            fmt.needs_newline();
        }
        fmt.format_item(&self.right)?;
        Ok(())
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum BinaryOp {
    Mul(Space<MultiplySymbol>),
    Div(Space<DivKeyword>),
    Rem(Space<RemKeyword>),
    Band(Space<BandKeyword>),
    Plus(Space<PlusSymbol>),
    Minus(Space<HyphenSymbol>),
    Bor(Space<BorKeyword>),
    Bxor(Space<BxorKeyword>),
    Bsl(Space<BslKeyword>),
    Bsr(Space<BsrKeyword>),
    Union(Space<VerticalBarSymbol>),
    Annotation(Space<DoubleColonSymbol>),
    Range(DoubleDotSymbol),
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
    Bnot(Space<BnotKeyword>),
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionType {
    fun: Space<FunKeyword>,
    params_and_return: Parenthesized<Maybe<(FunctionParams, (Space<RightArrowSymbol>, Type))>>,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub enum FunctionParams {
    Any(Parenthesized<TripleDotSymbol>),
    Params(Parenthesized<Elements<Type>>),
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
    item: Maybe<(Type, Maybe<(RightSpace<CommaSymbol>, TripleDotSymbol)>)>,
    close: CloseSquareSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct TupleType {
    open: OpenBraceSymbol,
    items: Elements<Type>,
    close: CloseBraceSymbol,
}

pub type MapItem = (
    Type,
    (Space<Either<DoubleRightArrowSymbol, MapMatchSymbol>>, Type),
);

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct MapType {
    sharp: SharpSymbol,
    open: OpenBraceSymbol,
    items: Elements<MapItem>,
    close: CloseBraceSymbol,
}

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct RecordType {
    sharp: SharpSymbol,
    name: AtomToken,
    open: OpenBraceSymbol,
    fields: Elements<(AtomToken, (Space<MatchSymbol>, Type))>,
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
    BinaryAndUnit(
        Box<(
            BitstringBinarySize,
            (RightSpace<CommaSymbol>, BitstringUnitSize),
        )>,
    ),
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
    use crate::items::styles::Child;
    use crate::parse::parse_text;
    use crate::FormatOptions;

    fn format(text: &str) -> String {
        FormatOptions::<Child<Type>>::new()
            .max_columns(20)
            .format_text(text)
            .expect("parse or format failed")
    }

    #[test]
    fn parse_mfargs_works() {
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
    fn format_mfargs_works() {
        assert_eq!(format("foo()"), "foo()");
        assert_eq!(format("foo:bar(A,1)"), "foo:bar(A, 1)");
        assert_eq!(
            format("foo:bar(A,1,baz(),qux())"),
            concat!(
                "foo:bar(A,\n",
                "        1,\n",
                "        baz(),\n",
                "        qux())"
            )
        );
    }

    #[test]
    fn parse_list_works() {
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
    fn format_list_works() {
        assert_eq!(format("[]"), "[]");
        assert_eq!(format("[foo()]"), "[foo()]");
        assert_eq!(format("[10,...]"), "[10, ...]");
    }

    #[test]
    fn parse_tuple_works() {
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
    fn format_tuple_works() {
        assert_eq!(format("{}"), "{}");
        assert_eq!(format("{foo()}"), "{foo()}");
        assert_eq!(
            format("{foo(),bar(),[baz()]}"),
            concat!(
                "{foo(),\n", //
                " bar(),\n",
                " [baz()]}"
            )
        );
    }

    #[test]
    fn parse_map_works() {
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
    fn format_map_works() {
        let expected = [
            "#{}",
            "#{atom() := integer()}",
            concat!(
                "#{atom() := {aaa,\n", //
                "             bbb,\n",
                "             ccc}}"
            ),
            concat!(
                "#{a => b,\n", //
                "  1 := 2,\n",
                "  atom() := atom()}"
            ),
        ];
        for text in expected {
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn parse_record_works() {
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
    fn format_record_works() {
        let texts = [
            "#foo{}",
            "#foo{bar = integer()}",
            concat!(
                "#foo{bar = b,\n", //
                "     baz = 2}"
            ),
        ];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn parse_function_works() {
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
    fn format_function_works() {
        let texts = [
            "fun ()",
            "fun ((...) -> atom())",
            "fun (() -> integer())",
            concat!(
                "fun ((A,\n",
                "      b,\n",
                "      $c,\n",
                "      {foo()}) -> tuple())"
            ),
        ];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn parse_unary_op_works() {
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
    fn format_unary_op_works() {
        let texts = ["-10", "+10", "bnot 100"];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn parse_binary_op_works() {
        let texts = ["-10 + 20 rem 3", "foo | (3 + 10) | -1..+20", "A :: atom()"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(x, Type::BinaryOp(_)));
        }
    }

    #[test]
    fn format_binary_op_works() {
        let texts = [
            "-10 + 20 rem 3",
            concat!(
                "foo |\n", //
                "(3 + 10) |\n",
                "-1..+20"
            ),
            "A :: atom()",
        ];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }

    #[test]
    fn parse_bitstring_works() {
        let texts = ["<<>>", "<<_:10>>", "<<_:_*8>>", "<<_:8, _:_*4>>"];
        for text in texts {
            let x = parse_text(text).unwrap();
            assert!(matches!(
                x,
                Type::NonLeftRecursive(NonLeftRecursiveType::Bitstring(_))
            ));
        }
    }

    #[test]
    fn format_bitstring_works() {
        let texts = ["<<>>", "<<_:10>>", "<<_:_*8>>", "<<_:8, _:_*4>>"];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }
}
