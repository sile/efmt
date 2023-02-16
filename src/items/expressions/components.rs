use crate::format::{Format, Formatter};
use crate::items::components::{Either, Guard, Maybe, NonEmptyItems, Params};
use crate::items::keywords;
use crate::items::symbols::{
    self, CommaSymbol, DoubleLeftArrowSymbol, DoubleVerticalBarSymbol, LeftArrowSymbol,
    MapMatchSymbol, RightArrowSymbol,
};
use crate::items::tokens::LexicalToken;
use crate::items::Expr;
use crate::parse::{self, Parse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

#[derive(Debug, Clone, Span, Parse)]
pub(crate) struct FunctionClause<Name, const BODY_INDENT: usize = 4> {
    name: Name,
    params: Params<Expr>,
    guard: Maybe<Guard<Expr>>,
    arrow: RightArrowSymbol,
    body: Body,
}

impl<Name: Format, const BODY_INDENT: usize> Format for FunctionClause<Name, BODY_INDENT> {
    fn format(&self, fmt: &mut Formatter) {
        let f = |fmt: &mut Formatter| {
            fmt.with_scoped_indent(|fmt| {
                let base_indent = fmt.indent();

                // 'Name'
                self.name.format(fmt);

                // 'Params'
                self.params.format(fmt);

                // 'Guard'
                if let Some(guard) = self.guard.get() {
                    let newline = fmt.has_newline_until(guard.conditions());
                    if newline {
                        fmt.set_indent(base_indent + BODY_INDENT - 2);
                        fmt.write_newline();
                    } else {
                        fmt.write_space();
                    }
                    guard.format(fmt);
                }
                fmt.write_space();

                // '->'
                let newline = fmt.has_newline_until(&self.body.end_position());
                self.arrow.format(fmt);
                if newline {
                    fmt.set_indent(base_indent + BODY_INDENT);
                    fmt.write_newline();
                } else {
                    fmt.write_space();
                }

                // 'Body'
                self.body.format(fmt);
            });
        };

        if self.contains_newline() {
            f(fmt);
        } else {
            fmt.with_single_line_mode(f);
        };
    }
}

/// ([Expr], `,`?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Expr, CommaSymbol>,
}

impl Body {
    pub(crate) fn exprs(&self) -> &[Expr] {
        self.exprs.items()
    }
}

impl Format for Body {
    fn format(&self, fmt: &mut Formatter) {
        self.exprs.format(fmt);
        fmt.set_next_comment_indent(fmt.indent());
    }
}

/// ((`$GENERATOR` | `$FILTER`) `,`?)+
/// - $GENERATOR: (`Expr` | `Expr` `:=` `Expr`) `<-` `Expr` | `Expr` `<=` `Expr`
/// - $FILTER: `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Qualifier(Either<Generator, Expr>);

#[derive(Debug, Clone, Span, Parse)]
pub struct MapGeneratorPattern {
    key: Expr,
    delimiter: MapMatchSymbol,
    value: Expr,
}

impl Format for MapGeneratorPattern {
    fn format(&self, fmt: &mut Formatter) {
        self.key.format(fmt);
        fmt.write_space();
        self.delimiter.format(fmt);
        fmt.write_space();
        self.value.format(fmt);
    }
}

#[derive(Debug, Clone, Span, Parse)]
struct Generator {
    pattern: Either<MapGeneratorPattern, Expr>,
    delimiter: GeneratorDelimiter,
    sequence: Expr,
}

impl Format for Generator {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            self.pattern.format(fmt);
            fmt.write_space();
            self.delimiter.format(fmt);
            fmt.write_space();
            fmt.set_indent(fmt.column());
            self.sequence.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct GeneratorDelimiter(Either<LeftArrowSymbol, DoubleLeftArrowSymbol>);

#[derive(Debug, Clone, Span, Parse)]
pub(crate) struct ComprehensionExpr<Open, Close> {
    open: Open,
    value: Expr,
    delimiter: DoubleVerticalBarSymbol,
    qualifiers: NonEmptyItems<Qualifier>,
    close: Close,
}

impl<Open: Format, Close: Format> Format for ComprehensionExpr<Open, Close> {
    fn format(&self, fmt: &mut Formatter) {
        self.open.format(fmt);
        fmt.with_scoped_indent(|fmt| {
            fmt.write_space();
            fmt.set_indent(fmt.column());

            self.value.format(fmt);

            if fmt.has_newline_until(&self.qualifiers) {
                fmt.write_newline();
            } else {
                fmt.write_space();
            }

            self.delimiter.format(fmt);
            fmt.write_space();
            fmt.set_indent(fmt.column());

            self.qualifiers.format(fmt);
        });
        fmt.write_space();
        self.close.format(fmt);
    }
}

/// `+` | `-` | `not` | `bnot`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Not(keywords::NotKeyword),
    Bnot(keywords::BnotKeyword),
}

#[derive(Debug, Clone, Span, Format)]
pub enum BinaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Mul(symbols::MultiplySymbol),
    FloatDiv(symbols::SlashSymbol),
    PlusPlus(symbols::PlusPlusSymbol),
    MinusMinus(symbols::MinusMinusSymbol),
    Match(symbols::MatchSymbol),
    MaybeMatch(symbols::MaybeMatchSymbol),
    Eq(symbols::EqSymbol),
    ExactEq(symbols::ExactEqSymbol),
    NotEq(symbols::NotEqSymbol),
    ExactNotEq(symbols::ExactNotEqSymbol),
    Less(symbols::LessSymbol),
    LessEq(symbols::LessEqSymbol),
    Greater(symbols::GreaterSymbol),
    GreaterEq(symbols::GreaterEqSymbol),
    Send(symbols::NotSymbol),
    IntDiv(keywords::DivKeyword),
    Rem(keywords::RemKeyword),
    Bor(keywords::BorKeyword),
    Bxor(keywords::BxorKeyword),
    Band(keywords::BandKeyword),
    Bsl(keywords::BslKeyword),
    Bsr(keywords::BsrKeyword),
    Or(keywords::OrKeyword),
    Xor(keywords::XorKeyword),
    And(keywords::AndKeyword),
    Andalso(keywords::AndalsoKeyword),
    Orelse(keywords::OrelseKeyword),
}

impl Parse for BinaryOp {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        match ts.peek::<LexicalToken>() {
            Some(LexicalToken::Symbol(token)) => match token.value() {
                Symbol::Plus => ts.parse().map(Self::Plus),
                Symbol::Hyphen => ts.parse().map(Self::Minus),
                Symbol::Multiply => ts.parse().map(Self::Mul),
                Symbol::Slash => ts.parse().map(Self::FloatDiv),
                Symbol::PlusPlus => ts.parse().map(Self::PlusPlus),
                Symbol::MinusMinus => ts.parse().map(Self::MinusMinus),
                Symbol::Match => ts.parse().map(Self::Match),
                Symbol::MaybeMatch => ts.parse().map(Self::MaybeMatch),
                Symbol::Eq => ts.parse().map(Self::Eq),
                Symbol::ExactEq => ts.parse().map(Self::ExactEq),
                Symbol::NotEq => ts.parse().map(Self::NotEq),
                Symbol::ExactNotEq => ts.parse().map(Self::ExactNotEq),
                Symbol::Less => ts.parse().map(Self::Less),
                Symbol::LessEq => ts.parse().map(Self::LessEq),
                Symbol::Greater => ts.parse().map(Self::Greater),
                Symbol::GreaterEq => ts.parse().map(Self::GreaterEq),
                Symbol::Not => ts.parse().map(Self::Send),
                _ => Err(parse::Error::unexpected_token(ts, token.into())),
            },
            Some(LexicalToken::Keyword(token)) => match token.value() {
                Keyword::Div => ts.parse().map(Self::IntDiv),
                Keyword::Rem => ts.parse().map(Self::Rem),
                Keyword::Bor => ts.parse().map(Self::Bor),
                Keyword::Bxor => ts.parse().map(Self::Bxor),
                Keyword::Band => ts.parse().map(Self::Band),
                Keyword::Bsl => ts.parse().map(Self::Bsl),
                Keyword::Bsr => ts.parse().map(Self::Bsr),
                Keyword::Or => ts.parse().map(Self::Or),
                Keyword::Xor => ts.parse().map(Self::Xor),
                Keyword::And => ts.parse().map(Self::And),
                Keyword::Andalso => ts.parse().map(Self::Andalso),
                Keyword::Orelse => ts.parse().map(Self::Orelse),
                _ => Err(parse::Error::unexpected_token(ts, token.into())),
            },
            Some(token) => Err(parse::Error::unexpected_token(ts, token)),
            None => Err(parse::Error::unexpected_eof(ts)),
        }
    }
}
