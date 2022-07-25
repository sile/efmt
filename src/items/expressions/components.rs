use crate::format::{Format, Formatter, Indent};
use crate::items::components::{
    BinaryOpStyle, Either, NonEmptyItems, Params, WithArrow, WithGuard,
};
use crate::items::keywords;
use crate::items::symbols::{
    self, CommaSymbol, DoubleLeftArrowSymbol, DoubleVerticalBarSymbol, LeftArrowSymbol,
};
use crate::items::tokens::LexicalToken;
use crate::items::Expr;
use crate::parse::{self, Parse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

#[derive(Debug, Clone, Span, Parse, Format)]
pub(crate) struct FunctionClause<Name, const OFFSET: usize = 4> {
    name: Name,
    params: WithArrow<WithGuard<Params<Expr>, Expr>>,
    body: Body<OFFSET>,
}

impl<Name: Format, const OFFSET: usize> FunctionClause<Name, OFFSET> {
    pub fn format_maybe_one_line_body(&self, fmt: &mut Formatter) {
        self.name.format(fmt);
        self.params.format(fmt);
        fmt.subregion(Indent::Offset(OFFSET), |fmt| self.body.exprs.format(fmt));
    }

    pub fn body(&self) -> &Body<OFFSET> {
        &self.body
    }
}

/// ([Expr], `,`?)+
#[derive(Debug, Clone, Span, Parse)]
pub struct Body<const OFFSET: usize = 4> {
    exprs: NonEmptyItems<Expr, CommaSymbol>,
}

impl<const OFFSET: usize> Body<OFFSET> {
    pub(crate) fn exprs(&self) -> &[Expr] {
        self.exprs.items()
    }
}

impl<const OFFSET: usize> Format for Body<OFFSET> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::Offset(OFFSET), |fmt| {
            self.exprs.format_multi_line(fmt)
        });
    }
}

/// ((`$GENERATOR` | `$FILTER`) `,`?)+
/// - $GENERATOR: `Expr` (`<-` | `<=`) `Expr`
/// - $FILTER: `Expr`
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Qualifier(Either<Generator, Expr>);

#[derive(Debug, Clone, Span, Parse)]
struct Generator {
    pattern: Expr,
    delimiter: GeneratorDelimiter,
    sequence: Expr,
}

impl Format for Generator {
    fn format(&self, fmt: &mut Formatter) {
        self.pattern.format(fmt);
        fmt.add_space();
        fmt.subregion(Indent::CurrentColumnOrOffset(4), |fmt| {
            self.delimiter.format(fmt);
            fmt.add_space();
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
        fmt.subregion(Indent::CurrentColumn, |fmt| {
            self.open.format(fmt);
            fmt.add_space();
            fmt.subregion(Indent::CurrentColumn, |fmt| {
                self.value.format(fmt);
                fmt.add_space();
                fmt.subregion(Indent::inherit(), |fmt| {
                    self.delimiter.format(fmt);
                    fmt.add_space();
                    fmt.subregion(Indent::CurrentColumn, |fmt| {
                        self.qualifiers.format(fmt);
                    });
                });
            });
            fmt.add_space();
            self.close.format(fmt);
        });
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

impl BinaryOpStyle<Expr> for BinaryOp {
    fn indent(&self) -> Indent {
        if matches!(self, Self::Match(_) | Self::MaybeMatch(_)) {
            Indent::Offset(4)
        } else {
            Indent::inherit()
        }
    }
}
