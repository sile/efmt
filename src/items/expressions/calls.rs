use crate::format::{self, Format};
use crate::items::expressions::{BaseExpr, Expr};
use crate::items::generics::{Args2, BinaryOpLike2, IndentOffset, Maybe, UnaryOpLike};
use crate::items::keywords;
use crate::items::styles::RightSpace;
use crate::items::symbols::{self, ColonSymbol};
use crate::items::tokens::{Token, TokenStr};
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;
use erl_tokenize::values::{Keyword, Symbol};

/// `$MODULE`? `$NAME` `(` (`$ARG` `,`?)* `)`
///
/// - $MODULE: [Expr] `:`
/// - $NAME: [Expr]
/// - $ARG: [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionCallExpr {
    module: Maybe<(BaseExpr, ColonSymbol)>,
    function: BaseExpr,
    args: Args2<Expr>,
}

impl ResumeParse<(BaseExpr, bool)> for FunctionCallExpr {
    fn resume_parse(
        ts: &mut parse::TokenStream,
        (expr, is_remote): (BaseExpr, bool),
    ) -> parse::Result<Self> {
        if is_remote {
            Ok(Self {
                module: Maybe::from_item((expr, ts.parse()?)),
                function: ts.parse()?,
                args: ts.parse()?,
            })
        } else {
            Ok(Self {
                module: Maybe::from_position(expr.start_position()),
                function: expr,
                args: ts.parse()?,
            })
        }
    }
}

/// [UnaryOp] [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpCallExpr(UnaryOpLike<UnaryOp, BaseExpr>);

/// `+` | `-` | `not` | `bnot`
#[derive(Debug, Clone, Span, Parse, Format)]
pub enum UnaryOp {
    Plus(symbols::PlusSymbol),
    Minus(symbols::HyphenSymbol),
    Not(RightSpace<keywords::NotKeyword>),
    Bnot(RightSpace<keywords::BnotKeyword>),
}

impl TokenStr for UnaryOp {
    fn token_str(&self) -> &str {
        match self {
            Self::Plus(x) => x.token_str(),
            Self::Minus(x) => x.token_str(),
            Self::Not(x) => x.get().token_str(),
            Self::Bnot(x) => x.get().token_str(),
        }
    }
}

/// [Expr] [BinaryOp] [Expr]
#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpCallExpr(BinaryOpLike2<Expr, BinaryOp, Expr>);

impl ResumeParse<Expr> for BinaryOpCallExpr {
    fn resume_parse(ts: &mut parse::TokenStream, left: Expr) -> parse::Result<Self> {
        ts.resume_parse(left).map(Self)
    }
}

impl BinaryOpCallExpr {
    fn is_name_and_arity(&self) -> bool {
        self.0.left.is_atom_token()
            && matches!(self.0.op, BinaryOp::FloatDiv(_))
            && self.0.right.is_integer_token()
    }
}

impl Format for BinaryOpCallExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        if self.is_name_and_arity() {
            // A workaround for some attributes such as `-export` and `-import`.
            self.0.left.format(fmt)?;
            self.0.op.format(fmt)?;
            self.0.right.format(fmt)?;
        } else {
            self.0.format(fmt)?;
        }
        Ok(())
    }
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
        match ts.peek::<Token>() {
            Some(Token::Symbol(token)) => match token.value() {
                Symbol::Plus => ts.parse().map(Self::Plus),
                Symbol::Hyphen => ts.parse().map(Self::Minus),
                Symbol::Multiply => ts.parse().map(Self::Mul),
                Symbol::Slash => ts.parse().map(Self::FloatDiv),
                Symbol::PlusPlus => ts.parse().map(Self::PlusPlus),
                Symbol::MinusMinus => ts.parse().map(Self::MinusMinus),
                Symbol::Match => ts.parse().map(Self::Match),
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
            Some(Token::Keyword(token)) => match token.value() {
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
            None => Err(parse::Error::UnexpectedEof {
                position: ts.current_position(),
            }),
        }
    }
}

impl IndentOffset for BinaryOp {
    fn indent_offset(&self) -> usize {
        match self {
            Self::Plus(_)
            | Self::Minus(_)
            | Self::Mul(_)
            | Self::FloatDiv(_)
            | Self::PlusPlus(_)
            | Self::MinusMinus(_)
            | Self::Eq(_)
            | Self::ExactEq(_)
            | Self::NotEq(_)
            | Self::ExactNotEq(_)
            | Self::Less(_)
            | Self::LessEq(_)
            | Self::Greater(_)
            | Self::GreaterEq(_)
            | Self::IntDiv(_)
            | Self::Rem(_)
            | Self::Bor(_)
            | Self::Bxor(_)
            | Self::Band(_)
            | Self::Bsl(_)
            | Self::Bsr(_)
            | Self::Or(_)
            | Self::Xor(_)
            | Self::And(_)
            | Self::Andalso(_)
            | Self::Orelse(_) => 0,
            Self::Send(_) | Self::Match(_) => 4,
        }
    }
}

impl TokenStr for BinaryOp {
    fn token_str(&self) -> &str {
        match self {
            Self::Plus(x) => x.token_str(),
            Self::Minus(x) => x.token_str(),
            Self::Mul(x) => x.token_str(),
            Self::FloatDiv(x) => x.token_str(),
            Self::PlusPlus(x) => x.token_str(),
            Self::MinusMinus(x) => x.token_str(),
            Self::Eq(x) => x.token_str(),
            Self::ExactEq(x) => x.token_str(),
            Self::NotEq(x) => x.token_str(),
            Self::ExactNotEq(x) => x.token_str(),
            Self::Less(x) => x.token_str(),
            Self::LessEq(x) => x.token_str(),
            Self::Greater(x) => x.token_str(),
            Self::GreaterEq(x) => x.token_str(),
            Self::IntDiv(x) => x.token_str(),
            Self::Rem(x) => x.token_str(),
            Self::Bor(x) => x.token_str(),
            Self::Bxor(x) => x.token_str(),
            Self::Band(x) => x.token_str(),
            Self::Bsl(x) => x.token_str(),
            Self::Bsr(x) => x.token_str(),
            Self::Or(x) => x.token_str(),
            Self::Xor(x) => x.token_str(),
            Self::And(x) => x.token_str(),
            Self::Andalso(x) => x.token_str(),
            Self::Orelse(x) => x.token_str(),
            Self::Send(x) => x.token_str(),
            Self::Match(x) => x.token_str(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn function_call_works() {
        let texts = [
            "foo()",
            "Foo(1, 2, 3)",
            indoc::indoc! {"
            (foo(Bar))(a,
                       b,
                       c())"},
            "foo:bar(baz)",
            "[]:bar(baz)",
            "foo:[](baz)",
            indoc::indoc! {"
            foo(A *
                10 * B / 1_0.0)"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn unary_op_call_works() {
        let texts = ["-1", "bnot Foo(1, +2, 3)", "- -7", "+ +-3"];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn binary_op_call_works() {
        let texts = [
            "1 + 2",
            "1 - 2 * 3",
            // TODO
            indoc::indoc! {"
            %---10---|%---20---|
            {A, B, C} =
                {foo, bar, baz} =
                    qux() /
                    quux() div 2"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
