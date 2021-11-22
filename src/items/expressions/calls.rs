use crate::format::{Format, Formatter};
use crate::items::expressions::{BaseExpr, Expr};
use crate::items::generics::{Args, BinaryOpLike, BinaryOpStyle, Maybe, UnaryOpLike};
use crate::items::keywords;
use crate::items::symbols::{self, ColonSymbol};
use crate::items::tokens::Token;
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
    args: Args<Expr>,
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
    Not(keywords::NotKeyword),
    Bnot(keywords::BnotKeyword),
}

/// [Expr] [BinaryOp] [Expr]
#[derive(Debug, Clone, Span, Parse)]
pub struct BinaryOpCallExpr(BinaryOpLike<Expr, BinaryOp, Expr>);

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
    fn format(&self, fmt: &mut Formatter) {
        if self.is_name_and_arity() {
            // A workaround for some attributes such as `-export` and `-import`.
            self.0.left.format(fmt);
            self.0.op.format(fmt);
            self.0.right.format(fmt);
        } else {
            self.0.format(fmt);
        }
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

impl BinaryOpStyle for BinaryOp {
    fn indent_offset(&self) -> usize {
        if matches!(self, Self::Match(_)) {
            4
        } else {
            0
        }
    }

    fn allow_newline(&self) -> bool {
        !matches!(self, Self::Send(_))
    }

    fn should_pack(&self) -> bool {
        self.indent_offset() == 0
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
            %---10---|%---20---|
            (foo(Bar))(a,
                       b,
                       c())"},
            "foo:bar(baz)",
            "[]:bar(baz)",
            "foo:[](baz)",
            indoc::indoc! {"
            %---10---|%---20---|
            foo(A * 10 * B /
                1_0.0)"},
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
            indoc::indoc! {"
            %---10---|%---20---|
            1 + 2 + 3 + 4 + 5 +
            6"},
            indoc::indoc! {"
            %---10---|%---20---|
            {A, B, C} =
                {foo, bar,
                 baz} =
                    qux() /
                    quux() div 2"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
