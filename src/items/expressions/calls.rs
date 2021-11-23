use crate::format::{Format, Formatter};
use crate::items::components::{Args, BinaryOpLike, Maybe, UnaryOpLike};
use crate::items::expressions::components::{BinaryOp, UnaryOp};
use crate::items::expressions::BaseExpr;
use crate::items::symbols::ColonSymbol;
use crate::items::Expr;
use crate::parse::{self, Parse, ResumeParse};
use crate::span::Span;

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
                module: Maybe::some((expr, ts.parse()?)),
                function: ts.parse()?,
                args: ts.parse()?,
            })
        } else {
            Ok(Self {
                module: Maybe::none_from_position(expr.start_position()),
                function: expr,
                args: ts.parse()?,
            })
        }
    }
}

/// [UnaryOp] [Expr]
#[derive(Debug, Clone, Span, Parse, Format)]
pub struct UnaryOpCallExpr(UnaryOpLike<UnaryOp, BaseExpr>);

impl UnaryOpCallExpr {
    pub(crate) fn item(&self) -> &BaseExpr {
        self.0.item()
    }
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
        self.0.left.get().is_atom_token()
            && matches!(self.0.op, BinaryOp::FloatDiv(_))
            && self.0.right.get().is_integer_token()
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
