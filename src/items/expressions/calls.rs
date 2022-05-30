use super::FullExpr;
use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::components::{Args, BinaryOpLike, BinaryOpStyle, Maybe, UnaryOpLike};
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
            fmt.subregion(Indent::inherit(), Newline::Never, |fmt| {
                self.0.left.format(fmt);
                let mut op = &self.0.op;
                let mut right = &self.0.right;
                while let Some(next_group) = format_op_and_right(op, right, fmt) {
                    op = next_group.0;
                    right = next_group.1;
                }
            });
        }
    }
}

fn format_op_and_right<'a>(
    op: &'a BinaryOp,
    right: &'a Expr,
    fmt: &mut Formatter,
) -> Option<(&'a BinaryOp, &'a Expr)> {
    fmt.add_space();
    op.format(fmt);
    fmt.add_space();

    let indent = op.indent();
    let newline = op.newline(right, fmt);

    if let FullExpr::BinaryOpCall(x) = &right.0 {
        if indent != Indent::inherit() {
            fmt.subregion(indent, newline, |fmt| right.format(fmt));
            None
        } else if matches!(x.0.op, BinaryOp::Andalso(_) | BinaryOp::Orelse(_)) {
            fmt.subregion(indent, newline, |fmt| x.0.left.format(fmt));
            Some((&x.0.op, &x.0.right))
        } else {
            let mut result = None;
            fmt.subregion(indent, newline, |fmt| {
                x.0.left.format(fmt);
                result = format_op_and_right(&x.0.op, &x.0.right, fmt)
            });
            result
        }
    } else {
        fmt.subregion(indent, newline, |fmt| right.format(fmt));
        None
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
            {A, B, C} = {Foo,
                         bar,
                         baz} =
                    qux() /
                    quux() div 2"},
            indoc::indoc! {"
            %---10---|%---20---|
            [a,
             b ! fooooooooooooooo,
             c +
             barrrrrrrrrrrrrr,
             d = bazzzzzzzzzzzzzz,
             qux =
                 quuxxxxxxxxxxx]"},
            indoc::indoc! {"
            %---10---|%---20---|
            foo =
                case bar of
                    baz ->
                        ok
                end"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
