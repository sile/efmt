use crate::format::{Format, Formatter};
use crate::items::components::{Args, BinaryOpLike, Maybe};
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
#[derive(Debug, Clone, Span, Parse)]
pub struct UnaryOpCallExpr {
    op: UnaryOp,
    expr: BaseExpr,
}

impl Format for UnaryOpCallExpr {
    fn format(&self, fmt: &mut Formatter) {
        let last = fmt.last_char().unwrap_or('\n');
        if !matches!(last, '\n' | ' ') {
            fmt.write_space();
        }

        self.op.format(fmt);
        if matches!(self.op, UnaryOp::Bnot(_) | UnaryOp::Not(_)) {
            fmt.write_space();
        }
        self.expr.format(fmt);
    }
}

impl UnaryOpCallExpr {
    pub(crate) fn item(&self) -> &BaseExpr {
        &self.expr
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
            self.0.left.format(fmt);

            // TODO
            // let multiline = fmt.has_newline_until(&self.0.right);
            fmt.write_space();
            self.0.op.format(fmt);

            // if multiline {
            //     fmt.write_newline();
            // } else {
            fmt.write_space();
            //}
            self.0.right.format(fmt);

            // TODO:
            // fmt.with_scoped_indent(|fmt| {
            //     fmt.set_indent(fmt.column());
            //     self.0.left.format(fmt);
            //     let mut op = &self.0.op;
            //     let mut right = &self.0.right;
            //     while let Some(next_group) = format_op_and_right(op, right, fmt) {
            //         op = next_group.0;
            //         right = next_group.1;
            //     }
            // });
        }
    }
}

// TODO: remove
// fn format_op_and_right<'a>(
//     op: &'a BinaryOp,
//     right: &'a Expr,
//     fmt: &mut Formatter,
// ) -> Option<(&'a BinaryOp, &'a Expr)> {
//     fmt.write_space();
//     op.format(fmt);
//     fmt.write_space();

//     let indent = op.indent();

//     if let FullExpr::BinaryOpCall(x) = &right.0 {
//         if indent != Indent::inherit() {
//             fmt.subregion(indent, |fmt| right.format(fmt));
//             None
//         } else if matches!(x.0.op, BinaryOp::Andalso(_) | BinaryOp::Orelse(_)) {
//             fmt.subregion(indent, |fmt| x.0.left.format(fmt));
//             Some((&x.0.op, &x.0.right))
//         } else {
//             let mut result = None;
//             fmt.subregion(indent, |fmt| {
//                 x.0.left.format(fmt);
//                 result = format_op_and_right(&x.0.op, &x.0.right, fmt)
//             });
//             result
//         }
//     } else {
//         fmt.subregion(indent, |fmt| right.format(fmt));
//         None
//     }
// }

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
            foo(A * 10 * B /
                1_0.0)"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn unary_op_call_works() {
        let texts = ["-1", "bnot Foo(1, +2, 3)", "- -7", "+ + -3"];
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
            1 + 2 + 3 + 4 + 5 +
            6"},
            indoc::indoc! {"
            {A, B, C} = {Foo,
                         bar,
                         baz} =
                    qux() /
                    quux() div 2"},
            indoc::indoc! {"
            [a,
             b ! fooooooooooooooo,
             c +
             barrrrrrrrrrrrrr,
             d = bazzzzzzzzzzzzzz,
             qux =
                 quuxxxxxxxxxxx]"},
            indoc::indoc! {"
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
