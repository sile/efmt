use crate::format::{Format, Formatter};
use crate::items::components::{Args, Maybe};
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
#[derive(Debug, Clone, Span, Parse)]
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

impl Format for FunctionCallExpr {
    fn format(&self, fmt: &mut Formatter) {
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            self.module.format(fmt);
            self.function.format(fmt);
            self.args.format(fmt);
        });
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
        if !matches!(last, '\n' | ' ' | '(' | '[' | '{') {
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
pub struct BinaryOpCallExpr {
    left: Expr,
    op: BinaryOp,
    right: Expr,
}

impl ResumeParse<Expr> for BinaryOpCallExpr {
    fn resume_parse(ts: &mut parse::TokenStream, left: Expr) -> parse::Result<Self> {
        Ok(Self {
            left,
            op: ts.parse()?,
            right: ts.parse()?,
        })
    }
}

impl BinaryOpCallExpr {
    fn is_name_and_arity(&self) -> bool {
        self.left.get().is_atom_token()
            && matches!(self.op, BinaryOp::FloatDiv(_))
            && self.right.get().is_integer_token()
    }

    fn format_binary_op(&self, fmt: &mut Formatter, mut update_indent: bool) {
        self.left.format(fmt);
        fmt.write_space();

        update_indent |= matches!(
            self.op,
            BinaryOp::Match(_) | BinaryOp::MaybeMatch(_) | BinaryOp::Send(_)
        );
        let multiline = fmt.has_newline_until(&self.right);

        self.op.format(fmt);
        if multiline {
            if update_indent {
                fmt.set_indent(fmt.indent() + 4);
                update_indent = false;
            }
            fmt.write_newline();
        } else {
            fmt.write_space();
        }

        if let Some(right) = self.right.as_binary_op() {
            right.format_binary_op(fmt, update_indent);
        } else {
            self.right.format(fmt);
        }
    }
}

impl Format for BinaryOpCallExpr {
    fn format(&self, fmt: &mut Formatter) {
        if self.is_name_and_arity() {
            // A workaround for some attributes such as `-export` and `-import`.
            self.left.format(fmt);
            self.op.format(fmt);
            self.right.format(fmt);
        } else {
            fmt.with_scoped_indent(|fmt| {
                self.format_binary_op(fmt, false);
            });
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
            foo(A * 10 * B /
                1_0.0)"},
            indoc::indoc! {"
            foo(a,
                b,
                c)"},
            indoc::indoc! {"
            foo(
              a, b, c)"},
            indoc::indoc! {"
            foo(
              a,
              b,
              c)"},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }

    #[test]
    fn unary_op_call_works() {
        let texts = ["-1", "bnot Foo(1, +2, 3)", "- -7", "+ + -3", "foo(-1)"];
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
