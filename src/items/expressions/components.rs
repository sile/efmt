use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::expressions::Expr;
use crate::items::generics::{NonEmptyItems, Params, WithArrow, WithGuard};
use crate::items::symbols::CommaSymbol;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct FunctionClause<Name> {
    name: Name,
    params: WithArrow<WithGuard<Params<Expr>, Expr>>,
    body: Body,
}

impl<Name: Format> FunctionClause<Name> {
    pub fn format_maybe_one_line_body(&self, fmt: &mut Formatter) {
        self.name.format(fmt);
        self.params.format(fmt);
        fmt.subregion(
            Indent::Offset(4),
            Newline::if_too_long_or_multi_line_parent(),
            |fmt| self.body.exprs.format(fmt),
        );
    }

    pub fn body(&self) -> &Body {
        &self.body
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct Body {
    exprs: NonEmptyItems<Expr, CommaSymbol>,
}

impl Body {
    pub fn exprs(&self) -> &[Expr] {
        self.exprs.items()
    }
}

impl Format for Body {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::Offset(4), Newline::Always, |fmt| {
            self.exprs.format_multi_line(fmt)
        });
    }
}
