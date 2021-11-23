use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::expressions::Expr;
use crate::items::generics::{
    BinaryOpLike, BinaryOpStyle, Either, NonEmptyItems, Params, WithArrow, WithGuard,
};
use crate::items::symbols::{
    CommaSymbol, DoubleLeftArrowSymbol, DoubleVerticalBarSymbol, LeftArrowSymbol,
};
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

#[derive(Debug, Clone, Span, Parse, Format)]
pub struct Qualifier(Either<Generator, Expr>);

#[derive(Debug, Clone, Span, Parse, Format)]
struct Generator(BinaryOpLike<Expr, GeneratorDelimiter, Expr>);

#[derive(Debug, Clone, Span, Parse, Format)]
struct GeneratorDelimiter(Either<LeftArrowSymbol, DoubleLeftArrowSymbol>);

impl BinaryOpStyle for GeneratorDelimiter {
    fn indent_offset(&self) -> usize {
        4
    }

    fn allow_newline(&self) -> bool {
        false
    }

    fn should_pack(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone, Span, Parse)]
pub struct ComprehensionExpr<Open, Close> {
    open: Open,
    body: BinaryOpLike<Expr, ComprehensionDelimiter, NonEmptyItems<Qualifier>>,
    close: Close,
}

impl<Open: Format, Close: Format> Format for ComprehensionExpr<Open, Close> {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            self.open.format(fmt);
            fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
                self.body.format(fmt)
            });
            self.close.format(fmt);
        });
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct ComprehensionDelimiter(DoubleVerticalBarSymbol);

impl BinaryOpStyle for ComprehensionDelimiter {
    fn indent_offset(&self) -> usize {
        4
    }

    fn parent_indent(&self) -> bool {
        true
    }

    fn allow_newline(&self) -> bool {
        true
    }

    fn should_pack(&self) -> bool {
        false
    }
}
