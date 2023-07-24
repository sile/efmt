use crate::format::{Format, Formatter};
use crate::items::symbols::DotSymbol;
use crate::items::Expr;
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};

/// ([Expr] `.`)*
#[derive(Debug, Clone, Span)]
pub struct Config {
    sof: Position,
    terms: Vec<Term>,
    eof: Position,
}

impl Config {
    pub fn exprs(&self) -> impl Iterator<Item = &Expr> {
        self.terms.iter().map(|t| &t.expr)
    }

    fn try_format_app_file(&self, fmt: &mut Formatter) -> bool {
        if self.terms.len() != 1 {
            return false;
        }

        if self.terms[0].expr.try_format_app_file(fmt) {
            self.terms[0].dot.format(fmt);
            true
        } else {
            false
        }
    }
}

impl Parse for Config {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.prev_token_end_position();
        let mut terms = Vec::new();
        while !ts.is_eof()? {
            terms.push(ts.parse()?);
        }
        let eof = ts.next_token_start_position()?;
        Ok(Self { sof, terms, eof })
    }
}

impl Format for Config {
    fn format(&self, fmt: &mut Formatter) {
        if self.try_format_app_file(fmt) {
            fmt.write_newline();
            return;
        }

        for term in &self.terms {
            term.format(fmt);
            fmt.write_newline();
        }
    }
}

#[derive(Debug, Clone, Span, Parse, Format)]
struct Term {
    expr: Expr,
    dot: DotSymbol,
}
