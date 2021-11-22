use crate::format::{Format, Formatter, Indent, Newline};
use crate::items::tokens::StringToken;
use crate::parse::{self, Parse};
use crate::span::{Position, Span};

/// [StringToken]+
#[derive(Debug, Clone)]
pub struct StringExpr(Vec<StringToken>);

impl Span for StringExpr {
    fn start_position(&self) -> Position {
        self.0[0].start_position()
    }

    fn end_position(&self) -> Position {
        self.0[self.0.len() - 1].end_position()
    }
}

impl Parse for StringExpr {
    fn parse(ts: &mut parse::TokenStream) -> parse::Result<Self> {
        let mut items = vec![ts.parse()?];
        while let Ok(item) = ts.parse() {
            items.push(item);
        }
        Ok(Self(items))
    }
}

impl Format for StringExpr {
    fn format(&self, fmt: &mut Formatter) {
        fmt.subregion(Indent::CurrentColumn, Newline::Never, |fmt| {
            for (i, item) in self.0.iter().enumerate() {
                item.format(fmt);
                if i + 1 < self.0.len() {
                    fmt.add_newline();
                }
            }
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::items::expressions::Expr;

    #[test]
    fn string_works() {
        let texts = [
            "\"foo\"",
            indoc::indoc! {r#"
            "foo"
            "bar"
            "baz""#},
            indoc::indoc! {r#"
            %---10---|%---20---|
            foo("bar"
                "baz",
                qux)"#},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
