use crate::format::{Format, Formatter};
use crate::items::components::Element;
use crate::items::tokens::StringToken;
use crate::parse::{self, Parse};
use crate::span::{Position, Span};

/// [StringToken]+
#[derive(Debug, Clone)]
pub struct StringExpr(Vec<StringToken>);

impl StringExpr {
    pub(crate) fn tokens(&self) -> &[StringToken] {
        &self.0
    }
}

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
        fmt.with_scoped_indent(|fmt| {
            fmt.set_indent(fmt.column());
            for (i, item) in self.0.iter().enumerate() {
                item.format(fmt);
                if i + 1 < self.0.len() {
                    fmt.write_newline();
                }
            }
        });
    }
}

impl Element for StringExpr {
    fn is_packable(&self) -> bool {
        self.0.len() == 1
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
