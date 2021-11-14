use crate::format::{self, Format};
use crate::items::generics::MaybeRepeat;
use crate::items::tokens::StringToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub struct StringExpr(MaybeRepeat<StringToken>);

impl Format for StringExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.subregion().current_column_as_indent().enter(|fmt| {
            for (i, item) in self.0.get().iter().enumerate() {
                item.format(fmt)?;
                if i + 1 < self.0.get().len() {
                    fmt.write_newline()?;
                }
            }
            Ok(())
        })
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
                foo("bar"
                    "baz",
                    qux)"#},
        ];
        for text in texts {
            crate::assert_format!(text, Expr);
        }
    }
}
