use crate::format::{self, Format};
use crate::items::generics::MaybeRepeat;
use crate::items::tokens::StringToken;
use crate::parse::Parse;
use crate::span::Span;

#[derive(Debug, Clone, Span, Parse)]
pub struct StringExpr(MaybeRepeat<StringToken>);

impl Format for StringExpr {
    fn format(&self, fmt: &mut format::Formatter) -> format::Result<()> {
        fmt.with_subregion(
            format::RegionOptions::new().indent(format::IndentMode::CurrentColumn),
            |fmt| {
                for (i, item) in self.0.get().iter().enumerate() {
                    item.format(fmt)?;
                    if i + 1 < self.0.get().len() {
                        fmt.write_newline()?;
                    }
                }
                Ok(())
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::items::expressions::Expr;

    fn format(text: &str) -> String {
        crate::FormatOptions::<crate::items::styles::Child<Expr>>::new()
            .max_columns(20)
            .format_text(text)
            .expect("parse or format failed")
    }

    #[test]
    fn tuple_works() {
        let texts = [
            "\"foo\"",
            indoc::indoc! {r#"
                "foo"
                "bar"
                "baz""#},
            indoc::indoc! {r#"
                foo("bar"
                    "baz", qux)"#},
        ];
        for text in texts {
            assert_eq!(format(text), text);
        }
    }
}
