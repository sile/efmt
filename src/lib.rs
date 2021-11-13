pub mod format;
pub mod items;
pub mod parse;
pub mod span;

#[cfg(test)]
#[macro_export]
macro_rules! assert_format {
    ($text:expr, $item_type:ident) => {{
        use crate::items::styles::Child;
        use crate::FormatOptions;
        let formatted = FormatOptions::<Child<$item_type>>::new()
            .max_columns(20)
            .format_text($text)
            .unwrap_or_else(|e| format!("parse or format failed: {}", e));
        let expected = $text;
        similar_asserts::assert_str_eq!(formatted, expected);
    }};
}

#[derive(Debug, Clone)]
pub struct FormatOptions<T = crate::items::module::Module> {
    // TODO
    pub(crate) max_columns: usize,
    item: std::marker::PhantomData<T>,
}

impl<T> Default for FormatOptions<T> {
    fn default() -> Self {
        Self {
            max_columns: 50,
            item: std::marker::PhantomData,
        } // TODO: Change the default
    }
}

impl<T> FormatOptions<T>
where
    T: crate::parse::Parse + crate::format::Format,
{
    pub fn new() -> Self {
        Self::default()
    }

    pub fn max_columns(mut self, n: usize) -> Self {
        self.max_columns = n;
        self
    }

    pub fn format_text(&self, text: &str) -> anyhow::Result<String> {
        let tokenizer = erl_tokenize::Tokenizer::new(text.to_owned());
        format::<T>(tokenizer, self)
    }

    pub fn format_file<P: AsRef<std::path::Path>>(&self, path: P) -> anyhow::Result<String> {
        let text = std::fs::read_to_string(&path)?;
        let mut tokenizer = erl_tokenize::Tokenizer::new(text);
        tokenizer.set_filepath(path);
        format::<T>(tokenizer, self)
    }
}

pub fn format_file(path: impl AsRef<std::path::Path>) -> anyhow::Result<String> {
    FormatOptions::<crate::items::module::Module>::new().format_file(path)
}

pub fn format_text(text: &str) -> anyhow::Result<String> {
    FormatOptions::<crate::items::module::Module>::new().format_text(text)
}

fn format<T>(
    tokenizer: erl_tokenize::Tokenizer<String>,
    options: &FormatOptions<T>,
) -> anyhow::Result<String>
where
    T: crate::parse::Parse + crate::format::Format,
{
    let mut ts = crate::parse::TokenStream::new(tokenizer, Default::default());
    let module: T = ts.parse()?;
    let mut formatter = crate::format::Formatter::new(
        ts.text().to_owned(),
        ts.comments().clone(),
        ts.macros().clone(),
        options,
    );
    module.format(&mut formatter)?;
    Ok(formatter.finish())
}
