pub mod format;
pub mod items;
pub mod lex;
pub mod parse;
pub mod span;

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
    let mut lexer = crate::lex::Lexer::new(tokenizer);
    let module: T = lexer.parse()?;
    let mut formatter = crate::format::Formatter::new(
        lexer.text().to_owned(),
        lexer.comments().clone(),
        lexer.macros().clone(),
        options,
    );
    formatter.format_item(&module)?;
    Ok(formatter.finish())
}
