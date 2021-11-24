use crate::items::components::Either;
use crate::items::forms::{DefineDirective, IncludeDirective};
use crate::items::macros::{Macro, MacroName};
use crate::items::symbols::{OpenParenSymbol, QuestionSymbol};
use crate::items::tokens::{
    AtomToken, CharToken, CommentKind, CommentToken, FloatToken, IntegerToken, KeywordToken,
    LexicalToken, StringToken, SymbolToken, VariableToken,
};
use crate::items::Module;
use crate::parse::{Error, Parse, Result, ResumeParse};
use crate::span::{Position, Span as _};
use erl_tokenize::values::Symbol;
use erl_tokenize::{PositionRange as _, Tokenizer};
use std::collections::{BTreeMap, HashSet};
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Debug, Default, Clone)]
pub struct TokenStreamOptions {
    include_dirs: Vec<PathBuf>,
    include_cache_dir: Option<PathBuf>,
}

impl TokenStreamOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn include_dirs(mut self, dirs: Vec<PathBuf>) -> Self {
        self.include_dirs = dirs;
        self
    }

    pub fn include_cache_dir(mut self, dir: PathBuf) -> Self {
        self.include_cache_dir = Some(dir);
        self
    }
}

#[derive(Debug)]
pub struct TokenStream {
    tokenizer: Tokenizer<String>,
    tokens: Vec<LexicalToken>,
    current_token_index: usize,
    comments: BTreeMap<Position, CommentToken>,
    macros: BTreeMap<Position, Macro>,
    macro_defines: BTreeMap<MacroDefineKey, MacroDefine>,
    missing_macros: HashSet<String>,
    known_replacement: HashSet<(usize, Vec<LexicalToken>)>,
    included: HashSet<String>,
    parsing_macro_replacement: bool,
    parsing_macro_arg: bool,
    parsing_tokens: bool,
    options: TokenStreamOptions,
    text: Arc<String>,
    path: Option<Arc<PathBuf>>,
    last_parse_error: Option<Error>,
}

impl TokenStream {
    pub fn new(tokenizer: Tokenizer<String>, options: TokenStreamOptions) -> Self {
        let text = Arc::new(tokenizer.text().to_owned());
        let path = tokenizer
            .next_position()
            .filepath()
            .map(|p| Arc::new(p.to_owned()));
        Self {
            tokenizer,
            tokens: Vec::new(),
            current_token_index: 0,
            comments: BTreeMap::new(),
            macros: BTreeMap::new(),
            macro_defines: BTreeMap::new(),
            missing_macros: HashSet::new(),
            known_replacement: HashSet::new(),
            included: HashSet::new(),
            parsing_macro_replacement: false,
            parsing_macro_arg: false,
            parsing_tokens: false,
            options,
            text,
            path,
            last_parse_error: None,
        }
    }

    pub fn parse_tokens<T: Parse>(&mut self, tokens: Vec<LexicalToken>) -> Result<T> {
        let old_tokens = std::mem::replace(&mut self.tokens, tokens);
        let old_index = self.current_token_index;
        let old_last_parse_error = self.last_parse_error.take();
        self.current_token_index = 0;

        self.parsing_tokens = true;
        let result = self.parse();
        self.parsing_tokens = false;

        let _ = std::mem::replace(&mut self.tokens, old_tokens);
        self.current_token_index = old_index;
        self.last_parse_error = old_last_parse_error;

        result
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        let index = self.current_token_index;
        let result = T::parse(self);
        if let Err(e) = &result {
            self.current_token_index = index;
            if self
                .last_parse_error
                .as_ref()
                .map_or(true, |e0| e0.position() < e.position())
            {
                self.last_parse_error = Some(e.clone());
            }
        } else {
            let succeeded_position = self.prev_token_end_position();
            if self
                .last_parse_error
                .as_ref()
                .map_or(false, |e| e.position() < succeeded_position)
            {
                self.last_parse_error = None;
            }
        }
        result
    }

    pub fn take_last_error(&mut self) -> Option<Error> {
        self.last_parse_error.take()
    }

    pub fn resume_parse<T, A>(&mut self, args: A) -> Result<T>
    where
        T: ResumeParse<A>,
    {
        let index = self.current_token_index;
        let result = T::resume_parse(self, args);
        if result.is_err() {
            self.current_token_index = index;
        }
        result
    }

    pub fn peek<T: Parse>(&mut self) -> Option<T> {
        let index = self.current_token_index;
        let result = self.parse::<T>().ok();
        self.current_token_index = index;
        result
    }

    pub fn text(&self) -> Arc<String> {
        Arc::clone(&self.text)
    }

    pub fn filepath(&self) -> Option<Arc<PathBuf>> {
        self.path.clone()
    }

    pub fn comments(&self) -> &BTreeMap<Position, CommentToken> {
        &self.comments
    }

    pub fn macros(&self) -> &BTreeMap<Position, Macro> {
        &self.macros
    }

    pub fn is_eof(&mut self) -> Result<bool> {
        let index = self.current_token_index;
        let eof = self.next().transpose()?.is_none();
        self.current_token_index = index;
        Ok(eof)
    }

    pub fn enter_macro_replacement<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.parsing_macro_replacement = true;
        let result = f(self);
        self.parsing_macro_replacement = false;
        result
    }

    fn enter_macro_arg<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        self.parsing_macro_arg = true;
        let result = f(self);
        self.parsing_macro_arg = false;
        result
    }

    pub fn prev_token_end_position(&self) -> Position {
        if let Some(i) = self.current_token_index.checked_sub(1) {
            self.tokens[i].end_position()
        } else {
            self.tokenizer.next_position().into()
        }
    }

    pub fn next_token_start_position(&mut self) -> Result<Position> {
        let index = self.current_token_index;
        if index == self.tokens.len() && self.is_eof()? {
            Ok(self.tokenizer.next_position().into())
        } else {
            Ok(self.tokens[index].start_position())
        }
    }

    fn read_token(&mut self) -> Result<Option<LexicalToken>> {
        if let Some(token) = self.tokens.get(self.current_token_index).cloned() {
            self.current_token_index += 1;

            if !self.parsing_macro_replacement {
                // TODO: note comment
                match &token {
                    LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
                        if !self.parsing_macro_arg {
                            return self.expand_macro_and_read_token();
                        }
                    }
                    _ => {}
                }
            }

            return Ok(Some(token));
        }
        if self.parsing_tokens {
            return Ok(None);
        }

        while let Some(token) = self
            .tokenizer
            .next()
            .transpose()
            .map_err(|e| Error::tokenize_error(self, e))?
        {
            let start_position = Position::from(token.start_position());
            let end_position = Position::from(token.end_position());
            let token: LexicalToken = match token {
                erl_tokenize::Token::Whitespace(_) => {
                    continue;
                }
                erl_tokenize::Token::Comment(x) => {
                    let is_trailing = self.tokens.last().map_or(false, |y| {
                        y.start_position().line() == x.start_position().line()
                    });
                    let kind = if is_trailing {
                        CommentKind::Trailing
                    } else {
                        CommentKind::Post
                    };
                    self.comments.insert(
                        start_position,
                        CommentToken::new(kind, start_position, end_position),
                    );
                    continue;
                }
                erl_tokenize::Token::Symbol(x) => {
                    SymbolToken::new(x.value(), start_position, end_position).into()
                }
                erl_tokenize::Token::Atom(x) => {
                    AtomToken::new(x.value(), start_position, end_position).into()
                }
                erl_tokenize::Token::Char(_) => CharToken::new(start_position, end_position).into(),
                erl_tokenize::Token::Float(_) => {
                    FloatToken::new(start_position, end_position).into()
                }
                erl_tokenize::Token::Integer(_) => {
                    IntegerToken::new(start_position, end_position).into()
                }
                erl_tokenize::Token::Keyword(x) => {
                    KeywordToken::new(x.value(), start_position, end_position).into()
                }
                erl_tokenize::Token::String(x) => {
                    StringToken::new(x.value(), start_position, end_position).into()
                }
                erl_tokenize::Token::Variable(x) => {
                    VariableToken::new(x.value(), start_position, end_position).into()
                }
            };
            self.tokens.push(token.clone());
            self.current_token_index += 1;

            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
                    if !self.parsing_macro_arg {
                        return self.expand_macro_and_read_token();
                    }
                }
                LexicalToken::Symbol(x) if x.value() == Symbol::Hyphen => {
                    let index = self.current_token_index;
                    self.try_handle_directives()?;
                    self.current_token_index = index;
                }
                _ => {}
            }

            return Ok(Some(token));
        }

        Ok(None)
    }

    fn is_macro_defined(&self, name: &str) -> (bool, bool) {
        let key = MacroDefineKey::new(name.to_owned(), None);
        let mut without_args = false;
        let mut with_args = false;
        for (x, _) in self.macro_defines.range(key..) {
            if x.name != name {
                break;
            }
            if x.arity.is_none() {
                without_args = true;
            } else {
                with_args = true;
            }
        }
        (without_args, with_args)
    }

    fn expand_macro_and_read_token(&mut self) -> Result<Option<LexicalToken>> {
        let macro_name: MacroName = self.parse()?;
        self.expand_macro(macro_name)?;
        self.read_token()
    }

    fn expand_macro(&mut self, macro_name: MacroName) -> Result<()> {
        match self.is_macro_defined(macro_name.value()) {
            (false, false) => self.expand_unknown_macro(macro_name),
            (true, false) => {
                let key = MacroDefineKey::new(macro_name.value().to_owned(), None);
                self.expand_macro_without_args(
                    macro_name,
                    self.macro_defines[&key].replacement.clone(),
                )
            }
            (true, true) if self.peek::<OpenParenSymbol>().is_none() => {
                let key = MacroDefineKey::new(macro_name.value().to_owned(), None);
                self.expand_macro_without_args(
                    macro_name,
                    self.macro_defines[&key].replacement.clone(),
                )
            }
            (_, _) => self.expand_macro_with_args(macro_name),
        }
    }

    fn expand_macro_with_args(&mut self, macro_name: MacroName) -> Result<()> {
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();
        let question = QuestionSymbol::new(start_position);

        let r#macro: Macro =
            self.enter_macro_arg(|ts| ts.resume_parse((question, macro_name.clone(), true)))?;
        let arity = r#macro.arity();
        assert!(arity.is_some());

        let key = MacroDefineKey::new(macro_name.value().to_owned(), arity);
        if let Some(define) = self.macro_defines.get(&key).cloned() {
            let variables = define.variables.as_ref().map(|x| x.to_owned());

            let replacement = r#macro.expand(variables, define.replacement);
            self.replace_tokens(start_index, replacement);
            self.macros.entry(start_position).or_insert(r#macro);
        } else {
            self.expand_unknown_macro(macro_name)?;
        }

        Ok(())
    }

    fn expand_macro_without_args(
        &mut self,
        macro_name: MacroName,
        replacement: Vec<LexicalToken>,
    ) -> Result<()> {
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();
        let question = QuestionSymbol::new(start_position);
        let r#macro: Macro = self.resume_parse((question, macro_name, false))?;

        let replacement = r#macro.expand(None, replacement);
        self.replace_tokens(start_index, replacement);
        self.macros.entry(start_position).or_insert(r#macro);

        Ok(())
    }

    fn expand_unknown_macro(&mut self, macro_name: MacroName) -> Result<()> {
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();

        if let Some(replacement) = get_predefined_macro(macro_name.value(), start_position) {
            self.expand_macro_without_args(macro_name, replacement)
        } else if self.parsing_macro_replacement {
            log::debug!(
                "Found an undefined macro {:?} in a macro replacement.",
                macro_name.value()
            );
            Ok(())
        } else {
            if !self.missing_macros.contains(macro_name.value()) {
                // TODO: consider arity
                log::warn!(
                    "The macro {:?} is not defined. 'EFMT_DUMMY' atom is used instead.",
                    macro_name.value()
                );
                self.missing_macros.insert(macro_name.value().to_owned());
            }
            self.expand_macro_without_args(
                macro_name,
                vec![LexicalToken::from(dummy_atom(start_position))],
            )
        }
    }

    fn replace_tokens(&mut self, start_index: usize, mut replacement: Vec<LexicalToken>) {
        if !replacement.is_empty()
            && !self
                .known_replacement
                .insert((start_index, replacement.clone()))
        {
            log::warn!(
                "A circular macro was detected. It was replaced with a dummy atom 'EFMT_DUMMY'."
            );
            let start_position = self.tokens[start_index].start_position();
            replacement = vec![LexicalToken::from(dummy_atom(start_position))];
        }

        let unread_tokens = self.tokens.split_off(self.current_token_index);
        self.tokens.truncate(start_index);
        self.tokens.extend(replacement);
        self.tokens.extend(unread_tokens);
        self.current_token_index = start_index;
    }

    fn try_handle_directives(&mut self) -> Result<()> {
        self.current_token_index -= 1;
        let result: Result<Either<DefineDirective, IncludeDirective>> = self.parse();
        match result {
            Ok(Either::A(x)) => {
                let name = x.macro_name().to_owned();
                let define: MacroDefine = x.into();
                let key = MacroDefineKey::new(name, define.arity());
                self.macro_defines.insert(key, define);
            }
            Ok(Either::B(x)) => {
                self.handle_include(x);
            }
            Err(_) => {}
        }
        Ok(())
    }

    fn try_load_macro_defines_from_cache(
        &self,
        include_path: &str,
    ) -> Option<BTreeMap<MacroDefineKey, MacroDefine>> {
        // TODO: check file mtime
        if let Some(cache_path) = self
            .options
            .include_cache_dir
            .as_ref()
            .map(|x| x.join(include_path))
        {
            // TODO: improve error handling
            std::fs::File::open(&cache_path)
                .map_err(|e| {
                    log::debug!("cache file open error ({:?}): {}", cache_path, e);
                    e
                })
                .ok()
                .map(BufReader::new)
                .and_then(|file| {
                    serde_json::from_reader(file)
                        .map(|entries: Vec<(String, MacroDefine)>| {
                            entries
                                .into_iter()
                                .map(|(name, define)| {
                                    (MacroDefineKey::new(name, define.arity()), define)
                                })
                                .collect()
                        })
                        .map_err(|e| {
                            log::warn!("deserialize error ({:?}): {}", cache_path, e);
                            e
                        })
                        .ok()
                })
        } else {
            log::debug!("[TODO] fail1");
            None
        }
    }

    fn try_save_macro_defines_into_cache(
        &self,
        include_path: &str,
        macro_defines: &BTreeMap<MacroDefineKey, MacroDefine>,
    ) {
        if let Some(cache_path) = self
            .options
            .include_cache_dir
            .as_ref()
            .map(|x| x.join(include_path))
        {
            // TODO: error handling
            let _ = tempfile::NamedTempFile::new()
                .ok()
                .map(BufWriter::new)
                .map(|mut file| {
                    let entries = macro_defines
                        .iter()
                        .map(|(k, v)| (&k.name, v))
                        .collect::<Vec<_>>();
                    let saved = serde_json::to_writer(&mut file, &entries)
                        .map_err(|e| {
                            log::warn!("serialization error: {}", e);
                            e
                        })
                        .ok()
                        .and_then(|_| {
                            cache_path.parent().and_then(|p| {
                                std::fs::create_dir_all(p)
                                    .map_err(|e| {
                                        log::warn!("create_dir_all({:?}) error: {}", p, e);
                                        e
                                    })
                                    .ok()
                            })
                        })
                        .is_some();
                    if saved {
                        if let Err(e) = file.into_inner().expect("TODO").persist(&cache_path) {
                            log::warn!("cannot save cache file {:?}: {}", cache_path, e);
                        }
                        log::debug!(
                            "Saved a include cache for {:?} into {:?}",
                            include_path,
                            cache_path
                        );
                    }
                });
        }
    }

    fn handle_include(&mut self, include: IncludeDirective) {
        if self.included.contains(include.path()) {
            return;
        }
        self.included.insert(include.path().to_owned());

        if let Some(macro_defines) = self.try_load_macro_defines_from_cache(include.path()) {
            log::debug!(
                "Found {} macro definitions in {:?} (cached)",
                macro_defines.len(),
                include.path()
            );

            self.macro_defines.extend(macro_defines);
            return;
        }

        let path = if let Some(path) = include.get_include_path(&self.options.include_dirs) {
            path
        } else {
            log::warn!("Cannot find include file {:?}", include.path());
            return;
        };

        let text = match std::fs::read_to_string(&path) {
            Ok(text) => text,
            Err(e) => {
                log::warn!("Cannot read include file {:?}: {}", include.path(), e);
                return;
            }
        };

        let mut tokenizer = Tokenizer::new(text);
        tokenizer.set_filepath(&path);
        let mut ts = TokenStream::new(tokenizer, self.options.clone());
        ts.macro_defines = self.macro_defines.clone();
        match ts.parse::<Module>() {
            Ok(_) => {
                // TODO: Check replacement before filtering
                let new_macro_defines = ts
                    .macro_defines
                    .iter()
                    .filter(|(k, _)| !self.macro_defines.contains_key(k))
                    .map(|(k, v)| (k.clone(), v.clone()))
                    .collect::<BTreeMap<_, _>>();

                log::debug!(
                    "Found {} macro definitions in {:?}",
                    new_macro_defines.len(),
                    path
                );

                self.try_save_macro_defines_into_cache(include.path(), &new_macro_defines);

                self.macro_defines.extend(ts.macro_defines);
            }
            Err(e) => {
                log::warn!("Cannot parse include file {:?}: {}", include.path(), e);
            }
        }
    }
}

impl Iterator for TokenStream {
    type Item = Result<LexicalToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token().transpose()
    }
}

fn get_predefined_macro(name: &str, position: Position) -> Option<Vec<LexicalToken>> {
    let token: LexicalToken = match name {
        "MODULE" | "FUNCTION_NAME" => dummy_atom(position).into(),
        "LINE" | "FUNCTION_ARITY" | "OTP_RELEASE" => dummy_integer(position).into(),
        "MODULE_STRING" | "FILE" | "MACHINE" => dummy_string(position).into(),
        _ => return None,
    };
    Some(vec![token])
}

fn dummy_atom(position: Position) -> AtomToken {
    AtomToken::new("EFMT_DUMMY", position, position)
}

fn dummy_integer(position: Position) -> IntegerToken {
    IntegerToken::new(position, position)
}

fn dummy_string(position: Position) -> StringToken {
    StringToken::new("EFMT_DUMMY", position, position)
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, serde::Serialize, serde::Deserialize)]
struct MacroDefineKey {
    name: String,
    arity: Option<usize>,
}

impl MacroDefineKey {
    fn new(name: String, arity: Option<usize>) -> Self {
        Self { name, arity }
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
struct MacroDefine {
    variables: Option<Vec<String>>,
    replacement: Vec<LexicalToken>,
}

impl From<DefineDirective> for MacroDefine {
    fn from(x: DefineDirective) -> Self {
        Self {
            variables: x
                .variables()
                .map(|v| v.iter().map(|v| v.value().to_owned()).collect()),
            replacement: x.replacement().to_owned(),
        }
    }
}

impl MacroDefine {
    fn arity(&self) -> Option<usize> {
        self.variables.as_ref().map(|x| x.len())
    }
}
