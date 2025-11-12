use crate::items::components::Either;
use crate::items::forms::{DefineDirective, IncludeDirective};
use crate::items::macros::{Macro, MacroName};
use crate::items::symbols::{OpenParenSymbol, QuestionSymbol};
use crate::items::tokens::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, LexicalToken,
    SigilStringToken, StringToken, SymbolToken, VariableToken,
};
use crate::parse::Tokenizer;
use crate::parse::{Error, Parse, Result, ResumeParse};
use crate::span::{Position, Span};
use erl_tokenize::PositionRange as _;
use erl_tokenize::values::Symbol;
use std::collections::{BTreeMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use super::TokenOrShebang;

#[derive(Debug)]
pub struct TokenStream {
    tokenizer: Tokenizer,
    tokens: Vec<LexicalToken>,
    current_token_index: usize,
    comments: BTreeMap<Position, CommentToken>,
    macros: BTreeMap<Position, Macro>,
    macro_defines: MacroDefines,
    new_macro_defines: HashSet<MacroDefineKey>,
    missing_macros: HashSet<String>,
    known_replacement: HashSet<(usize, Vec<LexicalToken>)>,
    disable_macro_expand: bool,
    parsing_tokens: bool,
    text: Arc<String>,
    path: Option<Arc<PathBuf>>,
    last_parse_error: Option<Error>,
}

impl TokenStream {
    pub fn new(tokenizer: Tokenizer) -> Self {
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
            new_macro_defines: HashSet::new(),
            missing_macros: HashSet::new(),
            known_replacement: HashSet::new(),
            disable_macro_expand: false,
            parsing_tokens: false,
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
                .is_none_or(|e0| e0.position() < e.position())
            {
                self.last_parse_error = Some(e.clone());
            }
        } else {
            let succeeded_position = self.prev_token_end_position();
            if self
                .last_parse_error
                .as_ref()
                .is_some_and(|e| e.position() < succeeded_position)
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

    pub fn set_filepath<P: AsRef<Path>>(&mut self, path: P) {
        self.tokenizer.set_filepath(&path);
        self.path = Some(Arc::new(path.as_ref().to_path_buf()));
    }

    pub fn contains_comment(&self, span: &impl Span) -> bool {
        self.comments
            .range(span.start_position()..span.end_position())
            .count()
            > 0
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

    pub fn with_macro_expand_disabled<F, T>(&mut self, f: F) -> Result<T>
    where
        F: FnOnce(&mut Self) -> Result<T>,
    {
        assert!(!self.disable_macro_expand);

        self.disable_macro_expand = true;
        let result = f(self);
        self.disable_macro_expand = false;
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

    pub fn visited_tokens(&self) -> &[LexicalToken] {
        &self.tokens
    }

    fn read_token(&mut self) -> Result<Option<LexicalToken>> {
        if let Some(token) = self.tokens.get(self.current_token_index).cloned() {
            self.current_token_index += 1;

            if !self.disable_macro_expand {
                match &token {
                    LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
                        return self.expand_macro_and_read_token();
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
                TokenOrShebang::Shebang(_) => {
                    let is_trailing = false;
                    self.comments.insert(
                        start_position,
                        CommentToken::new(is_trailing, start_position, end_position),
                    );
                    continue;
                }
                TokenOrShebang::Token(erl_tokenize::Token::Whitespace(_)) => {
                    continue;
                }
                TokenOrShebang::Token(erl_tokenize::Token::Comment(x)) => {
                    let is_trailing = self
                        .tokens
                        .last()
                        .is_some_and(|y| y.start_position().line() == x.start_position().line());
                    self.comments.insert(
                        start_position,
                        CommentToken::new(is_trailing, start_position, end_position),
                    );
                    continue;
                }
                TokenOrShebang::Token(erl_tokenize::Token::Symbol(x)) => {
                    SymbolToken::new(x.value(), start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Atom(x)) => {
                    AtomToken::new(x.value(), start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Char(_)) => {
                    CharToken::new(start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Float(_)) => {
                    FloatToken::new(start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Integer(_)) => {
                    IntegerToken::new(start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Keyword(x)) => {
                    KeywordToken::new(x.value(), start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::SigilString(_)) => {
                    SigilStringToken::new(start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::String(x)) => {
                    StringToken::new(x.value(), start_position, end_position).into()
                }
                TokenOrShebang::Token(erl_tokenize::Token::Variable(x)) => {
                    VariableToken::new(x.value(), start_position, end_position).into()
                }
            };
            self.tokens.push(token.clone());
            self.current_token_index += 1;

            match &token {
                LexicalToken::Symbol(x) if x.value() == Symbol::Question => {
                    if !self.disable_macro_expand {
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
            _ => self.expand_macro_with_args(macro_name, |this, key| {
                this.macro_defines.get(key).cloned()
            }),
        }
    }

    fn expand_macro_with_args<F>(&mut self, macro_name: MacroName, f: F) -> Result<()>
    where
        F: FnOnce(&Self, &MacroDefineKey) -> Option<MacroDefine>,
    {
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();
        let question = QuestionSymbol::new(start_position);

        let r#macro: Macro = self.with_macro_expand_disabled(|ts| {
            ts.resume_parse((question, macro_name.clone(), true))
        })?;
        let arity = r#macro.arity();
        assert!(arity.is_some());

        let key = MacroDefineKey::new(macro_name.value().to_owned(), arity);
        if let Some(define) = f(self, &key) {
            let variables = define.variables.as_ref().map(|x| x.to_owned());

            let replacement = r#macro.expand(variables, define.replacement);
            self.replace_tokens(start_index, &key, replacement);
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
        let key = MacroDefineKey::new(macro_name.value().to_owned(), None);
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();
        let question = QuestionSymbol::new(start_position);
        let r#macro: Macro = self.resume_parse((question, macro_name, false))?;

        let replacement = r#macro.expand(None, replacement);
        self.replace_tokens(start_index, &key, replacement);
        self.macros.entry(start_position).or_insert(r#macro);

        Ok(())
    }

    fn expand_unknown_macro(&mut self, macro_name: MacroName) -> Result<()> {
        let start_index = self.current_token_index - 2;
        let start_position = self.tokens[start_index].start_position();

        if let Some((has_args, replacement)) =
            get_predefined_macro(macro_name.value(), start_position)
        {
            if has_args {
                self.expand_macro_with_args(macro_name, |_this, _key| {
                    Some(MacroDefine {
                        variables: None,
                        replacement,
                    })
                })
            } else {
                self.expand_macro_without_args(macro_name, replacement)
            }
        } else if self.disable_macro_expand {
            log::debug!(
                "Found an undefined macro {:?} in disabling macro expansions.",
                macro_name.value()
            );
            Ok(())
        } else {
            if !self.missing_macros.contains(macro_name.value()) {
                log::debug!(
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

    fn replace_tokens(
        &mut self,
        start_index: usize,
        key: &MacroDefineKey,
        mut replacement: Vec<LexicalToken>,
    ) {
        if !replacement.is_empty()
            && !self
                .known_replacement
                .insert((start_index, replacement.clone()))
        {
            log::debug!(
                "A circular macro {:?} was detected. It was replaced with a dummy atom 'EFMT_DUMMY'.",
                key.to_string(),
            );
            let start_position = self.tokens[start_index].start_position();
            replacement = vec![LexicalToken::from(dummy_atom(start_position))];
            self.macro_defines
                .get_mut(key)
                .expect("unreachable")
                .replacement
                .clone_from(&replacement);
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
                self.new_macro_defines.insert(key.clone());
                self.macro_defines.insert(key, define);
            }
            Ok(Either::B(_)) => {}
            Err(_) => {}
        }
        Ok(())
    }
}

impl Iterator for TokenStream {
    type Item = Result<LexicalToken>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token().transpose()
    }
}

fn get_predefined_macro(name: &str, position: Position) -> Option<(bool, Vec<LexicalToken>)> {
    match name {
        "MODULE" | "FUNCTION_NAME" => Some((false, vec![dummy_atom(position).into()])),
        "LINE" | "FUNCTION_ARITY" | "OTP_RELEASE" => {
            Some((false, vec![dummy_integer(position).into()]))
        }
        "MODULE_STRING" | "FILE" | "MACHINE" => Some((false, vec![dummy_string(position).into()])),
        "assertMatch" | "assertNotMatch" => Some((true, vec![dummy_atom(position).into()])),
        _ => None,
    }
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

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub(crate) struct MacroDefineKey {
    name: String,
    arity: Option<usize>,
}

impl MacroDefineKey {
    pub(crate) fn new(name: String, arity: Option<usize>) -> Self {
        Self { name, arity }
    }
}

impl std::fmt::Display for MacroDefineKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(arity) = self.arity {
            write!(f, "?{}/{arity}", self.name)
        } else {
            write!(f, "?{}", self.name)
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct MacroDefine {
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
    pub(crate) fn arity(&self) -> Option<usize> {
        self.variables.as_ref().map(|x| x.len())
    }
}

pub(crate) type MacroDefines = BTreeMap<MacroDefineKey, MacroDefine>;
