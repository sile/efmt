use crate::items::forms::{DefineDirective, IncludeDirective};
use crate::items::generics::Either;
use crate::items::macros::{Macro, MacroName};
use crate::items::module::Module;
use crate::items::symbols::QuestionSymbol;
use crate::items::tokens::{
    AtomToken, CharToken, CommentKind, CommentToken, FloatToken, IntegerToken, KeywordToken,
    StringToken, SymbolToken, Token, VariableToken, WhitespaceToken,
};
use crate::parse::{Parse, Result, ResumeParse};
use crate::span::{Position, Span as _};
use erl_tokenize::values::Symbol;
use erl_tokenize::{PositionRange as _, Tokenizer};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, Default, Clone)]
pub struct TokenStreamOptions {
    include_dirs: Vec<PathBuf>,
}

impl TokenStreamOptions {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn include_dirs(mut self, dirs: Vec<PathBuf>) -> Self {
        self.include_dirs = dirs;
        self
    }
}

#[derive(Debug)]
pub struct TokenStream {
    tokenizer: Tokenizer<String>,
    tokens: Vec<Token>,
    current_token_index: usize,
    comments: BTreeMap<Position, CommentToken>,
    macros: BTreeMap<Position, Macro>,
    macro_defines: HashMap<String, DefineDirective>,
    missing_macros: HashSet<String>,
    included: HashSet<String>,
    options: TokenStreamOptions,
}

impl TokenStream {
    pub fn new(tokenizer: Tokenizer<String>, options: TokenStreamOptions) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            current_token_index: 0,
            comments: BTreeMap::new(),
            macros: BTreeMap::new(),
            macro_defines: HashMap::new(),
            missing_macros: HashSet::new(),
            included: HashSet::new(),
            options,
        }
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T> {
        let index = self.current_token_index;
        let result = T::parse(self);
        if result.is_err() {
            self.current_token_index = index;
        }
        result
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

    pub fn filepath(&self) -> Option<PathBuf> {
        self.tokenizer.next_position().filepath().cloned()
    }

    pub fn text(&self) -> &str {
        self.tokenizer.text()
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

    pub fn current_position(&self) -> Position {
        self.tokenizer.next_position().into()
    }

    pub fn current_whitespace_token(&mut self) -> Result<WhitespaceToken> {
        Ok(WhitespaceToken::new(
            self.prev_token_end_position(),
            self.next_token_start_position()?,
        ))
    }

    fn prev_token_end_position(&mut self) -> Position {
        if let Some(i) = self.current_token_index.checked_sub(1) {
            self.tokens[i].end_position()
        } else {
            self.tokenizer.next_position().into()
        }
    }

    fn next_token_start_position(&mut self) -> Result<Position> {
        let index = self.current_token_index;
        if index == self.tokens.len() && self.is_eof()? {
            Ok(self.tokenizer.next_position().into())
        } else {
            Ok(self.tokens[index].start_position())
        }
    }

    fn read_token(&mut self) -> Result<Option<Token>> {
        if let Some(token) = self.tokens.get(self.current_token_index).cloned() {
            self.current_token_index += 1;
            return Ok(Some(token));
        }

        while let Some(token) = self.tokenizer.next().transpose()? {
            let start_position = Position::from(token.start_position());
            let end_position = Position::from(token.end_position());
            let token: Token = match token {
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
                Token::Symbol(x) if x.value() == Symbol::Question => {
                    self.expand_macro()?;
                    return self.read_token();
                }
                Token::Symbol(x) if x.value() == Symbol::Hyphen => {
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

    fn expand_macro(&mut self) -> Result<()> {
        let start_index = self.current_token_index - 1;
        let start_position = self.tokens[start_index].start_position();
        let macro_name: MacroName = self.parse()?;
        let (variables, replacement) =
            if let Some(define) = self.macro_defines.get(macro_name.value()) {
                (
                    define.variables().map(|x| x.to_owned()),
                    define.replacement().to_owned(),
                )
            } else if let Some(replacement) = get_predefined_macro(macro_name.value()) {
                (None, replacement)
            } else {
                if !self.missing_macros.contains(macro_name.value()) {
                    log::warn!(
                        "The macro {:?} is not defined. 'EFMT_DUMMY' atom is used instead.",
                        macro_name.value()
                    );
                    self.missing_macros.insert(macro_name.value().to_owned());
                }
                (None, vec![Token::from(dummy_atom())])
            };
        let arity = variables.as_ref().map(|x| x.len());
        let question = QuestionSymbol::new(start_position);
        let r#macro = Macro::parse(self, question, macro_name, arity)?;
        let replacement = r#macro.expand(variables, replacement);

        let unread_tokens = self.tokens.split_off(self.current_token_index);
        self.tokens.truncate(start_index);
        self.tokens.extend(replacement);
        self.tokens.extend(unread_tokens);
        self.current_token_index = start_index;
        self.macros.insert(start_position, r#macro);
        Ok(())
    }

    fn try_handle_directives(&mut self) -> Result<()> {
        self.current_token_index -= 1;
        let result: Result<Either<DefineDirective, IncludeDirective>> = self.parse();
        match result {
            Ok(Either::A(x)) => {
                self.macro_defines.insert(x.macro_name().to_owned(), x);
            }
            Ok(Either::B(x)) => {
                self.handle_include(x);
            }
            Err(_) => {}
        }
        Ok(())
    }

    fn handle_include(&mut self, include: IncludeDirective) {
        if self.included.contains(include.path()) {
            return;
        }
        self.included.insert(include.path().to_owned());

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
        match ts.parse::<Module>() {
            Ok(_) => {
                log::debug!(
                    "Found {} macro definitions in {:?}",
                    ts.macro_defines.len(),
                    path
                );
                self.macro_defines.extend(ts.macro_defines);
            }
            Err(e) => {
                log::warn!("Cannot parse include file {:?}: {}", include.path(), e);
            }
        }
    }
}

impl Iterator for TokenStream {
    type Item = Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self.read_token().transpose()
    }
}

fn get_predefined_macro(name: &str) -> Option<Vec<Token>> {
    let token: Token = match name {
        "MODULE" => dummy_atom().into(),
        "LINE" | "FUNCTION_ARITY" | "OTP_RELEASE" => dummy_integer().into(),
        "MODULE_STRING" | "FILE" | "MACHINE" | "FUNCTION_NAME" => dummy_string().into(),
        _ => return None,
    };
    Some(vec![token])
}

fn dummy_atom() -> AtomToken {
    AtomToken::new("EFMT_DUMMY", Position::new(0, 0, 0), Position::new(0, 0, 0))
}

fn dummy_integer() -> IntegerToken {
    IntegerToken::new(Position::new(0, 0, 0), Position::new(0, 0, 0))
}

fn dummy_string() -> StringToken {
    StringToken::new("EFMT_DUMMY", Position::new(0, 0, 0), Position::new(0, 0, 0))
}
