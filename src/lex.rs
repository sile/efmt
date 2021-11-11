use crate::items::forms::{DefineDirective, IncludeDirective};
use crate::items::generics::Either;
use crate::items::macros::Macro;
use crate::items::symbols::QuestionSymbol;
use crate::items::tokens::{
    AtomToken, CharToken, CommentKind, CommentToken, FloatToken, IntegerToken, KeywordToken,
    StringToken, SymbolToken, Token, VariableToken,
};
use crate::parse;
use crate::span::{Position, Span};
use erl_tokenize::values::Symbol;
use erl_tokenize::{PositionRange as _, Tokenizer};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    ParseError(#[from] Box<parse::Error>),

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

// TODO: s/Lexer/TokenStream/
#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer<String>,
    tokens: Vec<Token>,
    current_token_index: usize,
    comments: BTreeMap<Position, CommentToken>,
    macros: BTreeMap<Position, Macro>,
    macro_defines: HashMap<String, DefineDirective>,
    missing_macros: HashSet<String>,
}

impl Lexer {
    pub fn new(tokenizer: Tokenizer<String>) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            current_token_index: 0,
            comments: BTreeMap::new(),
            macros: BTreeMap::new(),
            macro_defines: HashMap::new(),
            missing_macros: HashSet::new(),
        }
    }

    pub fn parse<T: parse::Parse>(&mut self) -> parse::Result<T> {
        let index = self.current_token_index;
        let result = T::parse(self);
        if result.is_err() {
            self.current_token_index = index;
        }
        result
    }

    // TODO
    pub fn generate_error_place(&self, unexpected_token: &Token) -> String {
        use std::fmt::Write;

        let line = unexpected_token.start_position().line();
        let column = unexpected_token.start_position().column();
        let file = self
            .filepath()
            .and_then(|x| x.to_str().map(|x| x.to_owned()))
            .unwrap_or_else(|| "<anonymous>".to_owned());
        let line_string = self.get_line_string(unexpected_token);

        let mut m = String::new();
        writeln!(&mut m).unwrap();
        writeln!(&mut m, "--> {}:{}:{}", file, line, column).unwrap();
        writeln!(&mut m, "{} | {}", line, line_string).unwrap();
        writeln!(
            &mut m,
            "{:line_width$} | {:>token_column$} unexpected token",
            "",
            "^",
            line_width = line.to_string().len(),
            token_column = column
        )
        .unwrap();
        m
    }

    fn get_line_string(&self, token: &Token) -> &str {
        let text = self.text();
        let offset = token.start_position().offset();
        let line_start = (&text[..offset]).rfind('\n').unwrap_or(0);
        let line_end = (&text[offset..])
            .find('\n')
            .map(|x| x + offset)
            .unwrap_or_else(|| text.len());
        (&text[line_start..line_end]).trim_matches(char::is_control)
    }

    pub fn peek<T: parse::Parse>(&mut self) -> bool {
        let index = self.current_token_index;
        let ok = self.parse::<T>().is_ok();
        self.current_token_index = index;
        ok
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
        if self.current_token_index < self.tokens.len() {
            Ok(false)
        } else if self.read_token()?.is_some() {
            self.current_token_index -= 1;
            Ok(false)
        } else {
            Ok(true)
        }
    }

    pub fn prev_token_end_position(&mut self) -> Result<Position> {
        let index = self.current_token_index;
        if index == 0 {
            Ok(self.tokenizer.next_position().into())
        } else {
            Ok(self.tokens[index - 1].end_position())
        }
    }

    pub fn next_token_start_position(&mut self) -> Result<Position> {
        let index = self.current_token_index;
        if index == self.tokens.len() && self.read_token()?.is_none() {
            Ok(self.tokenizer.next_position().into())
        } else {
            self.current_token_index = index;
            Ok(self.tokens[index].start_position())
        }
    }

    pub fn read_token(&mut self) -> Result<Option<Token>> {
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
        // TODO:
        use crate::items::macros::MacroName;

        let start_index = self.current_token_index - 1;
        let start_position = self.tokens[start_index].start_position();
        let macro_name: MacroName = self.parse().map_err(Box::new)?;
        let (variables, replacement) = if let Some(define) =
            self.macro_defines.get(macro_name.value())
        {
            (
                define.variables().map(|x| x.to_owned()),
                define.replacement().to_owned(),
            )
        } else if let Some(replacement) = get_predefined_macro(macro_name.value(), start_position) {
            (None, replacement)
        } else {
            if !self.missing_macros.contains(macro_name.value()) {
                // TODO: use logger
                eprintln!(
                    "[WARN] The macro {:?} is not defined. Use the atom 'EFMT_DUMMY' instead.",
                    macro_name.value()
                );
                self.missing_macros.insert(macro_name.value().to_owned());
            }
            let dummy_token = AtomToken::new("EFMT_DUMMY", start_position, start_position);
            let replacement = vec![Token::from(dummy_token)];
            (None, replacement)
        };
        let arity = variables.as_ref().map(|x| x.len());
        let question = QuestionSymbol::new(start_position);
        let r#macro = Macro::parse(self, question, macro_name, arity).map_err(Box::new)?;
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
        match self.parse().ok() {
            Some(Either::A(x)) => {
                self.macro_defines
                    .insert(DefineDirective::macro_name(&x).to_owned(), x);
            }
            Some(Either::B(x)) => {
                self.handle_include(x);
            }
            None => {}
        }
        Ok(())
    }

    fn handle_include(&mut self, include: IncludeDirective) {
        let code_paths: Vec<String> = Vec::new(); // TODO
        if let Some(path) = include.get_include_path(&code_paths) {
            if let Ok(text) = std::fs::read_to_string(&path) {
                let mut tokenizer = Tokenizer::new(text);
                tokenizer.set_filepath(&path);
                let mut lexer = Lexer::new(tokenizer);
                let ok = {
                    // TODO: Optimize by skipping to parse unnecessary items.
                    lexer.parse::<crate::items::module::Module>().is_ok()
                };
                if ok {
                    // TODO: delete this message
                    eprintln!(
                        "[INFO] Found {} macro definitions in {:?}",
                        lexer.macro_defines.len(),
                        path
                    );
                    self.macro_defines.extend(lexer.macro_defines);
                    return;
                }
            }
        }
        eprintln!(
            "[WARN] Cannot handle an include directive: path={:?}",
            include.path()
        );
    }
}

fn get_predefined_macro(name: &str, position: Position) -> Option<Vec<Token>> {
    let token: Token = match name {
        "MODULE" => AtomToken::new("EFMT_DUMMY", position, position).into(),
        "LINE" | "FUNCTION_ARITY" | "OTP_RELEASE" => IntegerToken::new(position, position).into(),
        "MODULE_STRING" | "FILE" | "MACHINE" | "FUNCTION_NAME" => {
            StringToken::new("EFMT_DUMMY", position, position).into()
        }
        _ => return None,
    };
    Some(vec![token])
}
