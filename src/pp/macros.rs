use crate::expect::Either;
use erl_tokenize::tokens::{AtomToken, VariableToken};
use erl_tokenize::{LexicalToken, Position};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct MacroDefine {
    // TODO: Make private
    pub name: Either<AtomToken, VariableToken>,
    pub params: Option<Vec<VariableToken>>,
    pub replacement: Vec<LexicalToken>,
}

impl MacroDefine {
    pub fn predefined() -> HashMap<String, Self> {
        [
            "MODULE",
            "MODULE_STRING",
            "FILE",
            "LINE",
            "MACHINE",
            "FUNCTION_NAME",
            "FUNCTION_ARITY",
            "OTP_RELEASE",
        ]
        .into_iter()
        .map(|name| {
            (
                name.to_owned(),
                MacroDefine {
                    name: Either::B(VariableToken::from_value(name, Position::new()).expect("bug")),
                    params: None,
                    replacement: vec![AtomToken::from_value("dummy", Position::new()).into()],
                },
            )
        })
        .collect()
    }

    pub fn name(&self) -> &str {
        match &self.name {
            Either::A(x) => x.value(),
            Either::B(x) => x.value(),
        }
    }

    pub fn replace_variables(&self, args: &HashMap<&str, Vec<LexicalToken>>) -> Vec<LexicalToken> {
        let mut tokens = Vec::new();
        for token in self.replacement.iter().cloned() {
            match &token {
                LexicalToken::Variable(x) if args.contains_key(x.value()) => {
                    tokens.extend(args[x.value()].iter().cloned());
                    continue;
                }
                _ => {}
            }
            tokens.push(token);
        }
        tokens
    }
}

#[derive(Debug, Clone)]
pub struct MacroCall {
    // TODO: make private
    pub name: String,
    pub args: Option<Vec<Vec<LexicalToken>>>,
    pub start_position: Position,
    pub end_position: Position,
}
