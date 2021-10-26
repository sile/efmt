use crate::token::{LexicalToken, TokenRegion};
use erl_tokenize::Position;

#[derive(Debug, Clone)]
pub struct MacroCall {
    // TODO: make private
    pub name: String,
    pub args: Option<Vec<Vec<LexicalToken>>>,
    pub range: TokenRegion,       // TODO: rename
    pub start_position: Position, // TODO: start_position, end_position
}
