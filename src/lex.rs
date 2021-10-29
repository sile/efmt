use crate::cst::attributes::{Attr, Define};
use crate::cst::macros::{MacroCall, MacroName};
use crate::parse::Parser;
use crate::token::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, Region,
    StringToken, Symbol, SymbolToken, Token, TokenIndex, TokenPosition, TokenRegion, VariableToken,
};
use crate::tokenize::{self, Tokenizer};
use erl_tokenize::PositionRange as _;
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown transaction {transaction:?}")]
    UnknownTransaction { transaction: Transaction },

    #[error(transparent)]
    TokenizeError(#[from] tokenize::Error),

    // TODO: remove
    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer,
    tokens: Vec<Token>,
    current: usize,
    comments: BTreeMap<TokenPosition, CommentToken>,
    macro_calls: BTreeMap<TokenPosition, MacroCall>,
    macro_defines: HashMap<String, Define>,
    transaction_seqno: u64,
    transactions: HashSet<Transaction>,
}

impl Lexer {
    pub fn new(tokenizer: Tokenizer) -> Self {
        Self {
            tokenizer,
            tokens: Vec::new(),
            current: 0,
            comments: BTreeMap::new(),
            macro_calls: BTreeMap::new(),
            macro_defines: HashMap::new(),
            transaction_seqno: 0,
            transactions: HashSet::new(),
        }
    }

    pub fn finish(self) -> LexedText {
        LexedText {
            original_text: self.tokenizer.finish(),
            tokens: self.tokens,
            comments: self.comments,
            macro_calls: self.macro_calls,
        }
    }

    pub fn is_eof(&mut self) -> Result<bool> {
        if self.current < self.tokens.len() {
            Ok(false)
        } else if self.read_token()?.is_some() {
            self.current -= 1;
            Ok(false)
        } else {
            Ok(true)
        }
    }

    pub fn read_token(&mut self) -> Result<Option<Token>> {
        if let Some(token) = self.tokens.get(self.current).cloned() {
            self.current += 1;
            return Ok(Some(token));
        }

        while let Some(token) = self.tokenizer.next().transpose()? {
            let token: Token = match token {
                erl_tokenize::Token::Whitespace(_) => {
                    continue;
                }
                erl_tokenize::Token::Comment(x) => {
                    self.comments.insert(
                        x.start_position().into(),
                        CommentToken::new(TokenRegion::from(&x)),
                    );
                    continue;
                }
                erl_tokenize::Token::Symbol(x) => {
                    SymbolToken::new(x.value(), TokenRegion::from(&x)).into()
                }
                erl_tokenize::Token::Atom(x) => {
                    AtomToken::new(x.value(), TokenRegion::from(&x)).into()
                }
                erl_tokenize::Token::Char(x) => CharToken::new(TokenRegion::from(&x)).into(),
                erl_tokenize::Token::Float(x) => FloatToken::new(TokenRegion::from(&x)).into(),
                erl_tokenize::Token::Integer(x) => IntegerToken::new(TokenRegion::from(&x)).into(),
                erl_tokenize::Token::Keyword(x) => {
                    KeywordToken::new(x.value(), TokenRegion::from(&x)).into()
                }
                erl_tokenize::Token::String(x) => StringToken::new(TokenRegion::from(&x)).into(),
                erl_tokenize::Token::Variable(x) => {
                    VariableToken::new(x.value(), TokenRegion::from(&x)).into()
                }
            };
            self.tokens.push(token.clone());
            self.current += 1;

            match &token {
                Token::Symbol(x) if x.value() == Symbol::Question => {
                    self.expand_macro(x.clone())?;
                    return self.read_token();
                }
                _ => {}
            }

            match &token {
                Token::Symbol(x) if x.value() == Symbol::Hyphen => {
                    let index = self.current;
                    self.try_handle_directives()?;
                    self.current = index;
                }
                _ => {}
            }

            return Ok(Some(token));
        }

        Ok(None)
    }

    fn expand_macro(&mut self, question: SymbolToken) -> Result<()> {
        let start = self.current - 1;
        let start_position = self.tokens[start].region().start();

        let macro_name = Parser::new(self)
            .parse::<MacroName>()
            .map_err(anyhow::Error::from)?;
        let (replacement, macro_call) =
            if let Some(define) = self.macro_defines.get(macro_name.get()).cloned() {
                if let Some(vars) = define.variables() {
                    let macro_call: MacroCall = Parser::new(self)
                        .resume_parse((question, macro_name, Some(vars.len())))
                        .map_err(anyhow::Error::from)?;
                    let args = vars
                        .iter()
                        .map(|x| x.value())
                        .zip(macro_call.args().expect("unreachable").iter())
                        .collect::<BTreeMap<_, _>>();
                    let mut tokens = Vec::new();
                    for token in define.replacement().tokens().iter().cloned() {
                        match token {
                            Token::Variable(x) if args.contains_key(x.value()) => {
                                tokens.extend(args[x.value()].tokens().iter().cloned());
                            }
                            token => {
                                tokens.push(token);
                            }
                        }
                    }
                    (tokens, macro_call)
                } else {
                    let macro_call = Parser::new(self)
                        .resume_parse((question, macro_name, None))
                        .map_err(anyhow::Error::from)?;
                    let tokens = define.replacement().tokens().to_owned();
                    (tokens, macro_call)
                }
            } else {
                // TODO: logger
                eprintln!(
                    "[WARN] The macro {:?} is not defined. Use the atom 'EFMT_DUMMY' instead.",
                    macro_name.get()
                );
                let macro_call: MacroCall = Parser::new(self)
                    .resume_parse((question, macro_name.clone(), None))
                    .map_err(anyhow::Error::from)?;
                let dummy_token = AtomToken::new("EFMT_DUMMY", macro_call.region());
                let tokens = vec![dummy_token.into()];
                (tokens, macro_call)
            };
        let replacement = replacement.into_iter().map(|token| match token {
            Token::Atom(x) => AtomToken::new(x.value(), macro_call.region()).into(),
            Token::Char(_) => CharToken::new(macro_call.region()).into(),
            Token::Float(_) => FloatToken::new(macro_call.region()).into(),
            Token::Integer(_) => IntegerToken::new(macro_call.region()).into(),
            Token::Keyword(x) => KeywordToken::new(x.value(), macro_call.region()).into(),
            Token::String(_) => StringToken::new(macro_call.region()).into(),
            Token::Symbol(x) => SymbolToken::new(x.value(), macro_call.region()).into(),
            Token::Variable(x) => VariableToken::new(x.value(), macro_call.region()).into(),
        });

        let unread_tokens = self.tokens.split_off(self.current);
        self.tokens.truncate(start);
        self.tokens.extend(replacement);
        self.tokens.extend(unread_tokens);
        self.current = start;
        self.macro_calls.insert(start_position, macro_call);
        Ok(())
    }

    fn try_handle_directives(&mut self) -> Result<()> {
        let is_target = match self.read_token()? {
            Some(Token::Atom(x)) if matches!(x.value(), "define" | "include" | "include_lib") => {
                true
            }
            _ => false,
        };
        if !is_target {
            return Ok(());
        }

        self.current -= 2;
        let mut parser = Parser::new(self);
        match parser.try_parse::<Attr>() {
            Some(Attr::Define(x)) => {
                self.macro_defines.insert(x.macro_name().to_owned(), x);
            }
            Some(Attr::Include(x)) => {
                eprintln!("TODO: {:?}", x)
            }
            Some(Attr::IncludeLib(x)) => {
                eprintln!("TODO: {:?}", x)
            }
            _ => {}
        }
        Ok(())
    }

    // TODO: s/current/next_start/
    // TODO: remove (maybe)
    pub fn current_position(&self) -> TokenPosition {
        self.tokens
            .get(self.current)
            .map(|x| x.region().start())
            .unwrap_or_else(|| self.tokenizer.next_position().into())
    }

    pub fn current_token_index(&self) -> TokenIndex {
        TokenIndex::new(self.current)
    }

    pub fn start_transaction(&mut self) -> Transaction {
        let index = self.current;
        let seqno = self.transaction_seqno;
        self.transaction_seqno += 1;
        self.transactions.insert(Transaction { seqno, index });
        Transaction { seqno, index }
    }

    pub fn commit(&mut self, transaction: Transaction) -> Result<()> {
        if self.transactions.remove(&transaction) {
            Ok(())
        } else {
            Err(Error::UnknownTransaction { transaction })
        }
    }

    pub fn rollback(&mut self, transaction: Transaction) -> Result<()> {
        if self.transactions.remove(&transaction) {
            assert!(transaction.index <= self.current);
            self.current = transaction.index;
            Ok(())
        } else {
            Err(Error::UnknownTransaction { transaction })
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Transaction {
    seqno: u64,
    index: usize,
}

#[derive(Debug)]
pub struct LexedText {
    pub original_text: String,
    pub tokens: Vec<Token>,
    pub comments: BTreeMap<TokenPosition, CommentToken>,
    pub macro_calls: BTreeMap<TokenPosition, MacroCall>,
}

// TODO
// #[cfg(test)]
// mod tests {
//     use super::*;
//     use anyhow::Context;

//     #[test]
//     fn lexer_works() -> anyhow::Result<()> {
//         let testnames = ["nomacro", "macro-novars", "macro-vars", "macro-nest"];
//         for testname in testnames {
//             let (before_path, before, after_path, after_expected) =
//                 crate::tests::load_testdata(&format!("lex/{}", testname))
//                     .with_context(|| format!("[{}] cannot load testdata", testname))?;

//             let tokenizer = Tokenizer::new(before);
//             let mut lexer = Lexer::new(tokenizer);
//             let mut tokens = Vec::new();
//             while let Some(token) = lexer
//                 .read_token()
//                 .with_context(|| format!("[{}] cannot read token", testname))?
//             {
//                 tokens.push(token);
//             }

//             let after_actual = tokens
//                 .iter()
//                 .map(|x| x.text())
//                 .collect::<Vec<_>>()
//                 .join(" ");
//             anyhow::ensure!(
//                 after_actual == after_expected.trim(),
//                 "unexpected result.\n[ACTUAL] {}\n{}\n\n[EXPECTED] {}\n{}",
//                 before_path,
//                 after_actual,
//                 after_path,
//                 after_expected
//             );
//         }
//         Ok(())
//     }
// }
