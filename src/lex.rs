use crate::items::forms::DefineDirective;
use crate::items::macros::Macro;
use crate::items::tokens::{
    AtomToken, CharToken, CommentToken, FloatToken, IntegerToken, KeywordToken, StringToken,
    SymbolToken, Token, VariableToken,
};
use crate::parse;
use crate::span::{Position, Span};
use erl_tokenize::PositionRange as _;
use erl_tokenize::Tokenizer;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("unknown transaction {transaction:?}")]
    UnknownTransaction { transaction: Transaction },

    #[error(transparent)]
    ParseError(#[from] Box<parse::Error>),

    #[error(transparent)]
    TokenizeError(#[from] erl_tokenize::Error),
}

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Lexer {
    tokenizer: Tokenizer<String>,
    tokens: Vec<Token>,
    current_token_index: usize,
    comments: BTreeMap<Position, CommentToken>,
    macros: BTreeMap<Position, Macro>,
    macro_defines: HashMap<String, DefineDirective>,
    transaction_seqno: u64,
    transactions: HashSet<Transaction>,
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
            transaction_seqno: 0,
            transactions: HashSet::new(),
        }
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
        if index == self.tokens.len() {
            if self.read_token()?.is_none() {
                return Ok(self.tokenizer.next_position().into());
            }
        }
        self.current_token_index = index;
        Ok(self.tokens[index].start_position())
    }

    pub fn start_transaction(&mut self) -> Transaction {
        let index = self.current_token_index;
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
            assert!(transaction.index <= self.current_token_index);
            self.current_token_index = transaction.index;
            Ok(())
        } else {
            Err(Error::UnknownTransaction { transaction })
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
                erl_tokenize::Token::Comment(_) => {
                    self.comments.insert(
                        start_position,
                        CommentToken::new(start_position, end_position),
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
                erl_tokenize::Token::String(_) => {
                    StringToken::new(start_position, end_position).into()
                }
                erl_tokenize::Token::Variable(x) => {
                    VariableToken::new(x.value(), start_position, end_position).into()
                }
            };
            self.tokens.push(token.clone());
            self.current_token_index += 1;

            // let token = match Question::new(token) {
            //     Ok(question) => {
            //         self.expand_macro(question)?;
            //         return self.read_token();
            //     }
            //     Err(token) => token,
            // };

            // match &token {
            //     Token::Symbol(x) if x.value() == Symbol::Hyphen => {
            //         let index = self.current;
            //         self.try_handle_directives()?;
            //         self.current = index;
            //     }
            //     _ => {}
            // }

            return Ok(Some(token));
        }

        Ok(None)
    }

    //     pub fn is_macro_expanded(&self, token: &Token) -> bool {
    //         self.macros.contains_key(&token.region().start())
    //     }

    //     fn expand_macro(&mut self, question: Question) -> Result<()> {
    //         let start = self.current - 1;
    //         let start_position = self.tokens[start].region().start();

    //         let macro_name = Parser::new(self)
    //             .parse::<MacroName>()
    //             .map_err(anyhow::Error::from)?;
    //         let (replacement, macro_call) =
    //             if let Some(define) = self.macro_defines.get(macro_name.get()).cloned() {
    //                 if let Some(vars) = define.variables() {
    //                     let macro_call: Macro = Parser::new(self)
    //                         .resume_parse((question, macro_name, Some(vars.len())))
    //                         .map_err(anyhow::Error::from)?;
    //                     let args = vars
    //                         .iter()
    //                         .map(|x| x.value())
    //                         .zip(macro_call.args().expect("unreachable").iter())
    //                         .collect::<BTreeMap<_, _>>();
    //                     let mut tokens = Vec::new();
    //                     for token in define.replacement().tokens().iter().cloned() {
    //                         match token {
    //                             Token::Variable(x) if args.contains_key(x.value()) => {
    //                                 tokens.extend(args[x.value()].tokens().iter().cloned());
    //                             }
    //                             token => {
    //                                 tokens.push(token);
    //                             }
    //                         }
    //                     }
    //                     (tokens, macro_call)
    //                 } else {
    //                     let macro_call = Parser::new(self)
    //                         .resume_parse((question, macro_name, None))
    //                         .map_err(anyhow::Error::from)?;
    //                     let tokens = define.replacement().tokens().to_owned();
    //                     (tokens, macro_call)
    //                 }
    //             } else {
    //                 // TODO: logger
    //                 eprintln!(
    //                     "[WARN] The macro {:?} is not defined. Use the atom 'EFMT_DUMMY' instead.",
    //                     macro_name.get()
    //                 );
    //                 let macro_call: Macro = Parser::new(self)
    //                     .resume_parse((question, macro_name.clone(), None))
    //                     .map_err(anyhow::Error::from)?;
    //                 let dummy_token = AtomToken::new("EFMT_DUMMY", macro_call.region());
    //                 let tokens = vec![dummy_token.into()];
    //                 (tokens, macro_call)
    //             };
    //         let replacement = replacement.into_iter().map(|token| match token {
    //             Token::Atom(x) => AtomToken::new(x.value(), macro_call.region()).into(),
    //             Token::Char(_) => CharToken::new(macro_call.region()).into(),
    //             Token::Float(_) => FloatToken::new(macro_call.region()).into(),
    //             Token::Integer(_) => IntegerToken::new(macro_call.region()).into(),
    //             Token::Keyword(x) => KeywordToken::new(x.value(), macro_call.region()).into(),
    //             Token::String(_) => StringToken::new(macro_call.region()).into(),
    //             Token::Symbol(x) => SymbolToken::new(x.value(), macro_call.region()).into(),
    //             Token::Variable(x) => VariableToken::new(x.value(), macro_call.region()).into(),
    //         });

    //         let unread_tokens = self.tokens.split_off(self.current);
    //         self.tokens.truncate(start);
    //         self.tokens.extend(replacement);
    //         self.tokens.extend(unread_tokens);
    //         self.current = start;
    //         self.macros.insert(start_position, macro_call);
    //         Ok(())
    //     }

    //     fn try_handle_directives(&mut self) -> Result<()> {
    //         let is_target = match self.read_token()? {
    //             Some(Token::Atom(x)) if matches!(x.value(), "define" | "include" | "include_lib") => {
    //                 true
    //             }
    //             _ => false,
    //         };
    //         if !is_target {
    //             return Ok(());
    //         }

    //         self.current -= 2;
    //         let mut parser = Parser::new(self);
    //         match parser.try_parse::<Attr>() {
    //             Some(Attr::Define(x)) => {
    //                 self.macro_defines.insert(x.macro_name().to_owned(), x);
    //             }
    //             Some(Attr::Include(x)) => {
    //                 eprintln!("TODO: {:?}", x)
    //             }
    //             Some(Attr::IncludeLib(x)) => {
    //                 eprintln!("TODO: {:?}", x)
    //             }
    //             _ => {}
    //         }
    //         Ok(())
    //     }
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Transaction {
    seqno: u64,
    index: usize,
}
