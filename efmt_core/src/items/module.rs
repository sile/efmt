use super::components::Either;
use super::tokens::LexicalToken;
use crate::format::{Format, Formatter};
use crate::items::forms::DefineDirective;
use crate::items::{forms, Form};
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use erl_tokenize::values::Symbol;

/// [Form]*
#[derive(Debug, Clone, Span)]
pub struct Module<const ALLOW_PARTIAL_FAILURE: bool = false> {
    sof: Position,
    forms: Vec<Either<Form, Skipped>>,
    eof: Position,
}

impl<const ALLOW_PARTIAL_FAILURE: bool> Module<ALLOW_PARTIAL_FAILURE> {
    pub fn children(&self) -> impl Iterator<Item = &Form> {
        self.forms.iter().filter_map(|form| match form {
            Either::A(form) => Some(form),
            Either::B(_) => None,
        })
    }
}

impl<const ALLOW_PARTIAL_FAILURE: bool> Parse for Module<ALLOW_PARTIAL_FAILURE> {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.prev_token_end_position();
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            let start = ts.next_token_start_position()?;
            let result = ts.parse();
            match result {
                Ok(form) => {
                    forms.push(Either::A(form));
                }
                Err(e) if !ALLOW_PARTIAL_FAILURE => {
                    return Err(e);
                }
                Err(e) => loop {
                    if let Some(Ok(token)) = ts.next() {
                        if let LexicalToken::Symbol(token) = token
                            && token.value() == Symbol::Dot {
                                log::warn!(
                                    concat!(
                                        "Skipped formatting a form due to ",
                                        "the following error.\n{}"
                                    ),
                                    e
                                );
                                let end = token.end_position();
                                forms.push(Either::B(Skipped { start, end }));
                                break;
                            }
                    } else {
                        return Err(e);
                    }
                },
            }
        }
        let eof = ts.next_token_start_position()?;
        Ok(Self { sof, forms, eof })
    }
}

impl<const ALLOW_PARTIAL_FAILURE: bool> Format for Module<ALLOW_PARTIAL_FAILURE> {
    fn format(&self, fmt: &mut Formatter) {
        let mut state = FormatState {
            is_last_spec_or_doc: false,
            pending_constants: Vec::new(),
        };
        let mut is_last_fun_decl = false;
        let mut is_last_include_lib = None;

        for form in &self.forms {
            if is_last_fun_decl {
                fmt.write_newlines(3);
                is_last_fun_decl = false;
            }

            let form = match form {
                Either::A(form) => form,
                Either::B(skipped) => {
                    fmt.write_span(&skipped);
                    fmt.write_newline();
                    continue;
                }
            };

            match (is_last_include_lib.take(), form.is_include_lib()) {
                (Some(a), Some(b)) if a != b => {
                    fmt.write_newlines(2);
                }
                _ => {}
            }

            if state.pend_if_need(fmt, form) {
                continue;
            }
            state.flush_pendings(fmt);
            if state.pend_if_need(fmt, form) {
                continue;
            }

            state.insert_two_empty_newlines_if_need(fmt, form);

            form.format(fmt);
            fmt.write_newline();
            is_last_fun_decl = form.is_func_decl();
            is_last_include_lib = form.is_include_lib();
        }

        state.flush_pendings(fmt);
    }
}

struct FormatState<'a> {
    is_last_spec_or_doc: bool,
    pending_constants: Vec<&'a DefineDirective>,
}

impl<'a> FormatState<'a> {
    fn format_aligned_constants(&mut self, fmt: &mut Formatter) {
        if self.pending_constants.len() < 2 {
            return;
        }

        let indent = self
            .pending_constants
            .iter()
            .map(|define| {
                let macro_name_len = define.macro_name_token().end_position().offset()
                    - define.macro_name_token().start_position().offset();
                "-define(".len() + macro_name_len + ", ".len()
            })
            .max()
            .expect("unreachable");

        for constant in self.pending_constants.drain(..) {
            constant.format_with_indent(fmt, Some(indent));
            fmt.write_newline();
        }
    }

    fn flush_pendings(&mut self, fmt: &mut Formatter) {
        self.format_aligned_constants(fmt);

        for constant in self.pending_constants.drain(..) {
            constant.format(fmt);
            fmt.write_newline();
        }
    }

    fn pend_if_need(&mut self, fmt: &Formatter, form: &'a Form) -> bool {
        if let forms::Form::Define(define) = form.get() {
            if define.variables().is_some() {
                return false;
            }

            if let Some(last) = self.pending_constants.last()
                && last.end_position().line() + 1 < define.start_position().line() {
                    return false;
                }

            if fmt.token_stream().contains_comment(define) {
                return false;
            }

            self.pending_constants.push(define);
            true
        } else {
            false
        }
    }

    fn insert_two_empty_newlines_if_need(&mut self, fmt: &mut Formatter, form: &'a Form) {
        if form.is_func_decl() && !self.is_last_spec_or_doc {
            fmt.flush_non_preceding_comments(form);
            fmt.write_newlines(3);
        }

        if form.is_func_spec_or_doc() && !self.is_last_spec_or_doc {
            fmt.flush_non_preceding_comments(form);
            fmt.write_newlines(3);
        }
        self.is_last_spec_or_doc = form.is_func_spec_or_doc();
    }
}

#[derive(Debug, Clone, Span)]
struct Skipped {
    start: Position,
    end: Position,
}
