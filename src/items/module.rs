use crate::format::{Format, Formatter};
use crate::items::forms::DefineDirective;
use crate::items::{forms, Form};
use crate::parse::{self, Parse, TokenStream};
use crate::span::{Position, Span};
use std::num::NonZeroUsize;

const THREE: NonZeroUsize = unsafe { NonZeroUsize::new_unchecked(3) };

/// [Form]*
#[derive(Debug, Clone, Span)]
pub struct Module {
    sof: Position,
    forms: Vec<Form>,
    eof: Position,
}

impl Parse for Module {
    fn parse(ts: &mut TokenStream) -> parse::Result<Self> {
        let sof = ts.prev_token_end_position();
        let mut forms = Vec::new();
        while !ts.is_eof()? {
            forms.push(ts.parse()?);
        }
        let eof = ts.next_token_start_position()?;
        Ok(Self { sof, forms, eof })
    }
}

impl Format for Module {
    fn format(&self, fmt: &mut Formatter) {
        let mut state = FormatState {
            is_last_spec: false,
            pending_constants: Vec::new(),
        };
        let mut is_last_fun_decl = false;

        for form in &self.forms {
            if is_last_fun_decl {
                fmt.write_newlines(THREE);
                is_last_fun_decl = false;
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
        }

        state.flush_pendings(fmt);
    }
}

struct FormatState<'a> {
    is_last_spec: bool,
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
            .map(|define| "-define(".len() + define.macro_name().len() + ", ".len())
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

            if let Some(last) = self.pending_constants.last() {
                if last.end_position().line() + 1 < define.start_position().line() {
                    return false;
                }
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
        if form.is_func_decl() && !self.is_last_spec {
            fmt.flush_non_preceding_comments(form);
            fmt.write_newlines(THREE);
        }

        self.is_last_spec = form.is_func_spec();
        if form.is_func_spec() {
            fmt.flush_non_preceding_comments(form);
            fmt.write_newlines(THREE);
        }
    }
}
