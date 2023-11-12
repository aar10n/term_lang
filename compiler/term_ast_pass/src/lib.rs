#![allow(unused)]
#![feature(trait_alias)]
#![feature(let_chains)]
#![feature(box_patterns)]
mod collect;
mod infer;
mod lower;
mod resolve;

use term_ast as ast;
use term_ast_lower as ast_lower;
use term_core as core;
use term_diag as diag;

use ast::visit::Visit;
use ast::{Module, Span};
use diag::{Diagnostic, IntoDiagnostic, Report};
use ustr::Ustr;

pub use collect::*;
pub use infer::*;
pub use lower::*;
pub use resolve::*;

/// A duplicate declaration error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DuplicateDeclErr {
    /// The kind of declaration.
    pub kind: &'static str,
    /// The duplicate declaration.
    pub span: Span,
    /// The first declaration.
    pub first: Span,
}

impl IntoDiagnostic for DuplicateDeclErr {
    fn into_diagnostic(self) -> Diagnostic {
        Diagnostic::error(format!("duplicate {} declaration", self.kind), self.span)
            .with_note("first declaration here", self.first)
    }
}

/// An unresolved name error.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnresolvedNameErr {
    /// The kind of reference.
    pub kind: &'static str,
    /// The name.
    pub name: String,
    /// The span of the name.
    pub span: Span,
    /// Conflicting name.
    pub conflict: Option<Span>,
}

impl IntoDiagnostic for UnresolvedNameErr {
    fn into_diagnostic(self) -> Diagnostic {
        let mut diag = Diagnostic::error(
            format!("unresolved {} name `{}`", self.kind, self.name),
            self.span,
        );
        if let Some(conflict) = self.conflict {
            diag = diag.with_note("conflicting name here", conflict);
        }
        diag
    }
}

/// A function that performs an AST pass.
pub trait PassFn<RetT> =
    for<'a> FnMut(&'a mut ast::Context, &'a mut core::Context, &'a mut Module) -> PassResult<RetT>;

pub enum PassResult<T = ()> {
    Ok(T),
    Err(Vec<Diagnostic>),
}

impl<T> PassResult<T> {
    pub fn into_result(self) -> Result<T, Report> {
        match self {
            PassResult::Ok(r) => Ok(r),
            PassResult::Err(ds) => Err(Report::from(ds)),
        }
    }
}

impl<T, I> From<I> for PassResult<T>
where
    I: IntoIterator<Item = Result<T, Vec<Diagnostic>>>,
{
    fn from(iter: I) -> PassResult<T> {
        let mut ds = vec![];
        for r in iter {
            match r {
                Ok(r) => return PassResult::Ok(r),
                Err(mut ds_) => ds.append(&mut ds_),
            }
        }
        PassResult::Err(ds)
    }
}

pub fn combine<T, U>(
    mut a: impl PassFn<T> + 'static,
    mut b: impl PassFn<U> + 'static,
) -> Box<dyn PassFn<U>> {
    use PassResult::*;
    let closure: Box<dyn PassFn<U>> = Box::new(move |ast, core, module| -> PassResult<U> {
        match a(ast, core, module) {
            Ok(_) => match b(ast, core, module) {
                Ok(r) => Ok(r),
                Err(ds) => Err(ds),
            },
            Err(ds) => Err(ds),
        }
    });
    closure
}

#[macro_export]
macro_rules! compose {
    ($pass:expr) => {
        $pass
    };
    ($pass:expr, $($passes:expr),+) => {
        $crate::combine($pass, $crate::compose!($($passes),+))
    };
}
