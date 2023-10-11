#![allow(unused)]
#![feature(trait_alias)]
#![feature(let_chains)]
#![feature(box_patterns)]
mod collect;
pub mod lower;
mod lower_decl;
mod lower_expr;
mod lower_impl;
mod resolve;

use term_ast as ast;
use term_core as core;
use term_diag as diag;

use ast::{Module, Span};
use diag::{Diagnostic, IntoDiagnostic, Report};
use ustr::Ustr;

pub use ast::Context;
pub use collect::*;
pub use lower_decl::*;
pub use lower_expr::*;
pub use lower_impl::*;
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
pub trait PassFn = for<'v, 'ast> FnMut(&'v mut Context<'ast>, &'v mut ast::Module) -> PassResult;

pub enum PassResult {
    Ok(Vec<Diagnostic>),
    Err(Vec<Diagnostic>),
}

/// Applies the pass function to the given module.
pub fn apply<'a>(
    ctx: &'a mut Context,
    module: &'a mut Module,
    mut f: impl PassFn,
) -> Result<(), Vec<Diagnostic>> {
    match f(ctx, module) {
        PassResult::Ok(ds) => {
            if !ds.is_empty() {
                Report::from(ds).print_stderr(&ctx.sources).unwrap();
            }
            Ok(())
        }
        PassResult::Err(ds) => Err(ds),
    }
}

/// A pass composed of multiple passes.
///
/// The passes are applied in order until one of them returns an error.
pub fn composite(mut passes: Vec<Box<dyn PassFn>>) -> Box<dyn PassFn> {
    use PassResult::*;
    let closure: Box<dyn PassFn> = Box::new(move |ctx, module| {
        let mut results = vec![];
        for pass in passes.iter_mut() {
            match pass(ctx, module) {
                Ok(ds) => {
                    results.extend(ds);
                }
                Err(ds) => {
                    results.extend(ds);
                    return PassResult::Err(results);
                }
            }
        }
        PassResult::Ok(results)
    });
    closure
}

#[macro_export]
macro_rules! compose {
    ($($pass:expr),*) => {
        $crate::composite(vec![$(Box::new($pass)),*])
    };
}
