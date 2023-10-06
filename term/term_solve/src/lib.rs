#![feature(box_patterns)]
#![feature(trait_alias)]
#![allow(unused)]
mod context;
mod hm;
mod typenv;
pub mod union_find;

pub use context::*;
use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Constraint, Expr, PolyVarId, Ty, TyE};
use diag::{Diagnostic, IntoDiagnostic};
use print::PrettyPrint;

use std::collections::BTreeSet;

pub fn solve<T: AsMut<core::Context>>(ctx: &mut T, e: &Expr, t: &TyE) -> diag::Result<TyE> {
    let mut ctx = Context::new(ctx.as_mut());
    let u = hm::algorithmj(&mut ctx, e.clone())?;
    let u = hm::update(&mut ctx, u);
    Ok(hm::unify(&mut ctx, t.clone(), u)?)
}

pub fn infer(ctx: &mut core::Context, e: &Expr) -> diag::Result<TyE> {
    let mut ctx = Context::new(ctx);
    let result = hm::algorithmj(&mut ctx, e.clone())?;
    let result = hm::generalize(&mut ctx, result, &mut Default::default());
    Ok(hm::update(&mut ctx, result))
}

/// Checks that a set of type parameters are valid.
///
/// This function validates that all non-empty constraints reference at least one
/// bound type parameter.
pub fn check_valid_type_params(
    ctx: &mut core::Context,
    params: &[PolyVarId],
    constraints: &[Constraint],
) -> diag::Result<()> {
    let ps = BTreeSet::from_iter(params.iter().map(|x| *x));
    for c in constraints {
        match c {
            Constraint::Empty => {}
            Constraint::Eq(box t1, box t2) => todo!(),
            Constraint::Class(id, ts) => todo!(),
        }
    }
    Ok(())
}

// /// Checks that a set of type arguments are valid for a set of type parameters.
// pub fn check_valid_type_args(
//     ctx: &mut core::Context,
//     args: &[TyE],
//     params: &[PolyVarId],
//     constraints: &[Constraint],
// ) -> diag::Result<()> {
//     if args.len() != params.len() {
//         Diagnostic::new(level, message, span)
//         return Err(Diagnostic::error().with_message("wrong number of type arguments"));
//     }
// }
