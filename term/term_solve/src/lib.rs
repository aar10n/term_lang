#![feature(box_patterns)]
#![feature(trait_alias)]
#![allow(unused)]
pub mod constraint;
mod context;
pub mod ef;
pub mod hm;
pub mod print;
pub mod ty;
pub mod type_env;
pub mod union_find;

pub use context::*;
use term_core as core;
use term_diag as diag;

use constraint::cs;
use core::{Constraint, Ef, Expr, MonoVarId, PolyVarId, Ty, TyE};
use diag::{Diagnostic, IntoDiagnostic};
use term_print::PrettyString;

use std::collections::{BTreeSet, HashMap};

macro_rules! debug_println {
    ($($arg:tt)*) => {
        println!($($arg)*);
    };
}
pub(crate) use debug_println;

/// Solves a type equation.
pub fn solve(ctx: &mut core::Context, e: &Expr, t: &TyE) -> diag::Result<TyE> {
    let mut ctx = Context::new(ctx);
    let u = match hm::algorithmj(&mut ctx, e.clone(), 0) {
        Ok(u) => Ok(u),
        Err(e) => Err(if let Some(s) = ctx.spans.last().copied() {
            e.with_span(s)
        } else {
            e
        }),
    }?;
    let u = update(&mut ctx, u);
    Ok(unify(&mut ctx, t.clone(), u)?)
}

/// Infers the most general type of an expression.
pub fn infer(ctx: &mut core::Context, e: &Expr) -> diag::Result<TyE> {
    let mut ctx = Context::new(ctx);
    let result = match hm::algorithmj(&mut ctx, e.clone(), 0) {
        Ok(u) => Ok(u),
        Err(e) => Err(if let Some(s) = ctx.spans.last().copied() {
            e.with_span(s)
        } else {
            e
        }),
    }?;
    let t = generalize(&mut ctx, result, &mut Default::default());
    Ok(t)
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

pub fn solve_constraints(
    ctx: &mut Context<'_>,
    mut cs: Vec<Constraint>,
) -> diag::Result<Vec<Constraint>> {
    let mut open = vec![];
    while let Some(c) = cs.pop() {
        match c {
            Constraint::Empty => {}
            Constraint::Eq(box t1, box t2) => {
                let t = unify(ctx, t1.clone(), t2.clone())?;
                if !t.is_concrete() {
                    open.push(Constraint::Eq(t1.into(), t2.into()));
                }
            }
            Constraint::Class(id, ts) => {
                // check that `ts` satisfies the class
                todo!()
            }
        }
    }
    Ok(open)
}

pub fn unify(ctx: &mut Context<'_>, t1: TyE, t2: TyE) -> diag::Result<TyE> {
    let t1 = cannonicalize(ctx, t1);
    let t2 = cannonicalize(ctx, t2);
    if t1 != t2 {
        // debug_println!(
        //     "unify: {} = {}",
        //     t1.pretty_string(ctx),
        //     t2.pretty_string(ctx)
        // );
    }

    let (t1, f1, mut cs) = t1.into_tuple();
    let (t2, f2, cs2) = t2.into_tuple();
    cs.extend(cs2);

    let t = ty::unify(ctx, t1, t2)?;
    let f = ef::unify(ctx, f1, f2)?;
    let cs = solve_constraints(ctx, cs)?;
    Ok(TyE::new(
        ty::update(ctx, t),
        ef::update(ctx, f),
        cs::update(ctx, cs),
    ))
}

pub fn update(ctx: &mut Context<'_>, t: TyE) -> TyE {
    let (t, f, cs) = t.into_tuple();
    TyE::new(ty::update(ctx, t), ef::update(ctx, f), cs::update(ctx, cs))
}

pub fn instantiate(ctx: &mut Context<'_>, t: TyE, ps: &mut HashMap<PolyVarId, MonoVarId>) -> TyE {
    let (t, f, mut cs) = t.into_tuple();
    let (t, f1, cs1) = ty::instantiate(ctx, t, ps).into_tuple();
    let f = ef::instantiate(ctx, f, ps) | f1;
    cs.extend(cs1);
    let cs = cs::instantiate(ctx, cs, ps);
    TyE::new(t, f, cs)
}

pub fn generalize(ctx: &mut Context<'_>, t: TyE, ps: &mut HashMap<MonoVarId, PolyVarId>) -> TyE {
    let (t, f, mut cs) = t.into_tuple();
    let (t, f1, cs1) = ty::generalize(ctx, t, ps).into_tuple();
    let f = ef::generalize(ctx, f, ps) | f1;
    cs.extend(cs1);
    let cs = cs::generalize(ctx, cs, ps);
    TyE::new(t, f, cs)
}

pub fn cannonicalize(ctx: &mut Context<'_>, t: TyE) -> TyE {
    let (t, f, mut cs) = t.into_tuple();
    let (t, f2, cs2) = ty::cannonicalize(ctx, t).into_tuple();
    cs.extend(cs2);

    let f = ef::cannonicalize(ctx, f | f2);
    let cs = cs::cannonicalize(ctx, cs);
    TyE::new(t, f, cs)
}

pub fn ty_occurs(x: &Ty, t: &TyE) -> bool {
    ty::ty_occurs(x, &t.ty) || ef::ty_occurs(x, &t.ef)
}

pub fn ef_occurs(x: &Ef, t: &TyE) -> bool {
    ty::ef_occurs(x, &t.ty) || ef::ef_occurs(x, &t.ef)
}

pub fn subst_ty(r: &Ty, x: &Ty, t: TyE) -> TyE {
    TyE::new(
        ty::subst_ty(r, x, t.ty),
        ef::subst_ty(r, x, t.ef),
        t.cs.into_iter()
            .map(|c| constraint::subst_ty(r, x, c))
            .collect(),
    )
}

pub fn subst_ef(r: &Ef, x: &Ef, t: TyE) -> TyE {
    TyE::new(
        ty::subst_ef(r, x, t.ty),
        ef::subst_ef(r, x, t.ef),
        t.cs.into_iter()
            .map(|c| constraint::subst_ef(r, x, c))
            .collect(),
    )
}
