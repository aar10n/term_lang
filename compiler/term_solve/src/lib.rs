#![feature(box_patterns)]
#![feature(trait_alias)]
#![feature(let_chains)]
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
use term_common as common;
use term_core as core;
use term_diag as diag;

use constraint::cs;
use core::{Constraint, Ef, Expr, MonoVarId, PolyVarId, Ty, TyE, VarId};
use diag::{Diagnostic, IntoDiagnostic};
use term_print::{PrettyPrint, PrettyString};
use type_env::TypeEnv;

use std::collections::{BTreeMap, BTreeSet};

/// Infers the most general type of an expression.
pub fn infer(core: &mut core::Context, e: Expr, trace: bool) -> diag::Result<(Expr, TyE)> {
    let mut ctx = Context::new(core, trace);

    let (e, e_t) = match hm::algorithmj(&mut ctx, e, 0) {
        Ok(u) => Ok(u),
        Err(e) => Err(e),
    }?;
    let t = hm::generalize(&mut ctx, e_t, &mut Default::default());
    Ok((e, t))
}

/// Satisfy a type equation.
pub fn satisfy(
    core: &mut core::Context,
    e: Expr,
    t: TyE,
    trace: bool,
) -> diag::Result<(Expr, TyE)> {
    let (e, e_t) = infer(core, e, trace)?;
    let mut ctx = Context::new(core, trace);
    let t = hm::unify(&mut ctx, t, e_t, 0)?;
    Ok((e, t))
}

macro_rules! trace_println {
    ($ctx:ident, $($arg:tt)*) => {
        if $ctx.trace {
            println!("{}", format!($($arg)*).as_str().trim_end());
        }
    };
}
pub(crate) use trace_println;
