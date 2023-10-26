#![feature(box_patterns)]
#![feature(trait_alias)]
#![feature(let_chains)]
#![allow(unused)]
pub mod constraint;
mod context;
pub mod ef;
pub mod hm;
pub mod print;
pub mod topo_sort;
pub mod ty;
pub mod type_env;
pub mod union_find;

pub use context::*;
use term_core as core;
use term_diag as diag;
use term_print::{PrettyPrint, PrettyString};

use constraint::cs;
use core::{Constraint, Ef, Expr, MonoVarId, PolyVarId, Ty, TyE, VarId};
use diag::{Diagnostic, IntoDiagnostic};
use topo_sort::TopologicalSort;
use type_env::TypeEnv;

use std::collections::{BTreeMap, BTreeSet};

pub struct Context<'ctx> {
    pub core: &'ctx mut core::Context,
    pub solve: &'ctx mut context::TyContext,
    pub trace: bool,
}

impl<'ctx> Context<'ctx> {
    pub fn new(
        core: &'ctx mut core::Context,
        solve: &'ctx mut context::TyContext,
        trace: bool,
    ) -> Self {
        Self { core, solve, trace }
    }

    fn new_normal(core: &'ctx mut core::Context, solve: &'ctx mut context::TyContext) -> Self {
        Self {
            core,
            solve,
            trace: false,
        }
    }

    fn new_tracing(core: &'ctx mut core::Context, solve: &'ctx mut context::TyContext) -> Self {
        Self {
            core,
            solve,
            trace: true,
        }
    }

    fn new_ty_var(&mut self) -> Ty {
        let id = self.core.ids.next_mono_var_id();
        self.solve.ty_set.insert(Ty::Mono(id));
        Ty::Mono(id)
    }

    fn new_ef_var(&mut self) -> Ef {
        let id = self.core.ids.next_mono_var_id();
        self.solve.ef_set.insert(Ef::Mono(id));
        Ef::Mono(id)
    }
}

impl<'ctx> From<(&'ctx mut core::Context, &'ctx mut context::TyContext)> for Context<'ctx> {
    fn from((core, solve): (&'ctx mut core::Context, &'ctx mut context::TyContext)) -> Self {
        Self::new_normal(core, solve)
    }
}

//
//

/// Infers the most general type of an expression.
pub fn infer(
    core: &mut core::Context,
    solve: &mut context::TyContext,
    e: Expr,
    trace: bool,
) -> diag::Result<(Expr, TyE)> {
    let mut ctx = if trace {
        Context::new_tracing(core, solve)
    } else {
        Context::new_normal(core, solve)
    };

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
    solve: &mut context::TyContext,
    e: Expr,
    t: &TyE,
) -> diag::Result<(Expr, TyE)> {
    let mut ctx = Context::new_normal(core, solve);
    let (e, e_t) = match hm::algorithmj(&mut ctx, e, 0) {
        Ok(u) => Ok(u),
        Err(e) => Err(e),
    }?;
    let u = hm::update(&mut ctx, e_t);
    let u = hm::unify(&mut ctx, t.clone(), u, 0)?;
    Ok((e, u))
}

pub fn sort_dependencies(ctx: &core::Context) -> Vec<VarId> {
    let mut topo = TopologicalSort::<VarId>::new();
    for (id, depends_on) in &ctx.dep_graph {
        topo.declare_item(*id);
        for dep_id in depends_on {
            topo.add_dependency(dep_id.clone(), id.clone());
        }
    }

    let mut out = vec![];
    while let mut deps = topo.pop() && !deps.is_empty() {
        deps.sort();
        out.extend(deps);
    }
    out
}

macro_rules! debug_println {
    ($ctx:ident, $($arg:tt)*) => {
        if $ctx.trace {
            println!("{}", format!($($arg)*).as_str().trim_end());
        }
    };
}
pub(crate) use debug_println;
