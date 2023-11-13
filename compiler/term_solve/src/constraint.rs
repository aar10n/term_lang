use crate::{ef, hm, trace_println, ty, Context};
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Constraint, Ef, MonoVarId, PolyVarId, Ty, TyE};
use diag::{IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};

use std::collections::HashMap;
use ustr::Ustr;

pub fn class_constraint(
    ctx: &mut Context,
    name: impl AsRef<str>,
    ts: impl IntoIterator<Item = TyE>,
) -> diag::Result<(Vec<Ty>, Vec<Constraint>)> {
    let name = name.as_ref();
    let class_id = ctx
        .core
        .global_types
        .get(&Ustr::from(name))
        .copied()
        .ok_or(format!("class `{}` not found", name).into_diagnostic())?
        .class_id();

    let class = ctx.core.classes[&class_id].clone();
    let class = class.borrow();

    let mut ts = ts.into_iter().collect::<Vec<_>>();
    if ts.len() > class.ps.len() {
        return format!(
            "class `{}` expects {} arguments, but got {}",
            name,
            class.ps.len(),
            ts.len()
        )
        .into_err();
    }

    // instantiate the class and associated constraints
    //   - first create a mapping from the class parameters to fresh monotypes
    let mut ps = HashMap::new();
    for p in class.ps.iter() {
        let mono = ctx.core.ids.next_mono_var_id();
        ps.insert(p.clone(), mono);
    }
    //   - then instantiate the constraints
    let mut cs = cs::instantiate(ctx, class.cs.clone(), &mut ps);
    //   - then substitute parameters with the given types
    for (mono, t) in class.ps.iter().map(|p| ps[p]).clone().zip(ts.iter()) {
        cs = cs::subst_ty(&Ty::Mono(mono), t, cs);
    }
    //   - finally collect the unspecialized parameters
    let mut out_ps = vec![];
    for v in class.ps.iter().skip(ts.len()).map(|p| ps[p]) {
        out_ps.push(Ty::Mono(v));
        ts.push(TyE::pure(Ty::Mono(v)));
    }

    // finally add the class constraint
    cs.push(Constraint::Class(class_id, ts));
    Ok((out_ps, cs))
}

pub fn update(ctx: &mut Context, c: Constraint) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(hm::update(ctx, t1).into(), hm::update(ctx, t2).into()),
        Class(id, ts) => Class(
            id,
            ts.into_iter()
                .map(|t| hm::update(ctx, t))
                .collect::<Vec<_>>(),
        ),
    }
}

pub fn instantiate(
    ctx: &mut Context,
    c: Constraint,
    ps: &mut HashMap<PolyVarId, MonoVarId>,
) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(
            hm::instantiate(ctx, t1, ps).into(),
            hm::instantiate(ctx, t2, ps).into(),
        ),
        Class(id, ts) => Class(
            id,
            ts.into_iter()
                .map(|t| hm::instantiate(ctx, t, ps))
                .collect(),
        ),
    }
}

pub fn generalize(
    ctx: &mut Context,
    c: Constraint,
    ps: &mut HashMap<MonoVarId, PolyVarId>,
) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(
            hm::generalize(ctx, t1, ps).into(),
            hm::generalize(ctx, t2, ps).into(),
        ),
        Class(id, ts) => Class(
            id,
            ts.into_iter().map(|t| hm::generalize(ctx, t, ps)).collect(),
        ),
    }
}

pub fn cannonicalize(ctx: &mut Context, c: Constraint) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(
            hm::cannonicalize(ctx, t1).into(),
            hm::cannonicalize(ctx, t2).into(),
        ),
        Class(id, ts) => Class(
            id,
            ts.into_iter()
                .map(|t| hm::cannonicalize(ctx, t))
                .collect::<Vec<_>>(),
        ),
    }
}

pub fn subst_ty(r: &Ty, x: &Ty, c: Constraint) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(hm::subst_ty(r, x, t1).into(), hm::subst_ty(r, x, t2).into()),
        Class(id, ts) => Class(id, ts.into_iter().map(|t| hm::subst_ty(r, x, t)).collect()),
    }
}

pub fn subst_ef(r: &Ef, x: &Ef, c: Constraint) -> Constraint {
    use Constraint::*;
    match c {
        Empty => Empty,
        Eq(box t1, box t2) => Eq(hm::subst_ef(r, x, t1).into(), hm::subst_ef(r, x, t2).into()),
        Class(id, ts) => Class(id, ts.into_iter().map(|t| hm::subst_ef(r, x, t)).collect()),
    }
}

pub mod cs {
    use super::*;

    pub fn update(ctx: &mut Context, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter().map(|c| super::update(ctx, c)).collect()
    }

    pub fn instantiate(
        ctx: &mut Context,
        cs: Vec<Constraint>,
        ps: &mut HashMap<PolyVarId, MonoVarId>,
    ) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| super::instantiate(ctx, c, ps))
            .collect()
    }

    pub fn generalize(
        ctx: &mut Context,
        cs: Vec<Constraint>,
        ps: &mut HashMap<MonoVarId, PolyVarId>,
    ) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| super::generalize(ctx, c, ps))
            .collect()
    }

    pub fn cannonicalize(ctx: &mut Context, mut cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.sort();
        cs.dedup();
        cs.into_iter()
            .map(|c| super::cannonicalize(ctx, c))
            .collect()
    }

    pub fn subst_ty(r: &Ty, x: &Ty, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter().map(|c| super::subst_ty(r, x, c)).collect()
    }

    pub fn subst_ef(r: &Ef, x: &Ef, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter().map(|c| super::subst_ef(r, x, c)).collect()
    }
}
