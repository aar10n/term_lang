use crate::{debug_println, ef, Context};
use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Ef, EffectId, MonoVarId, PolyVarId, Ty, TyE};
use diag::IntoDiagnostic;
use print::{PrettyPrint, PrettyString, TABWIDTH};

use std::collections::{BTreeMap, HashMap};

pub fn unify(ctx: &mut Context<'_>, t1: Ty, t2: Ty, level: usize) -> diag::Result<Ty> {
    if t1 == t2 {
        return Ok(t1);
    }

    let tab = TABWIDTH.repeat(level);
    debug_println!(
        "{tab}unify_ty: {} = {}",
        t1.pretty_string(ctx),
        t2.pretty_string(ctx)
    );

    use Ty::*;
    Ok(match (t1, t2) {
        (Infer, t) | (t, Infer) => t,
        (Never, t) => t,
        (Unit, Unit) => Unit,
        (Symbol(x), Symbol(y)) if x == y => Symbol(x),
        (t, Mono(x)) | (Mono(x), t) => {
            let tt = t.clone();
            let t = update(ctx, t);
            // debug_println!(
            //     "find({}) -> {}",
            //     tt.pretty_string(ctx),
            //     t.pretty_string(ctx)
            // );

            if ty_occurs(&Mono(x), &t) {
                return Err(
                    format!("recursive type `{}` contains itself", t.pretty_string(ctx))
                        .into_diagnostic(),
                );
            }
            let result = ctx.ty_set.union(t.clone(), Mono(x));
            // debug_println!(
            //     "union({}, {}) -> {}",
            //     t.pretty_string(ctx),
            //     Mono(x).pretty_string(ctx),
            //     result.pretty_string(ctx)
            // );
            result
        }
        (Rec(id1, box t1), Rec(id2, box t2)) if id1 == id2 => {
            let t = crate::unify(ctx, t1, t2, level + 1)?;
            Rec(id1, t.into())
        }
        (Data(id1, ts1), Data(id2, ts2)) if id1 == id2 => {
            if ts1.len() != ts2.len() {
                return Err(format!(
                    "cannot unify data types of different lengths: {} and {}",
                    ts1.len(),
                    ts2.len()
                )
                .into_diagnostic());
            }

            let ts = ts1
                .into_iter()
                .zip(ts2.into_iter())
                .map(|(t1, t2)| crate::unify(ctx, t1, t2, level + 1))
                .collect::<Result<Vec<_>, _>>()?;
            Data(id1, ts)
        }
        (Func(box t1, box t2), Func(box t3, box t4)) => {
            let t1 = crate::unify(ctx, t1, t3, level + 1)?;
            let t2 = crate::unify(ctx, t2, t4, level + 1)?;
            Func(t1.into(), t2.into())
        }
        (Record(fds1), Record(fds2)) => {
            let mut fields = BTreeMap::new();
            for (name, t1) in fds1 {
                let t2 = fds2.get(&name).ok_or_else(|| {
                    format!("field `{}` not found in record type", name).into_diagnostic()
                })?;
                let t = crate::unify(ctx, t1, t2.clone(), level + 1)?;
                fields.insert(name, t);
            }
            Record(fields)
        }
        (Effectful(box t1, f1), Effectful(box t2, f2)) => {
            let t = unify(ctx, t1, t2, level + 1)?;
            let f = ef::unify(ctx, f1, f2, level + 1)?;
            Effectful(t.into(), f)
        }
        (Effectful(box ft, f), t) | (t, Effectful(box ft, f)) => {
            let t = unify(ctx, ft, t, level + 1)?;
            let f = ef::unify(ctx, f, Ef::Pure, level + 1)?;
            t
        }
        (Poly(_), _) | (_, Poly(_)) => panic!("unexpected poly_var in type"),
        (t1, t2) => {
            println!(
                "expected type `{}`, found `{}`",
                t1.pretty_string(ctx),
                t2.pretty_string(ctx),
            );
            panic!();

            return Err(format!(
                "expected type `{}`, found `{}`",
                t1.pretty_string(ctx),
                t2.pretty_string(ctx),
            )
            .into_diagnostic());
        }
    })
}

pub fn update(ctx: &mut Context<'_>, t: Ty) -> Ty {
    use Ty::*;
    match t {
        Mono(x) => ctx.ty_set.find(Mono(x)),
        Rec(id, box t) => Rec(id, crate::update(ctx, t).into()),
        Data(id, ts) => Data(id, ts.into_iter().map(|t| crate::update(ctx, t)).collect()),
        Func(box t1, box t2) => Func(crate::update(ctx, t1).into(), crate::update(ctx, t2).into()),
        Record(fields) => Record(
            fields
                .into_iter()
                .map(|(k, v)| (k, crate::update(ctx, v)))
                .collect(),
        ),
        Effectful(box t, f) => Effectful(update(ctx, t).into(), f),
        t => t,
    }
}

pub fn instantiate(ctx: &mut Context<'_>, t: Ty, ps: &mut HashMap<PolyVarId, MonoVarId>) -> TyE {
    use Ty::*;
    match t {
        Poly(id) => match ps.get(&id) {
            Some(&id) => TyE::pure(Mono(id)),
            Some(_) => panic!("invalid poly var"),
            None => {
                let t = ctx.new_mono_var();
                ps.insert(id, t);
                let t = Ty::Mono(t);
                ctx.ty_set.insert(t.clone());
                TyE::pure(t)
            }
        },
        Rec(id, box t) => {
            let t = crate::instantiate(ctx, t, ps);
            TyE::pure(Rec(id, t.into()))
        }
        Data(id, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| crate::instantiate(ctx, t, ps))
                .collect();
            TyE::pure(Data(id, ts))
        }
        Func(box t1, box t2) => {
            let t1 = crate::instantiate(ctx, t1, ps);
            let t2 = crate::instantiate(ctx, t2, ps);
            TyE::pure(Func(t1.into(), t2.into()))
        }
        Record(fields) => {
            let fields = fields
                .into_iter()
                .map(|(k, v)| (k, crate::instantiate(ctx, v, ps)))
                .collect();
            TyE::pure(Record(fields))
        }
        Effectful(box t, f) => {
            let (t, f2, cs) = instantiate(ctx, t, ps).into_tuple();
            let f = ef::instantiate(ctx, f, ps) | f2;
            TyE::new(t, f, cs)
        }
        t @ _ => TyE::pure(t),
    }
}

pub fn generalize(ctx: &mut Context<'_>, t: Ty, ps: &mut HashMap<MonoVarId, PolyVarId>) -> TyE {
    use Ty::*;
    match t {
        Mono(id) => match ps.get(&id) {
            Some(&id) => TyE::pure(Poly(id)),
            Some(_) => panic!("invalid mono var"),
            None => {
                let t = ctx.ids.next_poly_var_id();
                ps.insert(id, t);
                TyE::pure(Ty::Poly(t))
            }
        },
        Rec(id, box t) => {
            let t = crate::generalize(ctx, t, ps);
            TyE::pure(Rec(id, t.into()))
        }
        Data(id, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| crate::generalize(ctx, t, ps))
                .collect();
            TyE::pure(Data(id, ts))
        }
        Func(box t1, box t2) => {
            let t1 = crate::generalize(ctx, t1, ps);
            let t2 = crate::generalize(ctx, t2, ps);
            TyE::pure(Func(t1.into(), t2.into()))
        }
        Record(fields) => {
            let fields = fields
                .into_iter()
                .map(|(k, v)| (k, crate::generalize(ctx, v, ps)))
                .collect();
            TyE::pure(Record(fields))
        }
        Effectful(box t, f) => {
            let (t, f2, cs) = generalize(ctx, t, ps).into_tuple();
            let f = ef::generalize(ctx, f, ps) | f2;
            TyE::new(t, f, cs)
        }
        t @ _ => TyE::pure(t),
    }
}

pub fn cannonicalize(ctx: &mut Context<'_>, t: Ty) -> TyE {
    use Ty::*;
    match t {
        Rec(id, box t) => {
            let t = crate::cannonicalize(ctx, t);
            TyE::pure(Rec(id, t.into()))
        }
        Data(id, ts) => {
            let (ts, f, cs) =
                ts.into_iter()
                    .fold((vec![], Ef::Infer, vec![]), |(mut ts, f, mut cs), t| {
                        let (t, f1, cs1) = crate::cannonicalize(ctx, t).into_tuple();
                        ts.push(TyE::pure(t));
                        cs.extend(cs1);
                        (ts, f | f1, cs)
                    });
            TyE::new(Data(id, ts), f, cs)
        }
        Func(box t1, box t2) => {
            let (t1, f1, mut cs) = crate::cannonicalize(ctx, t1).into_tuple();
            let (t2, f2, cs2) = crate::cannonicalize(ctx, t2).into_tuple();
            cs.extend(cs2);
            TyE::new(
                Ty::Func(TyE::pure(t1).into(), TyE::pure(t2).into()).into(),
                f1 | f2,
                cs,
            )
        }
        Record(fields) => {
            let fields = fields
                .into_iter()
                .map(|(k, v)| (k, crate::cannonicalize(ctx, v)))
                .collect();
            TyE::pure(Record(fields))
        }
        Effectful(box t, f) => {
            let (t, f2, cs) = cannonicalize(ctx, t).into_tuple();
            let f = ef::cannonicalize(ctx, f | f2);
            TyE::new(t, f, cs)
        }
        _ => TyE::pure(t),
    }
}

pub fn ty_occurs(x: &Ty, t: &Ty) -> bool {
    if x == t {
        return true;
    }

    use Ty::*;
    match t {
        Rec(_, box t) => crate::ty_occurs(x, t),
        Data(_, ts) => ts.iter().any(|t| crate::ty_occurs(x, t)),
        Func(box t1, box t2) => crate::ty_occurs(x, &t1) || crate::ty_occurs(x, &t2),
        Record(fields) => fields.values().any(|v| crate::ty_occurs(x, &v)),
        Effectful(box t, f) => ty_occurs(x, t) || ef::ty_occurs(x, f),
        _ => false,
    }
}

pub fn ef_occurs(x: &Ef, t: &Ty) -> bool {
    use Ty::*;
    match t {
        Rec(_, box t) => crate::ef_occurs(x, t),
        Data(_, ts) => ts.iter().any(|t| crate::ef_occurs(x, t)),
        Func(box t1, box t2) => crate::ef_occurs(x, &t1) || crate::ef_occurs(x, &t2),
        Record(fields) => fields.values().any(|v| crate::ef_occurs(x, &v)),
        Effectful(box t, f) => ef::ef_occurs(x, f),
        _ => false,
    }
}

pub fn subst_ty(r: &Ty, x: &Ty, t: Ty) -> Ty {
    if &t == x {
        return r.clone();
    }

    use Ty::*;
    match t {
        Rec(id, box t) => Rec(id, crate::subst_ty(r, x, t).into()),
        Data(id, ts) => Data(
            id,
            ts.into_iter().map(|t| crate::subst_ty(r, x, t)).collect(),
        ),
        Func(box t1, box t2) => Func(
            crate::subst_ty(r, x, t1).into(),
            crate::subst_ty(r, x, t2).into(),
        ),
        Record(fields) => Record(
            fields
                .into_iter()
                .map(|(k, v)| (k, crate::subst_ty(r, x, v)))
                .collect(),
        ),
        Effectful(box t, f) => Effectful(subst_ty(r, x, t).into(), ef::subst_ty(r, x, f)),
        _ => t,
    }
}

pub fn subst_ef(r: &Ef, x: &Ef, t: Ty) -> Ty {
    use Ty::*;
    match t {
        Data(id, ts) => Data(
            id,
            ts.into_iter().map(|t| crate::subst_ef(r, x, t)).collect(),
        ),
        Func(box t1, box t2) => Func(
            crate::subst_ef(r, x, t1).into(),
            crate::subst_ef(r, x, t2).into(),
        ),
        Record(fields) => Record(
            fields
                .into_iter()
                .map(|(k, v)| (k, crate::subst_ef(r, x, v)))
                .collect(),
        ),
        Effectful(box t, f) => Effectful(t.into(), ef::subst_ef(r, x, f)),
        _ => t,
    }
}
