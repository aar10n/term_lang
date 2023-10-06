use crate::{typenv, Context};
use term_core::*;
use term_diag as diag;
use term_print as print;

use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use typenv::TSet;

use std::collections::{BTreeMap, HashMap, HashSet};

macro_rules! debug_println {
    ($($arg:tt)*) => {
        // println!($($arg)*);
    };
}

pub struct PatSolution {
    pub vars: Vec<(Expr, TyE)>,
    pub handler: Option<Ef>,
}

impl PatSolution {
    pub fn new(vars: Vec<(Expr, TyE)>, handler: Option<Ef>) -> Self {
        Self { vars, handler }
    }
}

pub fn algorithmj(ctx: &mut Context<'_>, e: Expr) -> diag::Result<TyE> {
    use self::Bind::*;
    use self::Expr::*;
    use self::Lit::*;
    Ok(match e {
        Type(box t) => t,
        Lit(l) => TyE::pure(l.as_ty()),
        Sym(s) => todo!(),
        Var(id) => {
            let (e, t) = if let Some(d) = ctx.defs.get(&id) {
                (d.body.clone(), d.ty.clone())
            } else {
                match ctx.resolve_local_var(id) {
                    Some((e, t)) => (e, t),
                    None => {
                        if let Some(span) = ctx.id_as_span(id) {
                            return Diagnostic::error(
                                format!("name not found: {}", id.pretty_string(ctx)),
                                span,
                            )
                            .into_err();
                        } else {
                            return Err(format!("name not found: {}", id.pretty_string(ctx))
                                .into_diagnostic());
                        }
                    }
                }
            };

            let t = instantiate(ctx, t, &mut HashMap::new());
            if matches!(e, Expr::Var(_)) || t.is_concrete() {
                return Ok(t);
            }

            let u = algorithmj(ctx, e)?;
            let result = unify(ctx, t, u)?;
            debug_println!(
                "[var] {} : {}",
                id.pretty_string(ctx),
                result.pretty_string(ctx)
            );
            result
        }
        Apply(box e1, box e2) => {
            let t1 = algorithmj(ctx, e1.clone())?;
            let t2 = algorithmj(ctx, e2.clone())?;

            // let f = ctx.new_ef_var();
            let f = Ef::Pure;
            let result = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);
            let rt = unify(
                ctx,
                t1.clone(),
                TyE::new(
                    Ty::Func(t2.clone().into(), result.clone().into()),
                    f,
                    vec![],
                ),
            )?;

            let result = update(ctx, result);
            debug_println!(
                "[app] ({} : {} | {} : {}) -> {}",
                e1.pretty_string(ctx),
                t1.pretty_string(ctx),
                e2.pretty_string(ctx),
                t2.pretty_string(ctx),
                rt.pretty_string(ctx),
            );
            result
        }
        Lambda(x, box e) => {
            // let f = ctx.new_ef_var();
            let v = ctx.new_ty_var();
            let f = Ef::Pure;
            let t = TyE::new(v.clone(), f.clone(), vec![]);
            // debug_println!("[lam] (λ{} : {}) -> {}", x.pretty_string(ctx), t.pretty_string(ctx), e.pretty_string(ctx));

            ctx.push_typing(Expr::Var(x), t.clone());
            let rt = algorithmj(ctx, e.clone())?;
            ctx.pop_typings();

            let pt = update(ctx, t);
            let result = TyE::new(Ty::Func(pt.clone().into(), rt.clone().into()), f, vec![]);
            debug_println!(
                "[lam] (λ{} : {}) -> {}",
                x.pretty_string(ctx),
                pt.pretty_string(ctx),
                rt.pretty_string(ctx),
            );
            result
        }
        Let(Rec(x, box e), e1) => {
            // let f = ctx.new_ef_var();
            let f = Ef::Pure;
            let t = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);

            let ft = TyE::new(
                Ty::Func(t.clone().into(), t.clone().into()),
                f.clone(),
                vec![],
            );
            ctx.push_typing(Expr::Var(x), ft);
            let u = algorithmj(ctx, e.clone())?;
            ctx.pop_typings();

            let t = unify(ctx, t, u)?;
            let ft = TyE::new(
                Ty::Func(t.clone().into(), t.clone().into()),
                f.clone(),
                vec![],
            );
            let mut result = t.clone();
            if let Some(box e1) = e1 {
                ctx.push_typing(Expr::Var(x), ft);
                result = algorithmj(ctx, e1.clone())?;
                ctx.pop_typings();
            }

            debug_println!(
                "[let] {} : {} | {} : {}",
                x.pretty_string(ctx),
                t.pretty_string(ctx),
                e.pretty_string(ctx),
                result.pretty_string(ctx),
            );
            result
        }
        Let(NonRec(box p, box e), e1) => {
            let t = algorithmj(ctx, e.clone())?;
            let PatSolution { vars, handler } = solve_pat(ctx, p.clone(), t.clone())?;

            let mut result = t.clone();
            if let Some(box e1) = e1 {
                ctx.push_typings(vars);
                result = algorithmj(ctx, e1.clone())?;
                ctx.pop_typings();
            }

            debug_println!(
                "[let] {} : {} | {} : {}",
                p.pretty_string(ctx),
                t.pretty_string(ctx),
                e.pretty_string(ctx),
                result.pretty_string(ctx),
            );
            result
        }
        Case(box e, alts) => {
            let case_t = algorithmj(ctx, e.clone())?;
            let mut res_t = Ty::Infer;
            let mut res_f = Ef::Infer;
            for (p, e) in alts {
                let PatSolution { vars, handler } = solve_pat(ctx, p.clone(), case_t.clone())?;
                if let Some(ef) = handler {
                    todo!("associate handler with effect in context")
                }

                ctx.push_typings(vars);
                let (e_t, e_f, _) = algorithmj(ctx, e.clone())?.into_tuple();
                ctx.pop_typings();

                res_t = ty::unify(ctx, res_t, e_t)?;
                res_f = res_f | e_f;
            }
            TyE::new(res_t, res_f, vec![])
        }

        Wildcard => todo!(),
        EfCon(_, _) => todo!(),
        Do(_) => todo!(),
    })
}

pub fn solve_pat(ctx: &mut Context<'_>, p: Expr, t: TyE) -> diag::Result<PatSolution> {
    use Expr::*;
    use Ty::*;

    let (t, f, cs) = t.into_tuple();
    let cs = solve_constraints(ctx, cs)?;
    Ok(match (p, t) {
        (Wildcard, _) => PatSolution::new(vec![], None),
        (Lit(l), t) => {
            let t1 = TyE::pure(l.as_ty());
            let t2 = TyE::new(t, Ef::Pure, vec![]);
            unify(ctx, t1, t2)?;
            PatSolution::new(vec![], None)
        }
        (Var(x), t) => PatSolution::new(vec![(Var(x), TyE::pure(t))], None),
        (EfCon(eff_id, ts1), _) => {
            let mut fs = f.into_hashset();
            let f = Ef::Effect(eff_id, ts1.clone());
            if !fs.contains(&f) {
                return format!(
                    "effect `{}` found when expecting effect of `{}`",
                    ctx.id_as_str(eff_id),
                    ts1.iter()
                        .map(|t| t.pretty_string(ctx))
                        .collect::<Vec<_>>()
                        .join(", "),
                )
                .into_err();
            }
            PatSolution::new(vec![], Some(f))
        }
        (p, t) => {
            return format!(
                "pattern `{}` does not match type `{}`",
                p.pretty_string(ctx),
                t.pretty_string(ctx),
            )
            .into_err()
        }
    })
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

mod ty {
    use super::*;
    use crate::Context;

    pub fn unify(ctx: &mut Context<'_>, t1: Ty, t2: Ty) -> diag::Result<Ty> {
        use Ty::*;
        Ok(match (t1, t2) {
            (Infer, t) | (t, Infer) => t,
            (Never, Never) => Never,
            (Unit, Unit) => Unit,
            (Symbol(x), Symbol(y)) if x == y => Symbol(x),
            (t, Mono(x)) | (Mono(x), t) => {
                let tt = t.clone();
                let t = ty::update(ctx, t);
                debug_println!(
                    "find({}) -> {}",
                    tt.pretty_string(ctx),
                    t.pretty_string(ctx)
                );

                if ty_occurs(&Mono(x), &t) {
                    return Err(format!(
                        "recursive type `{}` contains itself",
                        t.pretty_string(ctx)
                    )
                    .into_diagnostic());
                }
                let result = ctx.ty_set.union(t.clone(), Mono(x));
                debug_println!(
                    "union({}, {}) -> {}",
                    t.pretty_string(ctx),
                    Mono(x).pretty_string(ctx),
                    result.pretty_string(ctx)
                );
                result
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
                    .map(|(t1, t2)| super::unify(ctx, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?;
                Data(id1, ts)
            }
            (Func(box t1, box t2), Func(box t3, box t4)) => {
                let t1 = super::unify(ctx, t1, t3)?;
                let t2 = super::unify(ctx, t2, t4)?;
                Func(t1.into(), t2.into())
            }
            (Sum(ts1), Sum(ts2)) => {
                if ts1.len() != ts2.len() {
                    return Err(format!(
                        "cannot unify sum types of different lengths: {} and {}",
                        ts1.len(),
                        ts2.len()
                    )
                    .into_diagnostic());
                }

                let ts = ts1
                    .into_iter()
                    .zip(ts2.into_iter())
                    .map(|(t1, t2)| super::unify(ctx, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?;
                Sum(ts)
            }
            (Record(fds1), Record(fds2)) => {
                let mut fields = BTreeMap::new();
                for (name, t1) in fds1 {
                    let t2 = fds2.get(&name).ok_or_else(|| {
                        format!("field `{}` not found in record type", name).into_diagnostic()
                    })?;
                    let t = super::unify(ctx, t1, t2.clone())?;
                    fields.insert(name, t);
                }
                Record(fields)
            }
            (Effectful(box t1, f1), Effectful(box t2, f2)) => {
                let t = unify(ctx, t1, t2)?;
                let f = ef::unify(ctx, f1, f2)?;
                Effectful(t.into(), f)
            }
            (Effectful(box ft, f), t) | (t, Effectful(box ft, f)) => {
                let t = unify(ctx, ft, t)?;
                let f = ef::unify(ctx, f, Ef::Pure)?;
                t
            }
            (Poly(_), _) | (_, Poly(_)) => panic!("unexpected poly_var in type"),
            (t1, t2) => {
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
            Data(id, ts) => Data(id, ts.into_iter().map(|t| super::update(ctx, t)).collect()),
            Func(box t1, box t2) => {
                Func(super::update(ctx, t1).into(), super::update(ctx, t2).into())
            }
            Sum(ts) => Sum(ts.into_iter().map(|t| super::update(ctx, t)).collect()),
            Record(fields) => Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (k, super::update(ctx, v)))
                    .collect(),
            ),
            Effectful(box t, f) => Effectful(update(ctx, t).into(), f),
            t => t,
        }
    }

    pub fn instantiate(
        ctx: &mut Context<'_>,
        t: Ty,
        ps: &mut HashMap<PolyVarId, MonoVarId>,
    ) -> TyE {
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
            Data(id, ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::instantiate(ctx, t, ps))
                    .collect();
                TyE::pure(Data(id, ts))
            }
            Func(box t1, box t2) => {
                let t1 = super::instantiate(ctx, t1, ps);
                let t2 = super::instantiate(ctx, t2, ps);
                TyE::pure(Func(t1.into(), t2.into()))
            }
            Sum(ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::instantiate(ctx, t, ps))
                    .collect();
                TyE::pure(Sum(ts))
            }
            Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(k, v)| (k, super::instantiate(ctx, v, ps)))
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
            Data(id, ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::generalize(ctx, t, ps))
                    .collect();
                TyE::pure(Data(id, ts))
            }
            Func(box t1, box t2) => {
                let t1 = super::generalize(ctx, t1, ps);
                let t2 = super::generalize(ctx, t2, ps);
                TyE::pure(Func(t1.into(), t2.into()))
            }
            Sum(ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::generalize(ctx, t, ps))
                    .collect();
                TyE::pure(Sum(ts))
            }
            Record(fields) => {
                let fields = fields
                    .into_iter()
                    .map(|(k, v)| (k, super::generalize(ctx, v, ps)))
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

    pub fn ty_occurs(x: &Ty, t: &Ty) -> bool {
        if x == t {
            return true;
        }

        use Ty::*;
        match t {
            Data(_, ts) => ts.iter().any(|t| super::ty_occurs(x, t)),
            Func(box t1, box t2) => super::ty_occurs(x, &t1) || super::ty_occurs(x, &t2),
            Sum(ts) => ts.iter().any(|t| super::ty_occurs(x, t)),
            Record(fields) => fields.values().any(|v| super::ty_occurs(x, &v)),
            Effectful(box t, f) => ty_occurs(x, t) || ef::ty_occurs(x, f),
            _ => false,
        }
    }

    pub fn ef_occurs(x: &Ef, t: &Ty) -> bool {
        use Ty::*;
        match t {
            Data(_, ts) => ts.iter().any(|t| super::ef_occurs(x, t)),
            Func(box t1, box t2) => super::ef_occurs(x, &t1) || super::ef_occurs(x, &t2),
            Sum(ts) => ts.iter().any(|t| super::ef_occurs(x, t)),
            Record(fields) => fields.values().any(|v| super::ef_occurs(x, &v)),
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
            Data(id, ts) => Data(
                id,
                ts.into_iter().map(|t| super::subst_ty(r, x, t)).collect(),
            ),
            Func(box t1, box t2) => Func(
                super::subst_ty(r, x, t1).into(),
                super::subst_ty(r, x, t2).into(),
            ),
            Sum(ts) => Sum(ts.into_iter().map(|t| super::subst_ty(r, x, t)).collect()),
            Record(fields) => Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (k, super::subst_ty(r, x, v)))
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
                ts.into_iter().map(|t| super::subst_ef(r, x, t)).collect(),
            ),
            Func(box t1, box t2) => Func(
                super::subst_ef(r, x, t1).into(),
                super::subst_ef(r, x, t2).into(),
            ),
            Sum(ts) => Sum(ts.into_iter().map(|t| super::subst_ef(r, x, t)).collect()),
            Record(fields) => Record(
                fields
                    .into_iter()
                    .map(|(k, v)| (k, super::subst_ef(r, x, v)))
                    .collect(),
            ),
            Effectful(box t, f) => Effectful(t.into(), ef::subst_ef(r, x, f)),
            _ => t,
        }
    }
}

mod ef {
    use super::*;
    use crate::Context;

    pub fn unify(ctx: &mut Context<'_>, f1: Ef, f2: Ef) -> diag::Result<Ef> {
        use Ef::*;
        Ok(match (f1, f2) {
            (Pure, Pure) => Pure,
            (Infer, f) | (f, Infer) => f,
            (f, Mono(x)) | (Mono(x), f) => {
                let f = ctx.ef_set.find(f);
                if ef_occurs(&Mono(x), &f) {
                    return Err(format!(
                        "recursive effect `{}` contains itself",
                        f.pretty_string(ctx)
                    )
                    .into_diagnostic());
                }
                ctx.ef_set.union(f, Mono(x))
            }
            (Effect(id1, ts1), Effect(id2, ts2)) if id1 == id2 => {
                if ts1.len() != ts2.len() {
                    return Err(format!(
                        "cannot unify effect types of different lengths: {} and {}",
                        ts1.len(),
                        ts2.len()
                    )
                    .into_diagnostic());
                }
                let ts = ts1
                    .into_iter()
                    .zip(ts2.into_iter())
                    .map(|(t1, t2)| super::unify(ctx, t1, t2))
                    .collect::<Result<Vec<_>, _>>()?;
                Effect(id1, ts)
            }
            (Union(fs1), Union(fs2)) => {
                if fs1.len() != fs2.len() {
                    return Err(format!(
                        "cannot unify effect unions of different lengths: {} and {}",
                        fs1.len(),
                        fs2.len()
                    )
                    .into_diagnostic());
                }
                let fs = fs1
                    .into_iter()
                    .zip(fs2.into_iter())
                    .map(|(f1, f2)| unify(ctx, f1, f2))
                    .collect::<Result<Vec<_>, _>>()?;
                Union(fs)
            }
            (f1, f2) => {
                return Err(format!(
                    "expected effect `{}`, found `{}`",
                    f1.pretty_string(ctx),
                    f2.pretty_string(ctx)
                )
                .into_diagnostic())
            }
        })
    }

    pub fn update(ctx: &mut Context<'_>, f: Ef) -> Ef {
        use Ef::*;
        match f {
            Mono(x) => ctx.ef_set.find(Mono(x)),
            Effect(id, ts) => {
                let ts = ts.into_iter().map(|t| super::update(ctx, t)).collect();
                Effect(id, ts)
            }
            Union(fs) => Union(fs.into_iter().map(|f| update(ctx, f)).collect()),
            f => f,
        }
    }

    pub fn instantiate(ctx: &mut Context<'_>, f: Ef, ps: &mut HashMap<PolyVarId, MonoVarId>) -> Ef {
        use Ef::*;
        match f {
            Poly(id) => match ps.get(&id) {
                Some(&id) => Mono(id),
                Some(_) => panic!("invalid poly var"),
                None => {
                    let t = ctx.new_mono_var();
                    ps.insert(id, t);
                    let f = Ef::Mono(t);
                    ctx.ef_set.insert(f.clone());
                    f
                }
            },
            Effect(id, ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::instantiate(ctx, t, ps))
                    .collect();
                Effect(id, ts)
            }
            Union(fs) => Union(fs.into_iter().map(|f| instantiate(ctx, f, ps)).collect()),
            f => f,
        }
    }

    pub fn generalize(ctx: &mut Context<'_>, f: Ef, ps: &mut HashMap<MonoVarId, PolyVarId>) -> Ef {
        use Ef::*;
        match f {
            Mono(id) => match ps.get(&id) {
                Some(&id) => Poly(id),
                Some(_) => panic!("invalid mono var"),
                None => {
                    let t = ctx.ids.next_poly_var_id();
                    ps.insert(id, t);
                    let f = Ef::Poly(t);
                    ctx.ef_set.insert(f.clone());
                    f
                }
            },
            Effect(id, ts) => {
                let ts = ts
                    .into_iter()
                    .map(|t| super::generalize(ctx, t, ps))
                    .collect();
                Effect(id, ts)
            }
            Union(fs) => Union(fs.into_iter().map(|f| generalize(ctx, f, ps)).collect()),
            f => f,
        }
    }

    pub fn contains(id: EffectId, ts: &Vec<TyE>, f: &Ef) -> bool {
        use Ef::*;
        match f {
            Effect(id2, ts2) if &id == id2 => {
                if ts.len() != ts2.len() {
                    return false;
                }
                ts.iter().zip(ts2.iter()).all(|(t1, t2)| t1 == t2)
            }
            Union(fs) => fs.iter().any(|f| contains(id, ts, f)),
            _ => false,
        }
    }

    pub fn ty_occurs(x: &Ty, f: &Ef) -> bool {
        use Ef::*;
        match f {
            Effect(_, ts) => ts.iter().any(|t| super::ty_occurs(x, t)),
            Union(ts) => ts.iter().any(|t| ty_occurs(x, t)),
            _ => false,
        }
    }

    pub fn ef_occurs(x: &Ef, f: &Ef) -> bool {
        if x == f {
            return true;
        }

        use Ef::*;
        match f {
            Effect(_, ts) => ts.iter().any(|t| super::ef_occurs(x, t)),
            Union(ts) => ts.iter().any(|t| ef_occurs(x, t)),
            _ => false,
        }
    }

    pub fn subst_ty(r: &Ty, x: &Ty, f: Ef) -> Ef {
        use Ef::*;
        match f {
            Effect(id, ts) => Effect(
                id,
                ts.into_iter().map(|t| super::subst_ty(r, x, t)).collect(),
            ),
            Union(ts) => Union(ts.into_iter().map(|t| subst_ty(r, x, t)).collect()),
            f => f,
        }
    }

    pub fn subst_ef(r: &Ef, x: &Ef, f: Ef) -> Ef {
        if &f == x {
            return r.clone();
        }

        use Ef::*;
        match f {
            Effect(id, ts) => Effect(
                id,
                ts.into_iter().map(|t| super::subst_ef(r, x, t)).collect(),
            ),
            Union(ts) => Union(ts.into_iter().map(|t| subst_ef(r, x, t)).collect()),
            f => f,
        }
    }
}

mod cs {
    use super::*;

    pub fn update(ctx: &mut Context<'_>, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter().map(|c| constraint::update(ctx, c)).collect()
    }

    pub fn instantiate(
        ctx: &mut Context<'_>,
        cs: Vec<Constraint>,
        ps: &mut HashMap<PolyVarId, MonoVarId>,
    ) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| constraint::instantiate(ctx, c, ps))
            .collect()
    }

    pub fn generalize(
        ctx: &mut Context<'_>,
        cs: Vec<Constraint>,
        ps: &mut HashMap<MonoVarId, PolyVarId>,
    ) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| constraint::generalize(ctx, c, ps))
            .collect()
    }

    pub fn subst_ty(r: &Ty, x: &Ty, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| constraint::subst_ty(r, x, c))
            .collect()
    }

    pub fn subst_ef(r: &Ef, x: &Ef, cs: Vec<Constraint>) -> Vec<Constraint> {
        cs.into_iter()
            .map(|c| constraint::subst_ef(r, x, c))
            .collect()
    }
}

mod constraint {
    use super::*;

    pub fn update(ctx: &mut Context<'_>, c: Constraint) -> Constraint {
        use Constraint::*;
        match c {
            Empty => Empty,
            Eq(box t1, box t2) => Eq(super::update(ctx, t1).into(), super::update(ctx, t2).into()),
            Class(id, ts) => Class(
                id,
                ts.into_iter()
                    .map(|t| super::update(ctx, t))
                    .collect::<Vec<_>>(),
            ),
        }
    }

    pub fn instantiate(
        ctx: &mut Context<'_>,
        c: Constraint,
        ps: &mut HashMap<PolyVarId, MonoVarId>,
    ) -> Constraint {
        use Constraint::*;
        match c {
            Empty => Empty,
            Eq(box t1, box t2) => Eq(
                super::instantiate(ctx, t1, ps).into(),
                super::instantiate(ctx, t2, ps).into(),
            ),
            Class(id, ts) => Class(
                id,
                ts.into_iter()
                    .map(|t| super::instantiate(ctx, t, ps))
                    .collect(),
            ),
        }
    }

    pub fn generalize(
        ctx: &mut Context<'_>,
        c: Constraint,
        ps: &mut HashMap<MonoVarId, PolyVarId>,
    ) -> Constraint {
        use Constraint::*;
        match c {
            Empty => Empty,
            Eq(box t1, box t2) => Eq(
                super::generalize(ctx, t1, ps).into(),
                super::generalize(ctx, t2, ps).into(),
            ),
            Class(id, ts) => Class(
                id,
                ts.into_iter()
                    .map(|t| super::generalize(ctx, t, ps))
                    .collect(),
            ),
        }
    }

    pub fn subst_ty(r: &Ty, x: &Ty, c: Constraint) -> Constraint {
        use Constraint::*;
        match c {
            Empty => Empty,
            Eq(box t1, box t2) => Eq(
                super::subst_ty(r, x, t1).into(),
                super::subst_ty(r, x, t2).into(),
            ),
            Class(id, ts) => Class(
                id,
                ts.into_iter().map(|t| super::subst_ty(r, x, t)).collect(),
            ),
        }
    }

    pub fn subst_ef(r: &Ef, x: &Ef, c: Constraint) -> Constraint {
        use Constraint::*;
        match c {
            Empty => Empty,
            Eq(box t1, box t2) => Eq(
                super::subst_ef(r, x, t1).into(),
                super::subst_ef(r, x, t2).into(),
            ),
            Class(id, ts) => Class(
                id,
                ts.into_iter().map(|t| super::subst_ef(r, x, t)).collect(),
            ),
        }
    }
}
