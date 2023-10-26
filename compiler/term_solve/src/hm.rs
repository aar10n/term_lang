use crate::{constraint, debug_println, ef, ty, type_env, Context};
use term_core as core;
use term_diag as diag;
use term_print as print;

use constraint::cs;
use core::*;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString, TABWIDTH};

use either::*;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use type_env::TSet;

pub fn algorithmj(ctx: &mut Context<'_>, e: Expr, level: usize) -> diag::Result<(Expr, TyE)> {
    use self::Bind::*;
    use self::Expr::*;
    use self::Lit::*;
    if level > 30 {
        panic!();
    }

    let tab = TABWIDTH.repeat(level);
    let ttab = TABWIDTH.repeat(level + 1);

    let result = match e {
        Type(box t) => (Expr::unit(), t),
        Lit(l) => {
            let t = TyE::pure(l.as_ty());
            (Lit(l), t)
        }
        Sym(s) => (Sym(s), TyE::simple(ctx.new_ty_var(), ctx.new_ef_var())),
        Var(id) => {
            debug_println!(ctx, "{tab}[var] solving: {}", id.pretty_string(ctx.core));
            let (e, t) = if let Some(t) = ctx.solve.typings.get(&Var(id)).cloned() {
                let t = update(ctx, t);
                (Expr::Var(id), cannonicalize(ctx, t))
            } else if let Some(d) = ctx.core.defs.get(&id).cloned() {
                let d = d.borrow();
                let t = instantiate(ctx, d.ty.clone(), &mut HashMap::new());
                ctx.solve.typings.insert(Expr::Var(id), t.clone());
                (d.body.clone(), t)
            } else {
                println!("-------");
                ctx.solve.typings.print_stdout(&ctx.core);
                println!("-------");
                panic!();
                return if let Some(span) = ctx.core.id_as_span(id) {
                    Diagnostic::error(
                        format!(
                            "no definition found for name: {}",
                            id.pretty_string(ctx.core)
                        ),
                        span,
                    )
                    .into_err()
                } else {
                    Err(format!(
                        "no definition found for name: {}",
                        id.pretty_string(ctx.core)
                    )
                    .into_diagnostic())
                };
            };

            if e == Var(id) || t.is_concrete() {
                debug_println!(
                    ctx,
                    "{tab}[var] done: {} : {}",
                    id.pretty_string(ctx.core),
                    t.pretty_string(ctx.core)
                );
                return Ok((Var(id), t));
            }

            let (e, e_t) = algorithmj(ctx, e, level + 1)?;
            let e_t = unify(ctx, t, e_t, level + 1)?;
            debug_println!(ctx, "{tab}[var] done: {}", e_t.pretty_string(ctx.core));
            (Var(id), e_t)
        }

        Apply(..) => {
            let (e, args) = e.uncurry_apply();
            debug_println!(
                ctx,
                "{tab}[app] solving: <n={}> {}",
                args.len(),
                e.pretty_string(ctx.core),
            );

            let (e, t) = algorithmj(ctx, e, level + 1)?;
            let (mut r_t, mut r_f) = t.clone().split_ef();

            let mut es = vec![];
            let mut ts = vec![];
            for e in args {
                debug_println!(
                    ctx,
                    "{ttab}[app] arg: {} | {}",
                    r_t.pretty_string(ctx.core),
                    e.pretty_string(ctx.core)
                );

                let (e, t) = algorithmj(ctx, e, level + 2)?;
                es.push(e);
                ts.push(t.clone());

                let (t, f) = t.split_ef();
                let v = ctx.new_ty_var();
                r_t = unify(ctx, r_t, TyE::func(t, TyE::pure(v.clone())), level + 2)?;

                r_t = update(ctx, TyE::pure(v));
                r_f = r_f | f;
            }

            let f_t = update(ctx, TyE::nary_func(ts, r_t.clone()).with_ef(r_f.clone()));
            debug_println!(
                ctx,
                "{tab}[app] type of {} is {}",
                e.pretty_string(ctx.core),
                f_t.pretty_string(ctx.core)
            );

            let res_t = update(ctx, r_t.with_ef(r_f));
            let res_e = Expr::apply_n(e, es);
            debug_println!(ctx, "{tab}[app] done: {}", res_t.pretty_string(ctx.core));
            (res_e, res_t)
        }
        Lambda(box p, box e) => {
            debug_println!(
                ctx,
                "{tab}[lam] solving: λ{}.{}",
                p.pretty_string(ctx.core),
                e.pretty_string(ctx.core)
            );

            let (t, vars) = solve_pat(ctx, &p, level + 1)?;
            ctx.solve.typings.push(vars);
            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (e_t, e_f, e_cs) = e_t.into_tuple();
            ctx.solve.typings.pop();

            let pt = ty::update(ctx, t);
            let result = TyE::new(
                Ty::Func(TyE::pure(pt).into(), TyE::pure(e_t).into()),
                e_f,
                e_cs,
            );

            let result = update(ctx, result);
            debug_println!(ctx, "{tab}[lam] done: {}", result.pretty_string(ctx.core),);
            (Lambda(p.into(), e.into()), result)
        }
        Case(box e, alts) => {
            debug_println!(
                ctx,
                "{tab}[case] solving: case {} in",
                e.pretty_string(ctx.core),
            );

            let (case_e, case_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (case_t, case_f) = case_t.split_ef();

            let mut new_alts = vec![];
            let mut res_t = TyE::pure(Ty::Infer);
            let mut res_f = Ef::Infer;
            for (p, e) in alts.into_iter() {
                debug_println!(
                    ctx,
                    "{ttab}[case] solving: {} -> {}",
                    p.pretty_string(ctx.core),
                    e.pretty_string(ctx.core)
                );

                let (p_t, vars) = solve_pat(ctx, &p, level + 2)?;
                // chceck pattern against the case expression
                unify(ctx, TyE::pure(p_t), case_t.clone(), level + 2)?;

                ctx.solve.typings.push(vars);
                let (e, e_t) = algorithmj(ctx, e.clone(), level + 2)?;
                let (e_t, e_f) = e_t.split_ef();
                ctx.solve.typings.pop();

                res_t = unify(ctx, res_t, e_t, level + 2)?;
                res_f = res_f | e_f;
                new_alts.push((p, e))
            }

            let result = update(ctx, res_t.with_ef(res_f));
            debug_println!(ctx, "{tab}[case] done: {}", result.pretty_string(ctx.core));
            (Case(e.into(), new_alts), result)
        }
        Handle(box e, Some(alts)) => {
            debug_println!(
                ctx,
                "{tab}[han] solving: handle {}",
                e.pretty_string(ctx.core),
            );

            let (han_e, han_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (han_t, han_f) = han_t.split_ef();

            let mut new_alts = vec![];
            let mut fs = han_f.into_set();
            for (f, e) in alts.into_iter() {
                debug_println!(
                    ctx,
                    "{ttab}[han] solving: {} ~> {}",
                    f.pretty_string(ctx.core),
                    e.pretty_string(ctx.core),
                );

                let (e, e_t) = algorithmj(ctx, e, level + 2)?;
                if f.is_pure() {
                    continue;
                }

                let h_t = solve_handler_ty(ctx, &f)?;
                let e_t = unify(ctx, e_t, h_t, level + 1)?;

                debug_println!(
                    ctx,
                    "{tab}[han] done: {} ~> {}",
                    f.pretty_string(ctx.core),
                    e_t.pretty_string(ctx.core),
                );

                // remove handled effects from the set
                for f in f.clone().into_set() {
                    fs.remove(&f);
                }

                let handler = Some(e.clone());
                new_alts.push((f, e));
            }

            let result = update(ctx, han_t.with_ef(Ef::from(fs)));
            debug_println!(ctx, "{tab}[han] done: {} ", result.pretty_string(ctx.core));
            (Handle(han_e.into(), Some(new_alts)), result)
        }
        Handle(box e, None) => {
            debug_println!(
                ctx,
                "{tab}[han] solving: handle default {}",
                e.pretty_string(ctx.core)
            );

            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (e_t, e_f, e_cs) = e_t.into_tuple();

            let mut new_alts = vec![];
            let mut fs = BTreeSet::new();
            for f in e_f.into_set().into_iter() {
                if let Ef::Effect(ef_id, ts) = f {
                    let ef = ctx.core.effects[&ef_id].clone();
                    let ef = ef.borrow();
                    if let Some(var_id) = ef.default {
                        new_alts.push((Ef::Effect(ef_id, ts), Expr::Var(var_id)));
                    } else {
                        fs.insert(Ef::Effect(ef_id, ts));
                    }
                } else {
                    fs.insert(f);
                }
            }

            let result = TyE::new(e_t, Ef::from(fs), e_cs);
            let result = update(ctx, result);
            debug_println!(ctx, "{tab}[han] done: {} ", result.pretty_string(ctx.core));
            (Handle(e.into(), Some(new_alts)), result)
        }
        Do(es) => {
            debug_println!(
                ctx,
                "{tab}[do ] solving: {}",
                es.iter()
                    .map(|e| e.pretty_string(ctx.core))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );

            let mut new_es = vec![];
            let mut res_t = TyE::pure(Ty::Unit);
            let mut res_f = Ef::Pure;
            for e in es {
                let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                let (t, f) = e_t.split_ef();
                new_es.push(e);
                res_f = res_f | f;
                res_t = t;
            }

            let result = update(ctx, res_t.with_ef(res_f));
            debug_println!(ctx, "{tab}[do ] done: {}", result.pretty_string(ctx.core));
            (Do(new_es), result)
        }
        Let(bs, e) => {
            debug_println!(ctx, "{tab}[let] solving: let <bindings> in <expr>");

            let mut vars = vec![];
            let mut new_bs = vec![];
            for b in bs {
                match b {
                    NonRec(box p, box e) => {
                        debug_println!(
                            ctx,
                            "{ttab}[let] solving: let {} = {}",
                            p.pretty_string(ctx.core),
                            e.pretty_string(ctx.core)
                        );

                        let (t, vs) = solve_pat(ctx, &p, level + 1)?;
                        ctx.solve.typings.push(vs);
                        let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                        ctx.solve.typings.pop();
                        let t = unify(ctx, t.into(), e_t, level + 1)?;

                        debug_println!(ctx, "{ttab}[let] done: {}", t.pretty_string(ctx.core));

                        vars.push((p.clone(), t));
                        new_bs.push(NonRec(p.into(), e.into()));
                    }
                    Rec(x, box e) => {
                        debug_println!(
                            ctx,
                            "{ttab}[let] solving: let rec {} = {}",
                            x.pretty_string(ctx.core),
                            e.pretty_string(ctx.core)
                        );

                        let v1 = TyE::pure(ctx.new_ty_var());
                        let v2 = TyE::pure(ctx.new_ty_var());
                        let f = ctx.new_ef_var();
                        let ft = TyE::func(v1.clone(), v2.clone()).with_ef(f);

                        ctx.solve.typings.push(vec![(Expr::Var(x), ft.clone())]);
                        let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
                        ctx.solve.typings.pop();
                        let t = unify(ctx, ft, e_t, level + 1)?;

                        debug_println!(ctx, "{ttab}[letr] done: {}", t.pretty_string(ctx.core));

                        vars.push((Expr::Var(x), t));
                        new_bs.push(Rec(x, e.into()));
                    }
                }
            }

            let (e, t) = if let Some(box e) = e {
                debug_println!(ctx, "{ttab}[let] solving in: {}", e.pretty_string(ctx.core));

                ctx.solve.typings.push(vars);
                let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                ctx.solve.typings.pop();
                let result = update(ctx, e_t);
                debug_println!(ctx, "{ttab}[let] done: {}", result.pretty_string(ctx.core));

                (Some(e.into()), result)
            } else if vars.len() == 1 {
                let (_, t) = vars.pop().unwrap();
                (None, t)
            } else {
                return format!("expected expression, found `{}`", e.pretty_string(ctx.core))
                    .into_err();
            };

            (Let(new_bs, e), t)
        }
        Record(fs) => {
            let mut es = BTreeMap::new();
            let mut rec = BTreeMap::new();
            for (n, e) in fs {
                debug_println!(
                    ctx,
                    "{tab}[rec] solving: {} = {}",
                    n,
                    e.pretty_string(ctx.core)
                );
                let (e, t) = algorithmj(ctx, e.clone(), level + 1)?;
                debug_println!(ctx, "{tab}[rec] done: {}", t.pretty_string(ctx.core));
                es.insert(n.clone(), e);
                rec.insert(n, t);
            }
            (Record(es), TyE::pure(Ty::Record(rec)))
        }
        RecSel(box e, key) => {
            debug_println!(
                ctx,
                "{tab}[sel] solving: {}.{}",
                e.pretty_string(ctx.core),
                key
            );

            let (e, t) = algorithmj(ctx, e, level + 1)?;
            let (t, f) = t.split_ef();
            if let Ty::Record(fs) = t.ty {
                if let Some(t) = fs.get(&key) {
                    let result = update(ctx, t.clone());
                    debug_println!(ctx, "{tab}[sel] done: {}", result.pretty_string(ctx.core));
                    return Ok((RecSel(e.into(), key), result));
                } else {
                    return format!(
                        "record `{}` does not have field `{}`",
                        Ty::Record(fs).pretty_string(ctx.core),
                        key
                    )
                    .into_err();
                }
            }
            format!("expected record, found `{}`", t.pretty_string(ctx.core)).into_err()?
        }

        Span(s, box e) => match algorithmj(ctx, e, level) {
            Ok((e, t)) => (Span(s, e.into()), t),
            Err(mut err) => {
                if err.span.is_invalid() {
                    err.span = s;
                }
                return Err(err);
            }
        },
    };
    Ok(result)
}

pub fn solve_pat(
    ctx: &mut Context<'_>,
    p: &Expr,
    level: usize,
) -> diag::Result<(Ty, Vec<(Expr, TyE)>)> {
    use Expr::*;
    match p {
        Lit(l) => Ok((l.as_ty(), vec![])),
        Var(id) => {
            let v = ctx.new_ty_var();
            Ok((v.clone(), vec![(Expr::Var(*id), TyE::pure(v))]))
        }
        Record(rec) => {
            let mut fields = BTreeMap::new();
            let mut vars = vec![];
            for (n, e) in rec {
                let (t, vs) = solve_pat(ctx, e, level + 1)?;
                fields.insert(*n, TyE::pure(t));
                vars.extend(vs);
            }
            Ok((Ty::Record(fields), vars))
        }
        Apply(box e1, box e2) => {
            // the outer leftmost expression must be a data constructor var id
            let (t1, vs1) = if e1.is_var() && let Expr::Var(id) = e1.clone().unwrap_inner() {
                // id is a data constructor, get the type
                let def = ctx.core.defs.get(&id).cloned().unwrap();
                let def = def.borrow();
                let t = instantiate(ctx, def.ty.clone(), &mut HashMap::default());
                (t.ty, vec![])
            } else {
                solve_pat(ctx, e1, level + 1)?
            };
            let (t2, vs2) = solve_pat(ctx, e2, level + 1)?;

            let v = ctx.new_ty_var();
            unify(
                ctx,
                TyE::pure(t1),
                TyE::func(TyE::pure(t2), TyE::pure(v.clone())),
                level,
            )?;
            let t = ty::update(ctx, v);
            Ok((t, vs1.into_iter().chain(vs2).collect()))
        }
        Span(s, box e) => match solve_pat(ctx, e, level) {
            Ok(res) => Ok(res),
            Err(mut err) => {
                if err.span.is_invalid() {
                    err.span = *s;
                }
                return Err(err);
            }
        },
        _ => {
            panic!();
            format!("invalid pattern: `{}`", p.pretty_string(ctx.core)).into_err()
        }
    }
}

pub fn solve_handler_ty(ctx: &mut Context<'_>, f: &Ef) -> diag::Result<TyE> {
    use Ef::*;
    Ok(match f {
        Effect(ef_id, ts) => {
            let effect = ctx.core.effects.get(ef_id).cloned().unwrap();
            let effect = effect.borrow();
            let mut t = instantiate(ctx, effect.handler_ty.clone(), &mut HashMap::default());
            t
        }
        Union(fs) => {
            // TODO: lower all effects and create a handler super-type by
            // combining all the records into one.
            format!("invalid effect pattern: `{}`", f.pretty_string(ctx.core)).into_err()?
        }
        _ => return format!("expected effect, found `{}`", f.pretty_string(ctx.core)).into_err(),
    })
}

//
//

pub fn solve_constraints(
    ctx: &mut Context<'_>,
    mut cs: Vec<Constraint>,
) -> diag::Result<Vec<Constraint>> {
    let mut open = vec![];
    while let Some(c) = cs.pop() {
        match c {
            Constraint::Empty => {}
            Constraint::Eq(box t1, box t2) => {
                let t = unify(ctx, t1.clone(), t2.clone(), 0)?;
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

pub fn unify(ctx: &mut Context<'_>, t1: TyE, t2: TyE, level: usize) -> diag::Result<TyE> {
    let t1 = cannonicalize(ctx, t1);
    let t2 = cannonicalize(ctx, t2);
    if t1 == t2 {
        return Ok(t1);
    }

    let tab = "  ".repeat(level);
    // debug_println!(
    //     ctx,
    //     "{tab}unify: {} = {}",
    //     t1.pretty_string(ctx.core),
    //     t2.pretty_string(ctx.core)
    // );

    let (t1, f1, mut cs) = t1.into_tuple();
    let (t2, f2, cs2) = t2.into_tuple();
    cs.extend(cs2);

    let t = ty::unify(ctx, t1, t2, level /* + 1 */)?;
    let f = ef::unify(ctx, f1, f2, level /* + 1 */)?;
    let cs = solve_constraints(ctx, cs)?;

    let ty = TyE::new(ty::update(ctx, t), ef::update(ctx, f), cs::update(ctx, cs));
    Ok(ty)
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
