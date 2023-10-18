use crate::{debug_println, ty, type_env, Context};
use term_core as core;
use term_diag as diag;
use term_print as print;

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
    let result = match e {
        Type(box t) => (Expr::unit(), t),

        Lit(l) => {
            let t = TyE::pure(l.as_ty());
            (Lit(l), t)
        }
        Sym(s) => todo!(),
        Var(id) => {
            debug_println!(ctx, "{tab}[var] solving: {}", id.pretty_string(ctx.core));
            let (e, t) = if let Some(t) = ctx.solve.typings.get(&Expr::Var(id)).cloned() {
                let t = crate::update(ctx, t);
                (Expr::Var(id), crate::cannonicalize(ctx, t))
            } else if let Some(d) = ctx.core.defs.get(&id).cloned() {
                let d = d.borrow();
                let t = crate::instantiate(ctx, d.ty.clone(), &mut HashMap::new());
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
            let e_t = crate::unify(ctx, t, e_t, level + 1)?;
            debug_println!(ctx, "{tab}[var] done: {}", e_t.pretty_string(ctx.core));
            (Var(id), e_t)
        }
        Record(fields) => {
            let mut es = BTreeMap::new();
            let mut rec = BTreeMap::new();
            for (n, e) in fields {
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

        Apply(box e1, box e2) => {
            debug_println!(
                ctx,
                "{tab}[app] solving: ({} {})",
                e1.pretty_string(ctx.core),
                e2.pretty_string(ctx.core)
            );

            let (e1, t1) = algorithmj(ctx, e1, level + 1)?;
            let (t1, f1) = t1.split_ef();
            let (e2, t2) = algorithmj(ctx, e2, level + 1)?;
            let (t2, f2) = t2.split_ef();

            let v = ctx.new_ty_var();
            crate::unify(
                ctx,
                t1.clone(),
                TyE::pure_func(t2.clone(), TyE::pure(v.clone())),
                level + 1,
            )?;

            let result = crate::update(ctx, TyE::pure(v).with_ef(f1 | f2));
            debug_println!(ctx, "{tab}[app] done: {}", result.pretty_string(ctx.core));
            (Apply(e1.into(), e2.into()), result)
        }
        Lambda(box p, box e) => {
            debug_println!(
                ctx,
                "{tab}[lam] solving: λ{}.{}",
                p.pretty_string(ctx.core),
                p.pretty_string(ctx.core)
            );

            let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
            ctx.solve.typings.push(vars);
            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (ret_t, ret_f, ret_cs) = e_t.into_tuple();
            ctx.solve.typings.pop();

            let pt = ty::update(ctx, t);
            let result = TyE::new(
                Ty::Func(TyE::pure(pt).into(), TyE::pure(ret_t).into()),
                ret_f,
                ret_cs,
            );

            let result = crate::update(ctx, result);
            debug_println!(ctx, "{tab}[lam] done: {}", result.pretty_string(ctx.core),);
            (Lambda(p.into(), e.into()), result)
        }
        Let(Rec(x, box e), e1) => {
            debug_println!(
                ctx,
                "{tab}[letr] solving: let rec {} = {}",
                x,
                e.pretty_string(ctx.core)
            );
            let v1 = TyE::pure(ctx.new_ty_var());
            let v2 = TyE::pure(ctx.new_ty_var());
            let f = ctx.new_ef_var();
            let ft = TyE::pure_func(v1.clone(), v2.clone()).with_ef(f);

            ctx.solve.typings.push(vec![(Expr::Var(x), ft.clone())]);
            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            ctx.solve.typings.pop();

            let e_t = crate::unify(ctx, ft, e_t, level + 1)?;
            let (e1, res_t) = if let Some(box e1) = e1 {
                debug_println!(
                    ctx,
                    "{tab}{TABWIDTH}[letr] solving in {}",
                    e1.pretty_string(ctx.core)
                );
                ctx.solve.typings.push(vec![(Expr::Var(x), e_t)]);
                let (e1, e1_t) = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.solve.typings.pop();
                (Some(e1.into()), e1_t)
            } else {
                (None, e_t)
            };

            debug_println!(ctx, "{tab}[letr] done: {}", res_t.pretty_string(ctx.core));
            (Let(Rec(x, e.into()), e1), res_t)
        }
        Let(NonRec(box p, box e), e1) => {
            debug_println!(
                ctx,
                "{tab}[let] solving: let {} = {}",
                p.pretty_string(ctx.core),
                e.pretty_string(ctx.core)
            );

            let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
            let t = TyE::pure(t);

            ctx.solve.typings.push(vars);
            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            ctx.solve.typings.pop();

            let (e1, res_t) = if let Some(box e1) = e1 {
                ctx.solve.typings.push(vec![(p.clone(), e_t)]);
                let (e1, e1_t) = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.solve.typings.pop();
                (Some(e1.into()), e1_t)
            } else {
                (None, e_t)
            };

            debug_println!(ctx, "{tab}[let] done: {}", res_t.pretty_string(ctx.core));
            (Let(NonRec(p.into(), e.into()), e1), res_t)
        }
        Case(box e, alts) => {
            debug_println!(
                ctx,
                "{tab}[case] solving: case {} in {}",
                e.pretty_string(ctx.core),
                alts.iter()
                    .map(|a| a.pretty_string(ctx.core))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );

            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let mut new_alts = vec![];
            let mut res_t = Ty::Infer;
            let mut res_f = Ef::Infer;
            for (p, e) in alts.into_iter().map(|a| (a.pat, a.expr)) {
                let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
                crate::unify(ctx, e_t.clone(), TyE::pure(t), level + 1)?;

                ctx.solve.typings.push(vars);
                let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
                let (e_t, e_f, _) = e_t.into_tuple();
                ctx.solve.typings.pop();

                res_t = ty::unify(ctx, res_t, e_t, level + 1)?;
                res_f = res_f | e_f;
                new_alts.push(Alt { pat: p, expr: e })
            }

            let result = TyE::new(res_t, res_f, vec![]);
            let result = crate::update(ctx, result);
            debug_println!(ctx, "{tab}[case] done: {}", result.pretty_string(ctx.core));
            (Case(e.into(), new_alts), result)
        }
        Handle(box e, Some(alts)) => {
            debug_println!(
                ctx,
                "{tab}[han] solving: handle {} with \n\t{}",
                e.pretty_string(ctx.core),
                alts.iter()
                    .map(|(ea)| format!(
                        "{} ~> {}",
                        ea.pretty_string(ctx.core),
                        e.pretty_string(ctx.core)
                    ))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );

            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (e_t, e_f, e_cs) = e_t.into_tuple();

            let mut new_alts = vec![];
            let mut fs = e_f.into_set();
            for (f, e) in alts.into_iter().map(|a| (a.ef, a.expr)) {
                debug_println!(
                    ctx,
                    "{tab}[han] solving: {} ~> {}",
                    f.pretty_string(ctx.core),
                    e.pretty_string(ctx.core),
                );

                let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                if f.is_pure() {
                    continue;
                }

                let ht = solve_handler_ty(ctx, &f)?;
                let e_t = crate::unify(ctx, e_t, ht, level + 1)?;

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
                new_alts.push(EfAlt { ef: f, expr: e });
            }

            let result = TyE::new(e_t, Ef::from(fs), e_cs);
            let result = crate::update(ctx, result);
            debug_println!(ctx, "{tab}[han] done: {} ", result.pretty_string(ctx.core));
            (Handle(e.into(), Some(new_alts)), result)
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
                        new_alts.push(EfAlt {
                            ef: Ef::Effect(ef_id, ts),
                            expr: Expr::Var(var_id),
                        });
                    } else {
                        fs.insert(Ef::Effect(ef_id, ts));
                    }
                } else {
                    fs.insert(f);
                }
            }

            let result = TyE::new(e_t, Ef::from(fs), e_cs);
            let result = crate::update(ctx, result);
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
            let mut res_f = Ef::Pure;
            let mut result = TyE::pure(Ty::Unit);
            for e in es {
                let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                let (t, f) = e_t.split_ef();
                new_es.push(e);
                res_f = res_f | f;
                result = t;
            }

            let result = crate::update(ctx, result.with_ef(res_f));
            debug_println!(ctx, "{tab}[do ] done: {}", result.pretty_string(ctx.core));
            (Do(new_es), result)
        }

        Span(s, box e) => {
            ctx.solve.spans.push(s);
            let result = algorithmj(ctx, e, level)?;
            ctx.solve.spans.pop();
            result
        }
    };
    Ok(result)
}

pub fn solve_collect_bindings(
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
                let (t, vs) = solve_collect_bindings(ctx, e, level + 1)?;
                fields.insert(*n, TyE::pure(t));
                vars.extend(vs);
            }
            Ok((Ty::Record(fields), vars))
        }
        Apply(box e1, box e2) => {
            // the outer leftmost expression must be a data constructor reference
            let (t1, vs1) = if let Expr::Var(id) = e1 {
                // id is a data constructor. get the type
                let def = ctx.core.defs.get(id).cloned().unwrap();
                let def = def.borrow();
                let t = crate::instantiate(ctx, def.ty.clone(), &mut HashMap::default());
                (t.ty, vec![])
            } else {
                solve_collect_bindings(ctx, e1, level + 1)?
            };
            let (t2, vs2) = solve_collect_bindings(ctx, e2, level + 1)?;

            let v = ctx.new_ty_var();
            crate::unify(
                ctx,
                TyE::pure(t1),
                TyE::pure_func(TyE::pure(t2), TyE::pure(v.clone())),
                level,
            )?;
            // println!("t: {} | v: {}", t.pretty_string(ctx), v.pretty_string(ctx));
            let t = ty::update(ctx, v);
            Ok((t, vs1.into_iter().chain(vs2).collect()))
        }
        Span(_, box e) => solve_collect_bindings(ctx, e, level),
        _ => format!("invalid pattern: `{}`", p.pretty_string(ctx.core)).into_err(),
    }
}

pub fn solve_handler_ty(ctx: &mut Context<'_>, f: &Ef) -> diag::Result<TyE> {
    use Ef::*;
    Ok(match f {
        Effect(ef_id, ts) => {
            let effect = ctx.core.effects.get(ef_id).cloned().unwrap();
            let effect = effect.borrow();
            let mut t = crate::instantiate(ctx, effect.handler_ty.clone(), &mut HashMap::default());
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
