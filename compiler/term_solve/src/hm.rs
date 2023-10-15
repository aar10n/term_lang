use crate::{debug_println, ty, type_env, Context};
use term_core::*;
use term_diag as diag;
use term_print as print;

use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString, TABWIDTH};

use either::*;
use std::collections::{BTreeMap, HashMap, HashSet};
use type_env::TSet;

pub fn algorithmj(ctx: &mut Context<'_>, e: Expr, level: usize) -> diag::Result<TyE> {
    if level > 30 {
        panic!();
    }
    use self::Bind::*;
    use self::Expr::*;
    use self::Lit::*;

    let tab = TABWIDTH.repeat(level);
    let result = match e {
        Type(box t) => t,

        Lit(l) => TyE::pure(l.as_ty()),
        Sym(s) => todo!(),
        Var(id) => {
            debug_println!("{tab}[var] solving: {}", id.pretty_string(ctx));
            let (e, t) = if let Some(t) = ctx.typings.get(&Expr::Var(id)).cloned() {
                let t = crate::update(ctx, t);
                (Expr::Var(id), crate::cannonicalize(ctx, t))
            } else if let Some(d) = ctx.defs.get(&id).cloned() {
                let d = d.borrow();
                let t = crate::instantiate(ctx, d.ty.clone(), &mut HashMap::new());
                ctx.typings.insert(Expr::Var(id), t.clone());
                (d.body.clone(), t)
            } else {
                println!("-------");
                ctx.typings.print_stdout(ctx);
                println!("-------");
                panic!();
                return if let Some(span) = ctx.id_as_span(id) {
                    Diagnostic::error(
                        format!("no definition found for name: {}", id.pretty_string(ctx)),
                        span,
                    )
                    .into_err()
                } else {
                    Err(
                        format!("no definition found for name: {}", id.pretty_string(ctx))
                            .into_diagnostic(),
                    )
                };
            };

            if e == Var(id) || t.is_concrete() {
                debug_println!(
                    "{tab}[var] done: {} : {}",
                    id.pretty_string(ctx),
                    t.pretty_string(ctx)
                );
                return Ok(t);
            }

            let u = algorithmj(ctx, e, level + 1)?;
            let result = crate::unify(ctx, t, u, level + 1)?;
            debug_println!(
                "{tab}[var] done: {} : {}",
                id.pretty_string(ctx),
                result.pretty_string(ctx)
            );
            result
        }
        Record(fields) => {
            let mut rec = BTreeMap::new();
            for (n, e) in fields {
                debug_println!("{tab}[rec] solving: {} = {}", n, e.pretty_string(ctx));
                let t = algorithmj(ctx, e, level + 1)?;
                debug_println!("{tab}[rec] done: {}", t.pretty_string(ctx));
                rec.insert(n, t);
            }
            TyE::pure(Ty::Record(rec))
        }

        Apply(box e1, box e2) => {
            debug_println!(
                "{tab}[app] solving: ({} {})",
                e1.pretty_string(ctx),
                e2.pretty_string(ctx)
            );

            let (t1, f1) = algorithmj(ctx, e1, level + 1)?.split_ef();
            let (t2, f2) = algorithmj(ctx, e2, level + 1)?.split_ef();

            let v = ctx.new_ty_var();
            crate::unify(
                ctx,
                t1.clone(),
                TyE::pure_func(t2.clone(), TyE::pure(v.clone())),
                level + 1,
            )?;

            let result = crate::update(ctx, TyE::pure(v).with_ef(f1 | f2));
            debug_println!("{tab}[app] done: {}", result.pretty_string(ctx),);
            result
        }
        Lambda(box p, box e) => {
            debug_println!(
                "{tab}[lam] solving: λ{}.{}",
                p.pretty_string(ctx),
                p.pretty_string(ctx)
            );

            let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
            ctx.typings.push(vars);
            let (ret_t, ret_f, ret_cs) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();
            ctx.typings.pop();

            let pt = ty::update(ctx, t);
            let result = TyE::new(
                Ty::Func(TyE::pure(pt).into(), TyE::pure(ret_t).into()),
                ret_f,
                ret_cs,
            );

            let result = crate::update(ctx, result);
            debug_println!("{tab}[lam] done: {}", result.pretty_string(ctx),);
            result
        }
        Let(Rec(x, box e), e1) => {
            debug_println!(
                "{tab}[letr] solving: let rec {} = {}",
                x,
                e.pretty_string(ctx)
            );
            let v1 = TyE::pure(ctx.new_ty_var());
            let v2 = TyE::pure(ctx.new_ty_var());
            let f = ctx.new_ef_var();
            let ft = TyE::pure_func(v1.clone(), v2.clone()).with_ef(f);

            ctx.typings.push(vec![(Expr::Var(x), ft.clone())]);
            let result = algorithmj(ctx, e.clone(), level + 1)?;
            ctx.typings.pop();

            let mut result = crate::unify(ctx, ft, result, level + 1)?;
            if let Some(box e1) = e1 {
                debug_println!("{tab}{TABWIDTH}[letr] solving in {}", e1.pretty_string(ctx));
                ctx.typings.push(vec![(Expr::Var(x), result)]);
                result = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.typings.pop();
            }

            debug_println!("{tab}[letr] done: {}", result.pretty_string(ctx));
            result
        }
        Let(NonRec(box p, box e), e1) => {
            debug_println!(
                "{tab}[let] solving: let {} = {}",
                p.pretty_string(ctx),
                e.pretty_string(ctx)
            );

            let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
            let t = TyE::pure(t);

            let mut result = t.clone();
            if let Some(box e1) = e1 {
                ctx.typings.push(vars);
                result = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.typings.pop();
            }

            debug_println!(
                "{tab}[let] done: {} | t: {}",
                result.pretty_string(ctx),
                t.pretty_string(ctx),
            );
            result
        }
        Case(box e, alts) => {
            debug_println!(
                "{tab}[case] solving: case {} in {}",
                e.pretty_string(ctx),
                alts.iter()
                    .map(|a| a.pretty_string(ctx))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );

            let case_t = algorithmj(ctx, e.clone(), level + 1)?;
            let mut res_t = Ty::Infer;
            let mut res_f = Ef::Infer;
            for (p, e) in alts.into_iter().map(|a| (a.pat, a.expr)) {
                let (t, vars) = solve_collect_bindings(ctx, &p, level + 1)?;
                crate::unify(ctx, case_t.clone(), TyE::pure(t), level + 1)?;

                ctx.typings.push(vars);
                let (e_t, e_f, _) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();
                ctx.typings.pop();

                res_t = ty::unify(ctx, res_t, e_t, level + 1)?;
                res_f = res_f | e_f;
            }

            let result = TyE::new(res_t, res_f, vec![]);
            let result = crate::update(ctx, result);
            debug_println!("{tab}[case] done: {}", result.pretty_string(ctx));
            result
        }
        Handle(box e, alts) => {
            // handle expressions permit the binding handlers to effects.
            debug_println!(
                "{tab}[han] solving: handle {} with \n\t{}",
                e.pretty_string(ctx),
                alts.iter()
                    .map(|(ea)| format!("{} ~> {}", ea.pretty_string(ctx), e.pretty_string(ctx)))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );
            let (expr_t, expr_f, expr_cs) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();

            let mut fs = expr_f.into_set();
            for (f, e) in alts.into_iter().map(|a| (a.ef, a.expr)) {
                debug_println!(
                    "{tab}[han] solving: {} ~> {}",
                    f.pretty_string(ctx),
                    e.pretty_string(ctx),
                );

                let t = algorithmj(ctx, e.clone(), level + 1)?;
                if f.is_pure() {
                    continue;
                }

                let ht = solve_handler_ty(ctx, &f)?;
                let t = crate::unify(ctx, t, ht, level + 1)?;

                debug_println!(
                    "{tab}[han] done: {} ~> {}",
                    f.pretty_string(ctx),
                    t.pretty_string(ctx),
                );

                // remove handled effects from the set
                for f in f.clone().into_set() {
                    fs.remove(&f);
                }
            }

            let result = TyE::new(expr_t, Ef::from(fs), expr_cs);
            let result = crate::update(ctx, result);
            debug_println!("{tab}[han] done: {} ", result.pretty_string(ctx));
            result
        }
        Do(es) => {
            debug_println!(
                "{tab}[do ] solving: {}",
                es.iter()
                    .map(|e| e.pretty_string(ctx))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );

            let mut res_f = Ef::Pure;
            let mut result = TyE::pure(Ty::Unit);
            for e in es {
                let (t, f) = algorithmj(ctx, e, level + 1)?.split_ef();
                res_f = res_f | f;
                result = t;
            }

            let result = crate::update(ctx, result.with_ef(res_f));
            debug_println!("{tab}[do ] done: {}", result.pretty_string(ctx));
            result
        }

        Span(s, box e) => {
            ctx.spans.push(s);
            let t = algorithmj(ctx, e, level)?;
            ctx.spans.pop();
            t
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
                let def = ctx.defs.get(id).cloned().unwrap();
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
        _ => format!("invalid pattern: `{}`", p.pretty_string(ctx)).into_err(),
    }
}

pub fn solve_handler_ty(ctx: &mut Context<'_>, f: &Ef) -> diag::Result<TyE> {
    use Ef::*;
    Ok(match f {
        Effect(ef_id, ts) => {
            let effect = ctx.effects.get(ef_id).cloned().unwrap();
            let effect = effect.borrow();
            let mut t = crate::instantiate(ctx, effect.handler_ty.clone(), &mut HashMap::default());
            t
        }
        Union(fs) => {
            // TODO: lower all effects and create a handler super-type by
            // combining all the records into one.
            format!("invalid effect pattern: `{}`", f.pretty_string(ctx)).into_err()?
        }
        _ => return format!("expected effect, found `{}`", f.pretty_string(ctx)).into_err(),
    })
}
