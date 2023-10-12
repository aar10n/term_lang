use crate::{debug_println, ty, type_env, Context};
use term_core::*;
use term_diag as diag;
use term_print as print;

use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use type_env::TSet;

use std::collections::{BTreeMap, HashMap, HashSet};

pub fn algorithmj(ctx: &mut Context<'_>, e: Expr, level: usize) -> diag::Result<TyE> {
    use self::Bind::*;
    use self::Expr::*;
    use self::Lit::*;

    let tab = "  ".repeat(level);
    let result = match e {
        Type(box t) => t,

        Lit(l) => TyE::pure(l.as_ty()),
        Sym(s) => todo!(),
        Var(id) => {
            debug_println!("{tab}[var] solving: {}", id.pretty_string(ctx));
            let (e, t) = if let Some(d) = ctx.defs.get(&id) {
                (d.body.clone(), d.ty.clone())
            } else {
                match ctx.resolve_local_var(id) {
                    Some((e, t)) => (e, t),
                    None => {
                        return if let Some(span) = ctx.id_as_span(id) {
                            Diagnostic::error(
                                format!("name not found: {}", id.pretty_string(ctx)),
                                span,
                            )
                            .into_err()
                        } else {
                            Err(format!("name not found: {}", id.pretty_string(ctx))
                                .into_diagnostic())
                        }
                    }
                }
            };

            let t = crate::instantiate(ctx, t, &mut HashMap::new());
            if e == Var(id) || t.is_concrete() {
                debug_println!(
                    "{tab}[var] done: {} : {}",
                    id.pretty_string(ctx),
                    t.pretty_string(ctx)
                );
                return Ok(t);
            }

            let u = algorithmj(ctx, e, level + 1)?;
            let result = crate::unify(ctx, t, u)?;
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
            )?;

            let result = crate::update(ctx, TyE::pure(v).with_ef(f1 | f2));
            debug_println!(
                "{tab}[app] done: {} | {} -> {}",
                t1.pretty_string(ctx),
                t2.pretty_string(ctx),
                result.pretty_string(ctx),
            );
            result
        }
        Lambda(x, box e) => {
            debug_println!(
                "{tab}[lam] solving: λ{}.{}",
                x.pretty_string(ctx),
                e.pretty_string(ctx)
            );
            let v = ctx.new_ty_var();
            let f = ctx.new_ef_var();
            let t = TyE::simple(v.clone(), f.clone());

            ctx.push_typing(Expr::Var(x), t.clone());
            let (ret_t, ret_f, ret_cs) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();
            ctx.pop_typings();

            let pt = crate::ty::update(ctx, v);
            let result = TyE::new(
                Ty::Func(TyE::pure(pt).into(), TyE::pure(ret_t).into()),
                f | ret_f,
                ret_cs,
            );
            debug_println!("{tab}[lam] done: {}", result.pretty_string(ctx),);
            result
        }
        Let(Rec(x, box e), e1) => {
            debug_println!(
                "{tab}[letr] solving: let rec {} = {}",
                x,
                e.pretty_string(ctx)
            );
            let f = ctx.new_ef_var();
            let t = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);

            let ft = TyE::new(
                Ty::Func(t.clone().into(), t.clone().into()),
                f.clone(),
                vec![],
            );
            ctx.push_typing(Expr::Var(x), ft);
            let u = algorithmj(ctx, e.clone(), level + 1)?;
            ctx.pop_typings();

            let t = crate::unify(ctx, t, u)?;
            let ft = TyE::new(
                Ty::Func(t.clone().into(), t.clone().into()),
                f.clone(),
                vec![],
            );
            let mut result = t.clone();
            if let Some(box e1) = e1 {
                ctx.push_typing(Expr::Var(x), ft);
                result = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.pop_typings();
            }

            debug_println!(
                "{tab}[letr] done: {} | t: {}",
                result.pretty_string(ctx),
                t.pretty_string(ctx),
            );
            result
        }
        Let(NonRec(box p, box e), e1) => {
            debug_println!(
                "{tab}[let] solving: let {} = {}",
                p.pretty_string(ctx),
                e.pretty_string(ctx)
            );

            let t = algorithmj(ctx, e.clone(), level + 1)?;
            let vars = unify_pat(ctx, p.clone(), t.clone())?;

            let mut result = t.clone();
            if let Some(box e1) = e1 {
                ctx.push_typings(vars);
                result = algorithmj(ctx, e1.clone(), level + 1)?;
                ctx.pop_typings();
            }

            debug_println!(
                "{tab}[let] done: {} | t: {}",
                result.pretty_string(ctx),
                t.pretty_string(ctx),
            );
            result
        }
        Case(box e, alts) => {
            let case_t = algorithmj(ctx, e.clone(), level + 1)?;
            let mut res_t = Ty::Infer;
            let mut res_f = Ef::Infer;
            for (p, e) in alts.into_iter().map(|a| (a.pat, a.expr)) {
                let vars = unify_pat(ctx, p.clone(), case_t.clone())?;
                ctx.push_typings(vars);
                let (e_t, e_f, _) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();
                ctx.pop_typings();

                res_t = ty::unify(ctx, res_t, e_t)?;
                res_f = res_f | e_f;
            }
            TyE::new(res_t, res_f, vec![])
        }
        Handle(box e, alts, unhandled) => {
            // handle expressions permit the binding handlers to effects.
            debug_println!(
                "{tab}[han] solving: handle {} with \n\t{}",
                e.pretty_string(ctx),
                alts.iter()
                    .map(|(f, e)| format!("{} ~> {}", f.pretty_string(ctx), e.pretty_string(ctx)))
                    .collect::<Vec<_>>()
                    .join("\n\t")
            );
            let (expr_t, expr_f, expr_cs) = algorithmj(ctx, e.clone(), level + 1)?.into_tuple();

            let mut fs = expr_f.into_hashset();
            let mut res_t = None;
            for (f, e) in alts.into_iter().map(|a| (a.ef, a.expr)) {
                debug_println!(
                    "{tab}[han] solving: {} ~> {}",
                    f.pretty_string(ctx),
                    e.pretty_string(ctx),
                );

                let t = algorithmj(ctx, e.clone(), level + 1)?;
                if f.is_pure() {
                    res_t = Some(t);
                    continue;
                }

                let ht = solve_handler_ty(ctx, &f)?;
                let t = crate::unify(ctx, t, ht)?;

                debug_println!(
                    "{tab}[han] done: {} ~> {}",
                    f.pretty_string(ctx),
                    t.pretty_string(ctx),
                );

                // remove handled effects from the set
                for f in f.clone().into_hashset() {
                    fs.remove(&f);
                }
            }

            let res_f = Ef::from(fs);
            let result = if let Some(t) = res_t {
                t.with_ef(res_f).with_cs(expr_cs)
            } else {
                TyE::new(Ty::Unit, res_f, expr_cs)
            };

            debug_println!(
                "{tab}[han] done: {} | t: {}",
                result.pretty_string(ctx),
                expr_t.pretty_string(ctx),
            );
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

            let result = result.with_ef(res_f);
            debug_println!("{tab}[do ] done: {}", result.pretty_string(ctx));
            result
        }

        Span(s, box e) => {
            ctx.spans.push(s);
            let t = algorithmj(ctx, e, level + 1)?;
            ctx.spans.pop();
            t
        }
    };
    Ok(result)
}

pub fn solve_handler_ty(ctx: &mut Context<'_>, f: &Ef) -> diag::Result<TyE> {
    use Ef::*;
    Ok(match f {
        Infer | Pure | Mono(_) | Poly(_) => {
            return format!("expected effect, found `{}`", f.pretty_string(ctx)).into_err()
        }
        Effect(ef_id, ts) => {
            let effect = ctx.effects.get(ef_id).unwrap();
            let mut t = crate::instantiate(ctx, effect.handler_ty.clone(), &mut HashMap::default());
            t
        }
        Union(fs) => {
            // TODO: lower all effects and create a handler super-type by
            // combining all the records into one.
            format!("invalid effect pattern: `{}`", f.pretty_string(ctx)).into_err()?
        }
    })
}

pub fn unify_pat(ctx: &mut Context<'_>, p: Expr, t: TyE) -> diag::Result<Vec<(Expr, TyE)>> {
    use Expr::*;
    use Ty::*;

    let (t, f, cs) = t.into_tuple();
    let cs = crate::solve_constraints(ctx, cs)?;
    Ok(match (p, t) {
        (Lit(l), t) => {
            ty::unify(ctx, t, l.as_ty())?;
            vec![]
        }
        (Var(x), t) => vec![(Var(x), TyE::new(t, f, cs))],
        (Span(_, box e), t) => unify_pat(ctx, e, TyE::new(t, f, cs))?,
        (p, t) => {
            // ctx.ty_set.print_stdout(ctx);
            // println!("-----");
            return format!(
                "pattern `{}` ({:?}) does not match type `{}` ({:?})",
                p.pretty_string(ctx),
                p,
                t.pretty_string(ctx),
                t,
            )
            .into_err();
        }
    })
}
