use crate::{debug_println, ty, type_env, Context};
use term_core::*;
use term_diag as diag;
use term_print as print;

use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use type_env::TSet;

use std::collections::{BTreeMap, HashMap, HashSet};

pub fn algorithmj(ctx: &mut Context<'_>, e: Expr) -> diag::Result<TyE> {
    use self::Bind::*;
    use self::Expr::*;
    use self::Lit::*;
    Ok(match e {
        Type(box t) => t,
        Wildcard => TyE::pure(Ty::Infer),
        Lit(l) => TyE::pure(l.as_ty()),
        Sym(s) => todo!(),
        Var(id) => {
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
            if matches!(e, Expr::Var(_)) || t.is_concrete() {
                return Ok(t);
            }

            let u = algorithmj(ctx, e)?;
            let result = crate::unify(ctx, t, u)?;
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

            let f = ctx.new_ef_var();
            let result = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);
            let rt = crate::unify(
                ctx,
                t1.clone(),
                TyE::new(
                    Ty::Func(t2.clone().into(), result.clone().into()),
                    f,
                    vec![],
                ),
            )?;

            let result = crate::update(ctx, result);
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
            let f = ctx.new_ef_var();
            let t = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);

            ctx.push_typing(Expr::Var(x), t.clone());
            let rt = algorithmj(ctx, e.clone())?;
            ctx.pop_typings();

            let pt = crate::update(ctx, t);
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
            let f = ctx.new_ef_var();
            let t = TyE::new(ctx.new_ty_var(), f.clone(), vec![]);

            let ft = TyE::new(
                Ty::Func(t.clone().into(), t.clone().into()),
                f.clone(),
                vec![],
            );
            ctx.push_typing(Expr::Var(x), ft);
            let u = algorithmj(ctx, e.clone())?;
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
            let vars = unify_pat(ctx, p.clone(), t.clone())?;

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
                let vars = unify_pat(ctx, p.clone(), case_t.clone())?;
                ctx.push_typings(vars);
                let (e_t, e_f, _) = algorithmj(ctx, e.clone())?.into_tuple();
                ctx.pop_typings();

                res_t = ty::unify(ctx, res_t, e_t)?;
                res_f = res_f | e_f;
            }
            TyE::new(res_t, res_f, vec![])
        }
        Handle(box e, alts) => {
            let expr_t = algorithmj(ctx, e.clone())?;
            let mut res_t = Ty::Infer;
            let mut res_f = Ef::Infer;
            for (f, e) in alts {
                let (e_t, e_f, _) = algorithmj(ctx, e.clone())?.into_tuple();
                ctx.pop_typings();

                res_t = ty::unify(ctx, res_t, e_t)?;
                res_f = res_f | e_f;
            }
            TyE::new(res_t, res_f, vec![])
        }
        Do(_) => todo!(),

        Span(s, box e) => {
            ctx.spans.push(s);
            let t = algorithmj(ctx, e)?;
            ctx.spans.pop();
            t
        }
    })
}

pub fn unify_pat(ctx: &mut Context<'_>, p: Expr, t: TyE) -> diag::Result<Vec<(Expr, TyE)>> {
    use Expr::*;
    use Ty::*;

    let (t, f, cs) = t.into_tuple();
    let cs = crate::solve_constraints(ctx, cs)?;
    Ok(match (p, t) {
        (Wildcard, _) => vec![],
        (Lit(l), t) => {
            ty::unify(ctx, t, l.as_ty())?;
            vec![]
        }
        (Var(x), t) => vec![(Var(x), TyE::pure(t))],
        (Span(_, box e), t) => unify_pat(ctx, e, TyE::pure(t))?,
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
