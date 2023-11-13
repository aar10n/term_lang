use crate::{constraint, ef, trace_println, ty, type_env, Context};
use term_core as core;
use term_diag as diag;
use term_print as print;

use constraint::cs;
use core::*;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString, TABWIDTH};
use ustr::Ustr;

use either::*;
use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    iter::zip,
};

/// Infers the type of an expression. Returns the inferred type and updated expression.
///
/// This is an implementation of the Hindley-Milner type inference algorithm extended
/// to support algebraic effects. It not only infers the complete type of an expression,
/// but also may transform the expression to insert implicit casts and coercions, and
/// resolve ambiguous symbols.
///
/// *Note:* The returned type is not generalized.
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
        Lit(l) => {
            let t = match l {
                Unit => TyE::pure(Ty::Unit),
                Bool(_) => ctx.core.get_primitive_ty("Bool"),
                Char(_) => ctx.core.get_primitive_ty("Char"),
                Int(_) => {
                    let (mut ps, cs) = constraint::class_constraint(ctx, "Num", vec![])?;
                    TyE::pure(ps.pop().unwrap()).with_cs(cs)
                }
                Double(_) => {
                    let (mut ps, cs) = constraint::class_constraint(ctx, "Frac", vec![])?;
                    TyE::pure(ps.pop().unwrap()).with_cs(cs)
                }
            };

            (Lit(l), t)
        }
        Sym(s) => (Sym(s), TyE::simple(ctx.new_ty_var(), ctx.new_ef_var())),
        Var(id) => {
            trace_println!(ctx, "{tab}[var] solving: {}", id.pretty_string(ctx.core));
            let (e, t) = if let Some(t) = ctx.typ_env.get(&Var(id)).cloned() {
                let t = update(ctx, t);
                (Expr::Var(id), cannonicalize(ctx, t))
            } else if let Some(d) = ctx.core.defs.get(&id).cloned() {
                let d = d.borrow();
                let t = instantiate(ctx, d.ty.clone(), &mut HashMap::new());
                ctx.typ_env.insert(Expr::Var(id), t.clone());
                (d.body.clone(), t)
            } else {
                // TODO: better error handling/reporting
                println!("-------");
                ctx.typ_env.print_stdout(&ctx.core);
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
                trace_println!(
                    ctx,
                    "{tab}[var] done: {} : {}",
                    id.pretty_string(ctx.core),
                    t.pretty_string(ctx.core)
                );
                return Ok((Var(id), t));
            }

            let (e, e_t) = algorithmj(ctx, e, level + 1)?;
            let e_t = unify(ctx, t, e_t, level + 1)?;
            trace_println!(ctx, "{tab}[var] done: {}", e_t.pretty_string(ctx.core));
            (Var(id), e_t)
        }
        Type(box t) => (Expr::Type(t.clone().into()), t),

        Cast(box e, box t) => {
            trace_println!(
                ctx,
                "{tab}[cast] solving: {} : {}",
                e.pretty_string(ctx.core),
                t.pretty_string(ctx.core)
            );

            todo!()
        }
        Apply(..) => {
            let (e, args) = e.uncurry_apply();
            trace_println!(
                ctx,
                "{tab}[app] solving: <n={}> {}",
                args.len(),
                e.pretty_string(ctx.core),
            );

            let mut es = vec![];
            let mut ts = vec![];
            let mut fs = BTreeSet::new();
            for e in args {
                trace_println!(
                    ctx,
                    "{ttab}[app] solving arg: {}",
                    e.pretty_string(ctx.core)
                );

                let (e, t) = algorithmj(ctx, e, level + 2)?;
                trace_println!(ctx, "{ttab}[arg] arg: {}", t.pretty_string(ctx.core));
                let (t, f) = t.split_ef();
                es.push(e);
                ts.push(t);
                fs.insert(f);
            }

            let res_t = TyE::pure(ctx.new_ty_var());
            let (e, es, ts) = if e.is_sym() && let Sym(s) = e.clone().unwrap_inner() {
                trace_println!(ctx, "{ttab}[app] resolving func: {}", s);
                let (id, es, ts) = resolve_symbol(ctx, s, zip(es, ts), res_t.clone(), level + 2)?;
                trace_println!(ctx, "{ttab}[app] resolved {} to {}", s, id.pretty_string(ctx.core));
                (Expr::Var(id), es, ts)
            } else {
                (e, es, ts)
            };

            let (e, t) = algorithmj(ctx, e, level + 1)?;
            let (t, f) = t.split_ef();
            fs.insert(f);

            let f_t = unify(ctx, t, TyE::nary_func(ts, res_t.clone()), level + 1)?;
            let res_t = update(ctx, res_t.with_ef(Ef::from(fs)));
            let res_e = Expr::apply_n(e, es);
            trace_println!(ctx, "{tab}[app] done: {}", res_t.pretty_string(ctx.core));
            (res_e, res_t)
        }
        Lambda(box p, box e) => {
            trace_println!(
                ctx,
                "{tab}[lam] solving: λ{}.{}",
                p.pretty_string(ctx.core),
                e.pretty_string(ctx.core)
            );

            let (t, vars) = solve_pat(ctx, &p, level + 1)?;
            ctx.typ_env.push(vars);
            let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (e_t, e_f, e_cs) = e_t.into_tuple();
            ctx.typ_env.pop();

            let pt = ty::update(ctx, t);
            let result = TyE::new(
                Ty::Func(TyE::pure(pt).into(), TyE::pure(e_t).into()),
                e_f,
                e_cs,
            );

            let result = update(ctx, result);
            trace_println!(ctx, "{tab}[lam] done: {}", result.pretty_string(ctx.core),);
            (Lambda(p.into(), e.into()), result)
        }
        Case(box e, alts) => {
            trace_println!(
                ctx,
                "{tab}[case] solving: case {} in",
                e.pretty_string(ctx.core),
            );

            let (case_e, case_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (case_t, case_f) = case_t.split_ef();

            let mut new_alts = vec![];
            let mut res_t = TyE::pure(Ty::Infer);
            let mut fs = BTreeSet::new();
            for (p, e) in alts.into_iter() {
                trace_println!(
                    ctx,
                    "{ttab}[case] solving: {} -> {}",
                    p.pretty_string(ctx.core),
                    e.pretty_string(ctx.core)
                );
                let (p_t, vars) = solve_pat(ctx, &p, level + 2)?;

                // chceck pattern against the case expression
                unify(ctx, TyE::pure(p_t), case_t.clone(), level + 2)?;

                ctx.typ_env.push(vars);
                let (e, t) = algorithmj(ctx, e.clone(), level + 2)?;
                let (t, f) = t.split_ef();
                ctx.typ_env.pop();

                new_alts.push((p, e));
                res_t = unify(ctx, res_t, t, level + 2)?;
                fs.insert(f);
            }

            let res_t = update(ctx, res_t.with_ef(Ef::from(fs)));
            trace_println!(ctx, "{tab}[case] done: {}", res_t.pretty_string(ctx.core));
            (Case(e.into(), new_alts), res_t)
        }
        Handle(box e, Some(alts)) => {
            //  handle <expr> with
            //    [<ef> ~> <handler>]...
            //
            // A `handle` expression matches the effects produced by the given
            // expression against the given handlers. The type of the `handle`
            // expression is the type of the given expression with all handled
            // effects removed.

            trace_println!(
                ctx,
                "{tab}[han] solving: handle {}",
                e.pretty_string(ctx.core),
            );

            let (han_e, han_t) = algorithmj(ctx, e.clone(), level + 1)?;
            let (han_t, han_f) = han_t.split_ef();

            let mut new_alts = vec![];
            let mut fs = han_f.into_set();
            for (f, e) in alts.into_iter() {
                trace_println!(
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

                trace_println!(
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
            trace_println!(ctx, "{tab}[han] done: {} ", result.pretty_string(ctx.core));
            (Handle(han_e.into(), Some(new_alts)), result)
        }
        Handle(box e, None) => {
            //  handle default <expr>
            //
            // A `handle` expression with no handlers defines a `handle default`
            // expression which causes default handler binding for all effects in
            // the handled expression. The type of the `handle default` expression
            // is the type of given the expression with all bound effects removed.

            trace_println!(
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
                    let effect = ctx.core.effects.get(&ef_id).cloned().unwrap();
                    let effect = effect.borrow();
                    if let Some((_, var_id)) = effect.default {
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
            trace_println!(ctx, "{tab}[han] done: {} ", result.pretty_string(ctx.core));
            (Handle(e.into(), Some(new_alts)), result)
        }
        Do(es) => {
            // do <expr>
            //   [expr]...
            //
            // The type of a `do` expression is the type of its last expression
            // effected by the sum of all effects produced by the expressions.

            trace_println!(
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
            trace_println!(ctx, "{tab}[do ] done: {}", result.pretty_string(ctx.core));
            (Do(new_es), result)
        }
        Let(bs, e) => {
            trace_println!(ctx, "{tab}[let] solving: let <bindings> in <expr>");

            let mut vars = vec![];
            let mut new_bs = vec![];
            for b in bs {
                match b {
                    NonRec(box p, box e) => {
                        trace_println!(
                            ctx,
                            "{ttab}[let] solving: let {} = {}",
                            p.pretty_string(ctx.core),
                            e.pretty_string(ctx.core)
                        );

                        let (t, vs) = solve_pat(ctx, &p, level + 1)?;
                        ctx.typ_env.push(vs);
                        let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                        ctx.typ_env.pop();
                        let t = unify(ctx, t.into(), e_t, level + 1)?;

                        trace_println!(ctx, "{ttab}[let] done: {}", t.pretty_string(ctx.core));

                        vars.push((p.clone(), t));
                        new_bs.push(NonRec(p.into(), e.into()));
                    }
                    Rec(x, box e) => {
                        trace_println!(
                            ctx,
                            "{ttab}[let] solving: let rec {} = {}",
                            x.pretty_string(ctx.core),
                            e.pretty_string(ctx.core)
                        );

                        let v1 = TyE::pure(ctx.new_ty_var());
                        let v2 = TyE::pure(ctx.new_ty_var());
                        let f = ctx.new_ef_var();
                        let ft = TyE::func(v1.clone(), v2.clone()).with_ef(f);

                        ctx.typ_env.push(vec![(Expr::Var(x), ft.clone())]);
                        let (e, e_t) = algorithmj(ctx, e.clone(), level + 1)?;
                        ctx.typ_env.pop();
                        let t = unify(ctx, ft, e_t, level + 1)?;

                        trace_println!(ctx, "{ttab}[letr] done: {}", t.pretty_string(ctx.core));

                        vars.push((Expr::Var(x), t));
                        new_bs.push(Rec(x, e.into()));
                    }
                }
            }

            let (e, t) = if let Some(box e) = e {
                trace_println!(ctx, "{ttab}[let] solving in: {}", e.pretty_string(ctx.core));

                ctx.typ_env.push(vars);
                let (e, e_t) = algorithmj(ctx, e, level + 1)?;
                ctx.typ_env.pop();
                let result = update(ctx, e_t);
                trace_println!(ctx, "{ttab}[let] done: {}", result.pretty_string(ctx.core));

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
                trace_println!(
                    ctx,
                    "{tab}[rec] solving: {} = {}",
                    n,
                    e.pretty_string(ctx.core)
                );
                let (e, t) = algorithmj(ctx, e.clone(), level + 1)?;
                trace_println!(ctx, "{tab}[rec] done: {}", t.pretty_string(ctx.core));
                es.insert(n.clone(), e);
                rec.insert(n, t);
            }
            (Record(es), TyE::pure(Ty::Record(rec)))
        }
        RecSel(box e, key) => {
            trace_println!(
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
                    trace_println!(ctx, "{tab}[sel] done: {}", result.pretty_string(ctx.core));
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

/// Resolves an ambiguous symbol to an implementation.
///
/// This function attempts to find a function to satsify the given arguments,
/// possibly performing type coercion if necessary. It first checks the expected
/// signature against the registered implementations. Then it looks for any
/// conversions that can be applied to the arguments to satisfy the signature.
fn resolve_symbol(
    ctx: &mut Context<'_>,
    name: Ustr,
    args: impl IntoIterator<Item = (Expr, TyE)>,
    res_t: TyE,
    level: usize,
) -> diag::Result<(VarId, Vec<Expr>, Vec<TyE>)> {
    let tab = TABWIDTH.repeat(level);
    let (mut es, mut ts): (Vec<_>, Vec<_>) = args.into_iter().unzip();
    let f_t = TyE::nary_func(ts.clone(), res_t.clone());

    let mut errs = vec![];
    for (id, _) in ctx.core.functions[&name].clone() {
        trace_println!(ctx, "{tab}trying: {}", id.pretty_string(ctx.core));
        let method_ty = {
            let def = ctx.core.defs[&id].clone();
            let def = def.borrow();
            instantiate(ctx, def.ty.clone(), &mut HashMap::default())
        };

        let err = match unify(ctx, f_t.clone(), method_ty, level + 1) {
            Ok(ty) => return Ok((id, es, ts)),
            Err(err) => err,
        };
        errs.push(err);
    }

    // match &mut ts[..] {
    //     [t1, t2] => {
    //         // look for a conversion from t2 to t1
    //     }
    //     _ => {}
    // }

    if errs.len() == 1 {
        errs.pop().unwrap().into_err()
    } else {
        let f_t = generalize(ctx, f_t, &mut HashMap::default());
        format!(
            "no matching implementation found for `{}` with type `{}`",
            name,
            f_t.pretty_string(ctx.core)
        )
        .into_err()
    }
}

/// Solves the type of a pattern expression. Returns the expected type and a list
/// of variable bindings produced by the pattern.
///
/// The provided expression must be a valid pattern expression.
fn solve_pat(
    ctx: &mut Context<'_>,
    p: &Expr,
    level: usize,
) -> diag::Result<(Ty, Vec<(Expr, TyE)>)> {
    use Expr::*;
    match p {
        Lit(l) => Ok((l.as_ty(&ctx.core), vec![])),
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

fn solve_constraints(
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

fn solve_handler_ty(ctx: &mut Context<'_>, f: &Ef) -> diag::Result<TyE> {
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

pub fn unify(ctx: &mut Context<'_>, t1: TyE, t2: TyE, level: usize) -> diag::Result<TyE> {
    let t1 = cannonicalize(ctx, t1);
    let t2 = cannonicalize(ctx, t2);
    if t1 == t2 {
        return Ok(t1);
    }

    // let tab = "  ".repeat(level);
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
