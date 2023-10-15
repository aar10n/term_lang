use crate::{debug_println, Context};
use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Ef, EffectId, MonoVarId, PolyVarId, Ty, TyE};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};

use std::collections::{BTreeSet, HashMap};

pub fn unify(ctx: &mut Context<'_>, f1: Ef, f2: Ef) -> diag::Result<Ef> {
    if f1 != f2 {
        debug_println!(
            "unify_ef: {} = {}",
            f1.pretty_string(ctx),
            f2.pretty_string(ctx)
        );
    }

    use Ef::*;
    Ok(match (f1, f2) {
        (Pure, Pure) => Pure,
        (Pure, f) => f,
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
                .map(|(t1, t2)| crate::unify(ctx, t1, t2))
                .collect::<Result<Vec<_>, _>>()?;
            Effect(id1, ts)
        }
        (Union(fs1), Union(fs2)) => {
            if fs1.len() != fs2.len() {
                return Err(format!(
                    "cannot unify union types of different lengths: {} and {}",
                    fs1.len(),
                    fs2.len()
                )
                .into_diagnostic());
            }

            let mut fs = BTreeSet::new();
            for (f1, f2) in fs1.into_iter().zip(fs2.into_iter()) {
                let f = unify(ctx, f1, f2)?;
                fs.insert(f);
            }
            Ef::from(fs)
        }
        (Poly(_), _) | (_, Poly(_)) => panic!("unexpected poly_var in effect"),
        (f1, f2) => {
            ctx.ty_set.print_stdout(ctx);
            println!("--");
            ctx.ef_set.print_stdout(ctx);
            println!("-----");
            // panic!();
            println!("f1 = {:?}", f1);
            println!("f2 = {:?}", f2);
            return Err(format!(
                "expected effect `{}`, found `{}`",
                f1.pretty_string(ctx),
                f2.pretty_string(ctx)
            )
            .into_diagnostic());
        }
    })
}

pub fn update(ctx: &mut Context<'_>, f: Ef) -> Ef {
    use Ef::*;
    match f {
        Mono(x) => ctx.ef_set.find(Mono(x)),
        Effect(id, ts) => {
            let ts = ts.into_iter().map(|t| crate::update(ctx, t)).collect();
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
                .map(|t| crate::instantiate(ctx, t, ps))
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
        Mono(_) => Pure,
        // Mono(id) => match ps.get(&id) {
        //     Some(&id) => Poly(id),
        //     Some(_) => panic!("invalid mono var"),
        //     None => {
        //         let t = ctx.ids.next_poly_var_id();
        //         ps.insert(id, t);
        //         let f = Ef::Poly(t);
        //         ctx.ef_set.insert(f.clone());
        //         f
        //     }
        // },
        Effect(id, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| crate::generalize(ctx, t, ps))
                .collect();
            Effect(id, ts)
        }
        Union(fs) => Union(fs.into_iter().map(|f| generalize(ctx, f, ps)).collect()),
        f => f,
    }
}

pub fn cannonicalize(ctx: &mut Context<'_>, f: Ef) -> Ef {
    use Ef::*;
    match f {
        Infer => Pure,
        Effect(id, ts) => {
            let ts = ts
                .into_iter()
                .map(|t| crate::cannonicalize(ctx, t))
                .collect();
            Effect(id, ts)
        }
        Union(mut fs) => {
            if fs.is_empty() {
                return Ef::Pure;
            } else if fs.iter().all(|f| matches!(f, Mono(_))) {
                let mut f = fs.first().unwrap().clone();
                for f2 in fs.into_iter().skip(1) {
                    f = ctx.ef_set.union(f, f2);
                }
                return f;
            }

            if fs.len() == 1 {
                cannonicalize(ctx, fs.into_iter().next().unwrap())
            } else {
                Ef::Union(
                    fs.into_iter()
                        .map(|f| cannonicalize(ctx, f).into())
                        .collect(),
                )
            }
        }
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
        Effect(_, ts) => ts.iter().any(|t| crate::ty_occurs(x, t)),
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
        Effect(_, ts) => ts.iter().any(|t| crate::ef_occurs(x, t)),
        Union(ts) => ts.iter().any(|t| ef_occurs(x, t)),
        _ => false,
    }
}

pub fn subst_ty(r: &Ty, x: &Ty, f: Ef) -> Ef {
    use Ef::*;
    match f {
        Effect(id, ts) => Effect(
            id,
            ts.into_iter().map(|t| crate::subst_ty(r, x, t)).collect(),
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
            ts.into_iter().map(|t| crate::subst_ef(r, x, t)).collect(),
        ),
        Union(ts) => Union(ts.into_iter().map(|t| subst_ef(r, x, t)).collect()),
        f => f,
    }
}
