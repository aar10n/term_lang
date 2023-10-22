use term_core as core;
use term_diag as diag;
use term_print as print;

use core::{Bind, Context, Expr};
use print::{PrettyPrint, PrettyString};
use ustr::ustr;

pub fn transform(ctx: &mut Context, expr: Expr) -> Expr {
    // csp(ctx, expr, Expr::Sym(ustr("halt")))
    todo!()
}

fn csp2(ctx: &mut Context, expr: Expr) -> Expr {
    use Bind::*;
    use Expr::*;
    match expr {
        Type(_) | Lit(_) | Sym(_) | Var(_) => expr,
        Record(fields) => {
            let fields = fields
                .clone()
                .into_iter()
                .map(|(k, v)| (k, csp2(ctx, v)))
                .collect();
            Record(fields)
        }
        Apply(box a, box b) => {
            // handle multi-argument application
            let mut es = vec![b];
            let mut f = a;
            while let Apply(box a, box b) = f {
                es.push(b);
                f = a;
            }

            let f = csp2(ctx, f);
            let e = fold_left(es, None, Expr::apply);
            Apply(f.into(), e.into())
        }
        Lambda(box p, box e) => {
            // handle curried lambda -- sounds tasty :P
            let mut ps = vec![p];
            let mut body = e;
            while let Lambda(box p, box e) = body {
                ps.push(p);
                body = e;
            }
            fold_right(ps, Some(csp2(ctx, body)), Expr::lambda)
        }
        Let(NonRec(box p, box e), e1) => {
            let e1 = csp2(ctx, e1.map(|x| *x).unwrap_or(Expr::unit()));
            let e = csp2(ctx, e);
            Let(NonRec(p.into(), e.into()), Some(e1.into()))
        }
        Let(Rec(v, box e), e1) => {
            let e1 = csp2(ctx, e1.map(|x| *x).unwrap_or(Expr::unit()));
            let e = csp2(ctx, e);
            Let(Rec(v, e.into()), Some(e1.into()))
        }
        Case(box e, alts) => {
            let e = csp2(ctx, e);
            let alts = alts
                .into_iter()
                .map(|mut alt| {
                    alt.expr = csp2(ctx, alt.expr);
                    alt
                })
                .collect();
            Case(e.into(), alts)
        }
        Handle(box e, None) => {
            let e = csp2(ctx, e);
            Handle(e.into(), None)
        }
        Handle(box e, Some(alts)) => {
            let e = csp2(ctx, e);
            let alts = alts
                .into_iter()
                .map(|mut alt| {
                    alt.expr = csp2(ctx, alt.expr);
                    alt
                })
                .collect();
            Handle(e.into(), Some(alts))
        }
        Do(es) => {
            new_cont(ctx, move |ctx, k| {
                // let es = es.into_iter().map(|e| csp2(ctx, e)).collect::<Vec<_>>();
                // Apply()
                todo!()
            })
        }
        Span(s, box e) => Span(s, csp2(ctx, e).into()),
    }
}

// fn csp(ctx: &mut Context, expr: Expr, k: Expr) -> Expr {
//     if is_atomic(&expr) {
//         return Apply(k.into(), expr.into());
//     }

//     use Bind::*;
//     use Expr::*;
//     match expr {
//         Type(_) | Lit(_) | Sym(_) | Var(_) => Apply(k.into(), expr.into()),
//         Record(_) if is_atomic(&expr) => Apply(k.into(), expr.into()),
//         Record(fields) => {
//             let e = new_cont(ctx, |ctx, k| {
//                 let fields = fields
//                     .clone()
//                     .into_iter()
//                     .map(|(k_, v)| (k_, csp(ctx, v, k.clone())))
//                     .collect();
//                 Apply(k.into(), Record(fields).into())
//             });
//             Apply(k.into(), e.into())
//         }
//         Apply(box a, box b) => {
//             // handle multi-argument application
//             let mut es = vec![b];
//             let mut f = a;
//             while let Apply(box a, box b) = f {
//                 es.push(b);
//                 f = a;
//             }

//             let f = csp(ctx, f, k.clone());
//             let e = if es.iter().all(is_atomic) {
//                 Apply(f.into(), fold_left(es, None, Expr::apply).into())
//             } else {
//                 new_cont(ctx, |ctx, k| {
//                     let expr = Apply(
//                         f.clone().into(),
//                         fold_left(es.clone(), None, Expr::apply).into(),
//                     );
//                     Apply(k.into(), expr.into())
//                 })
//             };
//             Apply(k.into(), e.into())
//         }
//         Lambda(box p, box e) => {
//             // handle curried lambda -- sounds tasty :P
//             let mut ps = vec![p];
//             let mut body = e;
//             while let Lambda(box p, box e) = body {
//                 ps.push(p);
//                 body = e;
//             }

//             let e = if is_atomic(&body) {
//                 fold_right(ps, Some(body), Expr::lambda)
//             } else {
//                 new_cont(ctx, |ctx, k| {
//                     let e = csp(ctx, body.clone(), k.clone());
//                     let e = fold_right(ps.clone(), Some(e), Expr::lambda);
//                     Apply(k.into(), e.into())
//                 })
//             };
//             Apply(k.into(), e.into())
//         }
//         Let(NonRec(box p, box e), e1) => {
//             let e1 = e1.map(|x| *x).unwrap_or(Expr::unit());
//             let c = new_cont(ctx, |ctx, k| {
//                 let e1 = csp(ctx, e1.clone(), k.clone());
//                 Apply(k.into(), e1.into())
//             });

//             let e = csp(ctx, e.clone(), c);
//             let e = Let(NonRec(p.into(), e.into()), Some(e1.into()));
//             Apply(k.into(), e.into())
//         }
//         Let(Rec(v, box e), e1) => {
//             let e1 = e1.map(|x| *x).unwrap_or(Expr::unit());
//             let c = new_cont(ctx, |ctx, k| {
//                 let e1 = csp(ctx, e1.clone(), k.clone());
//                 Apply(k.into(), e1.into())
//             });

//             let e = csp(ctx, e.clone(), c);
//             let e = Let(Rec(v, e.into()), Some(e1.into()));
//             Apply(k.into(), e.into())
//         }
//         Case(box e, alts) => {
//             let e = csp(ctx, e, k.clone());
//             let alts = alts
//                 .into_iter()
//                 .map(|mut alt| {
//                     alt.expr = csp(ctx, alt.expr, k.clone());
//                     alt
//                 })
//                 .collect();
//             let e = Case(e.into(), alts);
//             Apply(k.into(), e.into())
//         }
//         Handle(box e, None) => {
//             let e = csp(ctx, e, k.clone());
//             let e = Handle(e.into(), None);
//             Apply(k.into(), e.into())
//         }
//         Handle(box e, Some(alts)) => {
//             let e = csp(ctx, e, k.clone());
//             let alts = alts
//                 .into_iter()
//                 .map(|mut alt| {
//                     alt.expr = csp(ctx, alt.expr, k.clone());
//                     alt
//                 })
//                 .collect();
//             let e = Handle(e.into(), Some(alts));
//             Apply(k.into(), e.into())
//         }
//         Do(es) => {
//             let e = if let Some((e, es)) = es.split_first() {
//                 let _ = csp(ctx, e.clone(), k.clone());
//                 Do(es.to_vec())
//             } else if let Some(e) = es.last() {
//                 csp(ctx, e.clone(), k.clone())
//             } else {
//                 panic!("invalid do expression")
//             };
//             Apply(k.into(), e.into())
//         }
//         Span(s, box e) => Span(s, csp(ctx, e, k).into()),
//     }
// }

fn new_cont(ctx: &mut Context, cont: impl Fn(&mut Context, Expr) -> Expr) -> Expr {
    use Expr::*;
    let v = ctx.ids.next_var_id();
    ctx.register_id_name(v, ustr("k"), core::Span::default());
    let k = Var(v);
    let e = cont(ctx, k.clone());
    Lambda(k.into(), e.into())
}

fn is_atomic(expr: &Expr) -> bool {
    use Bind::*;
    use Expr::*;
    match expr {
        Type(_) | Lit(_) | Sym(_) | Var(_) => true,
        Record(fields) => fields.iter().all(|(_, v)| is_atomic(v)),
        Lambda(_, box e) => is_atomic(e),
        Let(NonRec(_, box e), e1) => is_atomic(e) && e1.iter().all(|e1| is_atomic(e1)),
        Let(Rec(_, box e), e1) => is_atomic(e) && e1.iter().all(|e1| is_atomic(e1)),
        Span(_, box e) => is_atomic(e),
        // Case(box e, alts) => is_atomic(e) && alts.iter().all(|alt| is_atomic(&alt.expr)),
        // Handle(..) => false,
        _ => false,
    }
}

fn fold_left<T>(es: impl IntoIterator<Item = T>, init: Option<T>, f: impl Fn(T, T) -> T) -> T {
    let mut es = es.into_iter();
    let mut e = init.unwrap_or_else(|| es.next().unwrap());
    for x in es {
        e = f(e, x);
    }
    e
}

fn fold_right<T>(es: impl IntoIterator<Item = T>, init: Option<T>, f: impl Fn(T, T) -> T) -> T {
    let mut es = es.into_iter();
    let mut e = init.unwrap_or_else(|| es.next().unwrap());
    for x in es {
        e = f(x, e);
    }
    e
}
