// use crate::*;
use super::Context;
use term_ast as ast;
use term_ast::visit::{Visit, Visitor};
use term_core as core;
use term_diag as diag;
use term_print as print;

use ast::Spanned;
use core::{DataConId, DataId, EffectOpId, Id, PolyVarId, Span, TyE, VarId};
use diag::{error_for, Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;

use std::collections::{BTreeMap, BTreeSet};
use std::vec;
use ustr::{ustr, UstrMap};

pub trait Lower {
    type Target: Clone;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target>;

    fn lower_ast_core(
        &self,
        ast: &mut ast::Context,
        core: &mut core::Context,
    ) -> diag::Result<Self::Target> {
        let mut ctx = (ast, core).into();
        self.lower(&mut ctx)
    }
}

impl<T> Lower for Box<T>
where
    T: Lower,
{
    type Target = T::Target;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        (**self).lower(ctx)
    }
}

impl<T: Lower> Lower for Vec<T> {
    type Target = Vec<T::Target>;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        let mut out = Vec::with_capacity(self.len());
        for t in self {
            out.push(t.lower(ctx)?);
        }
        Ok(out)
    }
}

impl<T: Lower> Lower for ast::Node<T> {
    type Target = T::Target;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        self.kind.lower(ctx)
    }
}

impl Lower for ast::Ty {
    type Target = core::TyE;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::TyKind;
        use core::{Id, Ty, TyE};
        let t = match &self.kind {
            TyKind::Infer => Ty::Infer.into(),
            TyKind::Never => Ty::Never.into(),
            TyKind::Unit => Ty::Unit.into(),
            TyKind::Int => primitive("Int").into(),
            TyKind::Float => primitive("Float").into(),
            TyKind::Bool => primitive("Bool").into(),
            TyKind::Char => primitive("Char").into(),
            TyKind::String => {
                let list_id = expect_data(ctx, "List")?;
                Ty::Data(list_id, vec![primitive("Char").into()]).into()
            }
            TyKind::Name(n) => match unwrap_resolved_ident(n)? {
                Id::Data(id) => TyE::pure(Ty::Data(id, vec![])),
                Id::PolyVar(id) => TyE::pure(Ty::Poly(id)),
                _ => panic!("unexpected id: {:?}", n),
            },
            TyKind::Inst(n, ts) => {
                let id = unwrap_resolved_ident(n)?.data_id();
                Ty::Data(id, ts.lower(ctx)?).into()
            }
            TyKind::Func(a, b) => {
                let a = a.lower(ctx)?;
                let b = b.lower(ctx)?;
                Ty::Func(a.into(), b.into()).into()
            }
            TyKind::List(t) => {
                let list_id = expect_data(ctx, "List")?;
                Ty::Data(list_id, vec![t.lower(ctx)?]).into()
            }
            TyKind::Tuple(ts) => {
                let ty_name = format!("Tuple{}", ts.len());
                let tuple_id = expect_data(ctx, &ty_name)?;
                Ty::Data(tuple_id, ts.lower(ctx)?).into()
            }
            TyKind::Record(fs) => {
                let mut fields = BTreeMap::new();
                for (n, t) in fs {
                    fields.insert(n.raw, t.lower(ctx)?);
                }
                Ty::Record(fields).into()
            }
            TyKind::Effect(t, f) => {
                let (t, f1, cs) = t.lower(ctx)?.into_tuple();
                let f = f.lower(ctx)? | f1;
                TyE::new(t, f, cs)
            }
            TyKind::Forall(ps, t) => {
                let (ps, mut cs) = ps.lower(ctx)?;
                let (t, f, cs1) = t.lower(ctx)?.into_tuple();
                cs.extend(cs1);
                TyE::new(t, f, cs)
            }
        };
        Ok(t)
    }
}

impl Lower for ast::Ef {
    type Target = core::Ef;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::EfKind;
        use core::{Ef, Id, Ty};
        Ok(match &self.kind {
            EfKind::Infer => Ef::Infer,
            EfKind::Pure => Ef::Pure,
            EfKind::Name(n, ts) => {
                let id = unwrap_resolved_ident(n)?.effect_id();
                let ts = ts.lower(ctx)?;
                Ef::Effect(id, ts)
            }
            EfKind::Union(fs) => {
                let fs = fs.lower(ctx)?;
                let mut fs = BTreeSet::from_iter(fs);
                fs.remove(&Ef::Pure);
                if fs.is_empty() {
                    Ef::Pure
                } else if fs.len() == 1 {
                    fs.into_iter().next().unwrap()
                } else {
                    Ef::Union(fs)
                }
            }
        })
    }
}

impl Lower for ast::DataDecl {
    type Target = (core::Data, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Data, Def, Ty, TyE};
        let id = self.name.id.unwrap().data_id();
        let span = ctx.core.id_as_span(id).unwrap();
        let (params, constraints) = self.ty_params.lower(ctx)?;
        let cons = self.cons.lower(ctx)?;

        let param_tys = ids_to_tys(params.clone());
        let data_ty = TyE::pure(Ty::Data(id, param_tys.clone()));

        // create the constructor built-in function definitions
        let mut defs = vec![];
        for con in &cons {
            let id = con.var_id;
            let ty = if con.fields.is_empty() {
                data_ty.clone()
            } else {
                make_ty_func(ctx, con.fields.iter().cloned(), data_ty.clone())
            };
            defs.push(Def::new_builtin(id, ty));
        }

        let data = Data {
            id,
            params,
            constraints,
            cons,
        };
        Ok((data, defs))
    }
}

impl Lower for ast::DataConDecl {
    type Target = core::DataCon;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::DataCon;
        let id = unwrap_resolved_ident(&self.name)?.data_con_id();
        let var_id = ctx.ast.con_var_ids[&id];
        ctx.core
            .register_id_name(var_id, self.name.raw, self.name.span());

        let fields = self.fields.lower(ctx)?;
        Ok(DataCon { id, var_id, fields })
    }
}

impl Lower for ast::EffectDecl {
    type Target = (core::Effect, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Ef, Effect, Ty, TyE};
        let id = self.name.id.unwrap().effect_id();
        let (params, constraints) = self.ty_params.lower(ctx)?;
        let combining_efs = self.side_efs.lower(ctx)?;
        let ops = self.ops.lower(ctx)?;

        let param_tys = ids_to_tys(params.clone());
        let effect = Ef::Effect(id, param_tys.clone());

        let handler_ty = {
            let mut rec = BTreeMap::new();
            for (name, op) in self.ops.iter().map(|x| x.name.raw).zip(ops.iter()) {
                let (t, f, cs) = op.ty.clone().into_tuple();
                let f = f | Ef::Union(BTreeSet::from_iter(combining_efs.clone()));
                let ty = TyE::new(t, f, cs);
                rec.insert(name, ty);
            }
            TyE::pure(Ty::Record(rec))
        };

        // create the op function definitions
        let mut defs = vec![];
        for op in &ops {
            // an effect operation with the type `a -> b` corresponds to a global
            // effectful function `a -> b ~ <effect>`.
            let id = ctx.ast.op_var_ids[&op.id];
            let (op_t, op_f, op_cs) = op.ty.clone().into_tuple();

            let f = op_f | effect.clone() | Ef::Union(BTreeSet::from_iter(combining_efs.clone()));
            let ty = TyE::new(op_t, f, op_cs);
            defs.push(Def::new_builtin(id, ty));
        }

        let effect = Effect {
            id,
            params,
            constraints,
            combining_efs,
            ops,
            handler_ty,
            handlers: vec![],
            default: None,
        };
        Ok((effect, defs))
    }
}

impl Lower for ast::EffectOpDecl {
    type Target = core::EffectOp;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        let id = unwrap_resolved_ident(&self.name)?.effect_op_id();
        let ty = self.ty.lower(ctx)?;
        Ok(core::EffectOp { id, ty })
    }
}

impl Lower for ast::EffectHandler {
    type Target = (core::Handler, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Expr, Handler, Ty};
        let effect_id = unwrap_resolved_ident(&self.effect)?.effect_id();
        let id = self.name.id.unwrap().handler_id();
        let var_id = ctx.ast.handler_var_ids[&id];
        let (params, constraints) = self.ty_args.lower(ctx)?;

        let mut rec_exprs = BTreeMap::new();
        let mut rec_tys = BTreeMap::new();
        let mut ops = BTreeMap::new();
        let mut defs = vec![];
        for op in self.ops.iter() {
            let name = op.name.raw;
            let (op_id, def) = op.lower(ctx)?;
            ops.insert(op_id, def.id);
            rec_exprs.insert(name.clone(), Expr::Var(def.id));
            rec_tys.insert(name.clone(), def.ty.clone());
            defs.push(def);
        }

        let handler_ty = TyE::pure(Ty::Record(rec_tys));
        let handler_def = Def::new(var_id, handler_ty, Expr::Record(rec_exprs));
        defs.push(handler_def);

        let handler = Handler {
            effect_id,
            id,
            params,
            constraints,
            ops,
        };
        Ok((handler, defs))
    }
}

impl Lower for ast::EffectOpImpl {
    type Target = (EffectOpId, core::Def);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Expr, Ty, TyE};

        let op_id = match self.op_id {
            Some(id) => id,
            None => format!("expected op id").into_err()?,
        };
        let id = unwrap_resolved_ident(&self.name)?.var_id();
        let body = if self.params.is_empty() {
            self.expr.lower(ctx)? // variable
        } else {
            lower_lambda(ctx, &self.params, &self.expr)? // function
        };

        let ty = TyE::infer();
        Ok((op_id, Def::new(id, ty, body)))
    }
}

impl Lower for ast::ClassDecl {
    type Target = core::Class;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Class;
        let id = self.name.id.unwrap().class_id();
        let (params, constraints) = self.ty_params.lower(ctx)?;
        let methods = self.members.lower(ctx)?;

        Ok(Class {
            id,
            params,
            constraints,
            methods,
            insts: vec![],
        })
    }
}

impl Lower for ast::MemberDeclKind {
    type Target = core::Method;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::MemberDeclKind;
        use core::Method;
        Ok((match &self {
            MemberDeclKind::AssocTy(_) => todo!(),
            MemberDeclKind::Method(method) => method.lower(ctx)?,
        }))
    }
}

impl Lower for ast::MethodDecl {
    type Target = core::Method;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Method;
        let id = unwrap_resolved_ident(&self.name)?.decl_id();
        let ty = self.ty.lower(ctx)?;
        Ok(Method { id, ty })
    }
}

impl Lower for ast::ClassInst {
    type Target = core::Inst;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Inst;
        let class_id = self.class.id.unwrap().class_id();
        let id = self.id.unwrap();
        let (params, constraints) = self.ty_args.lower(ctx)?;

        let mut methods = vec![];
        for def in self.members.lower(ctx)? {
            methods.push(def.id);
            ctx.core.defs.insert(def.id, RefCell::new(def).into());
        }

        Ok(Inst {
            id,
            params,
            constraints,
            methods,
        })
    }
}

impl Lower for ast::MemberImplKind {
    type Target = core::Def;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::MemberImplKind;
        match &self {
            MemberImplKind::AssocTy(..) => todo!(),
            MemberImplKind::Method(method) => method.lower(ctx),
        }
    }
}

impl Lower for ast::MethodImpl {
    type Target = core::Def;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Bind, Def, Expr, Ty, TyE};
        let id = unwrap_resolved_ident(&self.name)?.var_id();
        Ok(if self.params.is_empty() {
            // variable
            let expr = self.expr.lower(ctx)?;
            let ty = TyE::infer();
            Def::new(id, ty, expr)
        } else {
            // function
            let expr = lower_lambda(ctx, &self.params, &self.expr)?;
            let ty = TyE::infer();
            Def::new(id, ty, expr)
        })
    }
}

impl Lower for ast::VarDecl {
    type Target = Option<core::Def>;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Def;
        let id = self.name.id.unwrap().decl_id();
        let var_id = ctx.ast.decl_var_ids[&id];
        let name = self.name.raw;
        if !ctx.core.builtins.contains(&name) {
            return Ok(None);
        }

        let ty = self.ty.lower(ctx)?;
        Ok(Some(Def::new_builtin(var_id, ty)))
    }
}

impl Lower for ast::TyParams {
    type Target = (Vec<PolyVarId>, Vec<core::Constraint>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Id;
        let cs = self.constraints.lower(ctx)?;
        let mut ps = vec![];
        for p in &self.params {
            let id = unwrap_resolved_ident(&p)?.poly_var_id();
            ps.push(id);
        }
        Ok((ps, cs))
    }
}

impl Lower for ast::TyArgs {
    type Target = (Vec<core::TyE>, Vec<core::Constraint>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Id;
        let gs = self.args.lower(ctx)?;
        let cs = self.constraints.lower(ctx)?;
        Ok((gs, cs))
    }
}

impl Lower for ast::Constraint {
    type Target = core::Constraint;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::ConstraintKind;
        use core::{Constraint, Id};
        Ok(match &self.kind {
            ConstraintKind::Class(n, ts) => {
                let id = unwrap_resolved_ident(n)?.class_id();
                let ts = ts.lower(ctx)?;
                Constraint::Class(id, ts)
            }
        })
    }
}

impl Lower for ast::Expr {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::{ExprKind, UnOp};
        use core::{Bind, Expr, Id, Ty};
        let e = match &self.kind {
            ExprKind::Apply(a, b) => {
                let a = a.lower(ctx)?;
                let b = b.lower(ctx)?;
                Expr::Apply(a.into(), b.into())
            }
            ExprKind::Binary(op, a, b) => {
                let op = Expr::Sym(ustr(op.as_str()));
                let a = a.lower(ctx)?;
                let b = b.lower(ctx)?;
                Expr::Apply(Expr::Apply(op.into(), a.into()).into(), b.into())
            }
            ExprKind::Unary(op, a) => {
                let a = a.lower(ctx)?;
                if matches!(op, UnOp::Effect) {
                    // handle default
                    Expr::Handle(a.into(), None)
                } else {
                    let op = Expr::Sym(ustr(op.as_str()));
                    Expr::Apply(op.into(), a.into())
                }
            }
            ExprKind::Case(c) => c.lower(ctx)?,
            ExprKind::Handle(h) => h.lower(ctx)?,
            ExprKind::Do(b) => b.lower(ctx)?,
            ExprKind::If(i) => i.lower(ctx)?,
            ExprKind::Func(f) => f.lower(ctx)?,
            ExprKind::Lambda(l) => l.lower(ctx)?,
            ExprKind::Var(v) => v.lower(ctx)?,
            ExprKind::List(es) => {
                let list_id = expect_data(ctx, "List")?;
                let nil_id = expect_var(ctx, "Nil")?;
                let cons_id = expect_var(ctx, "Cons")?;

                // ((<cons> e0) ((<cons> e1) <nil>))
                let mut expr = Expr::Var(nil_id);
                for e in es.lower(ctx)?.into_iter().rev() {
                    expr = make_cons_expr(cons_id, e, expr);
                }
                expr
            }
            ExprKind::Tuple(es) => {
                let ty_name = format!("Tuple{}", es.len());
                let tuple_id = expect_var(ctx, &ty_name)?;

                // (((<tuple> e0) e1) e2)
                let mut expr = Expr::Var(tuple_id);
                for e in es.lower(ctx)?.into_iter().rev() {
                    expr = Expr::Apply(expr.into(), e.into());
                }
                expr
            }
            ExprKind::Record(fields) => {
                let mut rec = BTreeMap::new();
                for (n, e) in fields {
                    rec.insert(n.raw, e.lower(ctx)?);
                }
                let expr = Expr::Record(rec);
                expr
            }
            ExprKind::Lit(l) => l.lower(ctx)?,
            ExprKind::Ident(n) => {
                let expr = match n.id {
                    Some(Id::Var(id)) => Ok(Expr::Var(id)),
                    Some(id) => panic!("unexpected id: {:?}", id),
                    None => Ok(Expr::Sym(n.raw)),
                }?;
                expr
            }
        };

        Ok(Expr::Span(self.span, e.into()))
    }
}

impl Lower for ast::Pat {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::PatKind;
        use core::{DataId, Expr, Id, Lit};
        let e = match &self.kind {
            PatKind::Unit => Expr::Lit(Lit::Unit),
            PatKind::Wildcard => {
                // ignored temporary variable
                let v = ctx.core.ids.next_var_id();
                let name = format!("w_{:06x}", self.span.hash());
                ctx.core.register_id_name(v, ustr(&name), Span::default());
                Expr::Var(v)
            }
            PatKind::DataCon(n, ps) => {
                let con_id = unwrap_resolved_ident(n)?.data_con_id();
                let var_id = ctx.ast.con_var_ids[&con_id];

                // (((<con> p0) p1) p2)
                let mut expr = Expr::Var(var_id);
                for p in ps.lower(ctx)?.into_iter() {
                    expr = Expr::Apply(expr.into(), p.into());
                }
                expr
            }
            PatKind::Tuple(ps) => {
                let ty_name = format!("Tuple{}", ps.len());
                let tuple_id = expect_var(ctx, &ty_name)?;

                // (((<tuple> p0) p1) p2)
                let mut expr = Expr::Var(tuple_id);
                for p in ps.lower(ctx)?.into_iter().rev() {
                    expr = Expr::Apply(expr.into(), p.into());
                }
                expr
            }
            PatKind::List(ps) => {
                let nil_id = expect_var(ctx, "Nil")?;
                let cons_id = expect_var(ctx, "Cons")?;

                // ((<cons> p0) ((<cons> p1) <nil>))
                let mut expr = Expr::Var(nil_id);
                for p in ps.lower(ctx)?.into_iter().rev() {
                    expr = make_cons_expr(cons_id, p, expr);
                }
                expr
            }
            PatKind::Record(fs) => {
                let mut rec = BTreeMap::new();
                for (n, p) in fs {
                    rec.insert(n.raw, p.lower(ctx)?);
                }
                Expr::Record(rec)
            }
            PatKind::Cons(x, xs) => {
                let cons_id = expect_var(ctx, "Cons")?;
                let x = x.lower(ctx)?;
                let xs = xs.lower(ctx)?;
                make_cons_expr(cons_id, x, xs)
            }
            PatKind::Lit(l) => l.lower(ctx)?,
            PatKind::Ident(n) => {
                let id = unwrap_resolved_ident(n)?.var_id();
                Expr::Var(id)
            }
        };
        Ok(Expr::Span(self.span, e.into()))
    }
}

impl Lower for ast::Case {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Expr;
        let e = self.expr.lower(ctx)?;
        let bs = self.alts.lower(ctx)?;
        Ok(Expr::Case(e.into(), bs))
    }
}

impl Lower for ast::CaseAlt {
    type Target = core::Alt;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Alt;
        let pat = self.pat.lower(ctx)?;
        let expr = self.expr.lower(ctx)?;
        Ok(Alt { pat, expr })
    }
}

impl Lower for ast::Handle {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Expr, TyE};
        let e = self.expr.lower(ctx)?;
        let hs = self.alts.lower(ctx)?;
        Ok(Expr::Handle(e.into(), Some(hs)))
    }
}

impl Lower for ast::HandleAlt {
    type Target = core::EfAlt;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::EfAlt;
        let ef = self.ef.lower(ctx)?;
        let expr = self.expr.lower(ctx)?;
        Ok(EfAlt { ef, expr })
    }
}

impl Lower for ast::Do {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Expr;
        let exprs = self.exprs.lower(ctx)?;
        Ok(Expr::Do(exprs))
    }
}

impl Lower for ast::If {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Alt, Expr, Lit};
        let c = self.cond.lower(ctx)?;
        let t = self.then.lower(ctx)?;
        let f = self.else_.lower(ctx)?;
        let expr = Expr::Case(
            c.into(),
            vec![
                Alt {
                    pat: Expr::Lit(Lit::Bool(true)),
                    expr: t,
                },
                Alt {
                    pat: Expr::Lit(Lit::Bool(false)),
                    expr: f,
                },
            ],
        );
        Ok(expr)
    }
}

impl Lower for ast::Func {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Bind, Expr};
        let id = unwrap_resolved_ident(&self.name)?.var_id();
        let body = lower_lambda(ctx, &self.params, &self.body)?;
        Ok(Expr::Let(Bind::Rec(id, body.into()), None))
    }
}

impl Lower for ast::Lambda {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        lower_lambda(ctx, &self.params, &self.body)
    }
}

impl Lower for ast::Var {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Bind, Expr};
        let pat = self.pat.lower(ctx)?;
        let e = self.expr.lower(ctx)?;
        let b = Bind::NonRec(pat.into(), e.into());
        Ok(Expr::Let(b, None))
    }
}

impl Lower for ast::Lit {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::LitKind;
        use core::{Expr, Lit, Ty};

        match &self.kind {
            LitKind::String(s) => lower_string_lit(ctx, s),
            k => {
                let l = lower_non_string_lit(ctx, k)?;
                Ok(Expr::Lit(l))
            }
        }
    }
}

impl Lower for ast::Ident {
    type Target = Id;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        unwrap_resolved_ident(self)
    }
}

pub fn make_cons_expr(cons_id: VarId, a: core::Expr, b: core::Expr) -> core::Expr {
    use core::Expr;
    Expr::Apply(
        Expr::Apply(Expr::Var(cons_id).into(), a.into()).into(),
        b.into(),
    )
}

pub fn make_ty_func(
    ctx: &mut Context,
    params: impl IntoIterator<Item = core::TyE>,
    ret_t: core::TyE,
) -> core::TyE {
    use ast::TyKind;
    use core::{Ty, TyE};
    let params = params.into_iter().collect::<Vec<_>>();
    if params.is_empty() {
        TyE::pure_func(TyE::pure(Ty::Unit), ret_t)
    } else {
        let mut t = ret_t;
        for p in params.into_iter().rev() {
            t = Ty::Func(p.into(), t.into()).into();
        }
        t
    }
}

pub fn lower_lambda(
    ctx: &mut Context,
    params: &[Box<ast::Pat>],
    body: &ast::Expr,
) -> diag::Result<core::Expr> {
    use ast::PatKind;
    use core::{Bind, Expr};

    let mut ps = vec![];
    for p in params.iter().rev() {
        ps.push(p.lower(ctx)?);
    }

    let mut expr = body.lower(ctx)?;
    for p in &ps {
        expr = Expr::Lambda(p.clone().into(), expr.into());
    }
    Ok(expr)
}

pub fn lower_non_string_lit(ctx: &mut Context, kind: &ast::LitKind) -> diag::Result<core::Lit> {
    use ast::LitKind;
    use core::Lit;
    match &kind {
        LitKind::Unit => Ok(Lit::Unit),
        LitKind::Bool(b) => Ok(Lit::Bool(*b)),
        LitKind::Int(i) => Ok(Lit::Int(*i)),
        LitKind::Float(f) => Ok(Lit::Float(f.to_bits())),
        LitKind::Char(c) => Ok(Lit::Char(*c)),
        LitKind::String(s) => panic!("unexpected string literal: {:?}", s),
    }
}

pub fn lower_string_lit(ctx: &mut Context, s: impl AsRef<str>) -> diag::Result<core::Expr> {
    use core::{Expr, Lit};
    let nil_id = expect_var(ctx, "Nil")?;
    let cons_id = expect_var(ctx, "Cons")?;

    let mut expr = Expr::Var(nil_id);
    for p in s.as_ref().chars().rev() {
        let c = Expr::Lit(Lit::Char(p));
        let e = Expr::Apply(Expr::Var(cons_id).into(), c.into());
        expr = Expr::Apply(e.into(), expr.into());
    }
    Ok(expr)
}

fn primitive(t: &str) -> core::Ty {
    core::Ty::Symbol(ustr(t))
}

fn expect_var(ctx: &Context, name: &str) -> diag::Result<VarId> {
    expect_id_defined(&ctx.core.global_names, "variable", name, |id| match id {
        Id::DataCon(id) => Some(ctx.ast.con_var_ids[&id]),
        Id::Var(id) => Some(*id),
        Id::Decl(id) => ctx.ast.decl_var_ids.get(&id).copied(),
        _ => None,
    })
}

fn expect_data(ctx: &Context, name: &str) -> diag::Result<DataId> {
    expect_id_defined(&ctx.core.global_types, "data", name, Id::as_data)
}

fn expect_data_con(ctx: &Context, name: &str) -> diag::Result<DataConId> {
    expect_id_defined(&ctx.core.global_names, "constructor", name, Id::as_data_con)
}

fn expect_id_defined<T: Into<Id>>(
    map: &UstrMap<Id>,
    kind: &str,
    name: &str,
    f: impl Fn(&Id) -> Option<T>,
) -> diag::Result<T> {
    let name = ustr(name);
    map.get(&name).and_then(|id| f(id)).ok_or(
        format!(
            "expected {} `{}` to be defined, but it was not found in the context",
            kind, name
        )
        .into_diagnostic(),
    )
}

fn ids_to_tys<T: Into<Id>>(ids: impl IntoIterator<Item = T>) -> Vec<core::TyE> {
    ids.into_iter()
        .map(|id| match Id::from(id.into()) {
            Id::MonoVar(x) => core::Ty::Mono(x).into(),
            Id::PolyVar(x) => core::Ty::Poly(x).into(),
            id @ _ => panic!("invalid id type: {:?}", id),
        })
        .collect()
}

fn unwrap_resolved_ident(ident: &ast::Ident) -> diag::Result<core::Id> {
    match ident.id {
        Some(id) => Ok(id),
        None => error_for(ident, format!("unresolved identifier: {}", ident.raw)).into_err(),
    }
}