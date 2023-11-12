use super::Context;
use term_ast as ast;
use term_ast::visit::{Visit, Visitor};
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_print as print;

use ast::{Either, Left, Right, Spanned};
use core::{DataConId, DataId, EffectOpId, Id, PolyVarId, Span, TyE, VarId};
use diag::{error_for, Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};

use std::f32::consts::E;

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use ustr::{ustr, Ustr, UstrMap};

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

impl<T: Lower<Target = core::Expr>> Lower for ast::Node<T> {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Expr;
        Ok(Expr::Span(self.span, self.kind.lower(ctx)?.into()))
    }
}

impl Lower for ast::Ty {
    type Target = core::TyE;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        // Lowering ast::Ty is a fairly straightforward conversion into core::TyE.
        // The primitive types are mapped to symbols, strings, lists, and tuple
        // expressions are desuggared into full constructor calls.
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
                Ty::func(a, b).into()
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
        // Lowering ast::Ef is a direct 1:1 conversion into core::Ef.
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

//
//

impl Lower for ast::ClassDecl {
    type Target = core::Class;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        // Lowering ast::ClassDecl is a straightforward conversion into core::Class.
        // The members are lowered and collected into a map from name to core::TyE.
        use ast::MemberDeclKind;
        use core::{Class, Def, Expr};
        let id = unwrap_resolved_ident(&self.name)?.class_id();
        let (_, cs) = self.ty_params.lower(ctx)?;

        let mut entries = vec![];
        for member in &self.members {
            match &member.kind {
                MemberDeclKind::AssocTy(_) => todo!(),
                MemberDeclKind::Method(method) => {
                    let (name, ty) = method.lower(ctx)?;
                    entries.push((name, ty.with_cs(cs.clone())));
                }
            }
        }

        let decls = BTreeMap::from_iter(entries);
        let class = Class::new(id, cs, decls);
        Ok(class)
    }
}

impl Lower for ast::MethodDecl {
    type Target = (Ustr, core::TyE);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        let name = self.name.raw;
        let ty = self.ty.lower(ctx)?;
        Ok((name, ty))
    }
}

impl Lower for ast::ClassInst {
    type Target = (BTreeMap<Ustr, VarId>, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::MemberImplKind;
        use core::{Constraint, Def, Expr};
        let inst_id = self.inst_id.unwrap();
        let class_id = self.class.id.unwrap().class_id();

        let (ts, mut cs) = self.ty_args.lower(ctx)?;
        cs.insert(0, Constraint::Class(class_id, ts));

        let mut defs = vec![];
        let mut entries = vec![];
        for member in &self.members {
            match &member.kind {
                MemberImplKind::AssocTy(..) => todo!(),
                MemberImplKind::Method(method) => {
                    let (name, def) = method.lower(ctx)?;
                    entries.push((name, def.id));
                    defs.push(def);
                }
            }
        }

        let members = BTreeMap::from_iter(entries);
        Ok((members, defs))
    }
}

impl Lower for ast::MethodImpl {
    type Target = (Ustr, core::Def);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Expr, Ty};
        let id = unwrap_resolved_ident(&self.name)?.var_id();
        let name = self.name.raw;
        Ok(if self.params.is_empty() {
            // variable
            let expr = self.expr.lower(ctx)?;
            let ty = TyE::infer();
            (name, Def::new(id, ty, expr))
        } else {
            // function
            let expr = Expr::lambda_n(self.params.lower(ctx)?, self.expr.lower(ctx)?);
            let ty = TyE::infer();
            (name, Def::new(id, ty, expr))
        })
    }
}

impl Lower for ast::DataDecl {
    type Target = Vec<core::Def>;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        // Lowering ast::DataDecl involves converting all data constructors to
        use core::{Def, Expr, Ty};
        let id = unwrap_resolved_ident(&self.name)?.data_id();
        let span = ctx.core.id_as_span(id).unwrap();
        let (ps, cs) = self.ty_params.lower(ctx)?;

        let data_ty = TyE::pure(Ty::Data(id, ids_to_tys(ps))).with_cs(cs);

        // create the constructor built-in function definitions
        let mut defs = vec![];
        for con in &self.cons {
            let id = ctx.ast.id_var_ids[&con.name.id.unwrap()];
            let fields = con.fields.lower(ctx)?;

            let ty = if con.fields.is_empty() {
                data_ty.clone()
            } else {
                make_ty_func(ctx, fields, data_ty.clone())
            };
            defs.push(Def::new_builtin(id, ty));
        }

        Ok(defs)
    }
}

impl Lower for ast::EffectDecl {
    type Target = (core::Effect, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        // Lowering ast::EffectDecl requires both the conversion into core::Effect,
        // and the declaration of special built-in 'effectful' versions of each of
        // the declared effect operations. These built-in functions are essentially
        // constructors for the effect as they all produce the
        use core::{Def, Ef, Effect, Expr, Ty};
        let id = self.name.id.unwrap().effect_id();
        let var_id = ctx.ast.id_var_ids[&id.into()];
        let (params, constraints) = self.ty_params.lower(ctx)?;
        let side_efs = self.side_efs.lower(ctx)?;
        let param_tys = ids_to_tys(params.clone());

        let effect = Ef::Effect(id, param_tys.clone());
        let mut defs = vec![];
        let mut entries = vec![];
        for (id, name, ty) in self.ops.lower(ctx)? {
            let (t, f) = ty.split_ef();
            let f = f | Ef::union(side_efs.clone());
            let def = Def::new_builtin(id, t.clone().with_ef(f.clone() | effect.clone()));
            let t = t.with_ef(f);

            entries.push((name, t));
            defs.push(def);
        }

        let ops = BTreeMap::from_iter(entries.clone());
        let handler_ty = TyE::record(entries);
        let effect = Effect::new(id, side_efs, ops, handler_ty);
        Ok((effect, defs))
    }
}

impl Lower for ast::EffectOpDecl {
    type Target = (VarId, Ustr, core::TyE);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Def;
        let var_id = ctx.ast.id_var_ids[&self.name.id.unwrap()];
        let name = self.name.raw;
        let ty = self.ty.lower(ctx)?;
        Ok((var_id, name, ty))
    }
}

impl Lower for ast::EffectHandler {
    type Target = (BTreeMap<Ustr, VarId>, Vec<core::Def>);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Expr};

        let mut defs = vec![];
        let mut entries = vec![];
        for op in &self.ops {
            let (name, def) = op.lower(ctx)?;
            entries.push((name, def.id));
            defs.push(def);
        }

        let ops = BTreeMap::from_iter(entries);
        Ok((ops, defs))
    }
}

impl Lower for ast::EffectOpImpl {
    type Target = (Ustr, core::Def);

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Def, Expr, TyE};
        let var_id = unwrap_resolved_ident(&self.name)?.var_id();
        let body = if self.params.is_empty() {
            self.expr.lower(ctx)? // variable
        } else {
            let ps = self.params.lower(ctx)?;
            let b = self.expr.lower(ctx)?;
            Expr::lambda_n(ps, b)
        };
        let def = Def::new(var_id, TyE::infer(), body);
        Ok((self.name.raw, def))
    }
}

impl Lower for ast::Decl {
    type Target = Option<core::Def>;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Def;
        let var_id = ctx.ast.id_var_ids[&self.name.id.unwrap()];
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

impl Lower for ast::ExprKind {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::{ExprKind, UnOp};
        use core::{Bind, Expr, Id, Ty};
        let e = match &self {
            ExprKind::Apply(box a, box b) => {
                let mut f = a;
                let mut es = vec![b.lower(ctx)?];
                while let ExprKind::Apply(box a1, box b1) = &f.kind {
                    es.push(b1.lower(ctx)?);
                    f = a1;
                }
                es.reverse();

                let f = f.lower(ctx)?;
                Expr::apply_n(f, es)
            }
            ExprKind::Binary(op, a, b) => {
                let op = Expr::Sym(ustr(op.as_str()));
                let a = a.lower(ctx)?;
                let b = b.lower(ctx)?;
                Expr::apply_n(op, vec![a, b])
            }
            ExprKind::Unary(op, a) => {
                let a = a.lower(ctx)?;
                if matches!(op, UnOp::Effect) {
                    Expr::Handle(a.into(), None)
                } else {
                    let op = Expr::Sym(ustr(op.as_str()));
                    Expr::apply(op, a)
                }
            }
            ExprKind::Case(c) => c.lower(ctx)?,
            ExprKind::Handle(h) => h.lower(ctx)?,
            ExprKind::Do(b) => b.lower(ctx)?,
            ExprKind::If(i) => i.lower(ctx)?,
            ExprKind::Func(f) => match f {
                Left(f) => f.lower(ctx)?,
                Right(id) => {
                    let func = ctx.ast.funcs.get(id).cloned().unwrap();
                    let func = func.borrow();
                    func.lower(ctx)?
                }
            },
            ExprKind::Lambda(l) => l.lower(ctx)?,
            ExprKind::Var(v) => v.lower(ctx)?,
            ExprKind::List(es) => {
                let nil = Expr::Var(expect_var(ctx, "Nil")?);
                let cons = Expr::Var(expect_var(ctx, "Cons")?);

                let mut expr = nil;
                for e in es.lower(ctx)?.into_iter().rev() {
                    expr = Expr::apply(Expr::apply(cons.clone(), e), expr);
                }
                expr
            }
            ExprKind::Tuple(es) => {
                let ty_name = format!("Tuple{}", es.len());
                let tuple_id = expect_var(ctx, &ty_name)?;

                let f = Expr::Var(tuple_id);
                let es = es.lower(ctx)?;
                Expr::apply_n(Expr::Var(tuple_id), es)
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

        Ok(e.into())
    }
}

impl Lower for ast::PatKind {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use ast::PatKind;
        use core::{DataId, Expr, Id, Lit};
        let e = match &self {
            PatKind::Unit => Expr::Lit(Lit::Unit),
            PatKind::Wildcard => {
                // ignored temporary variable
                let v = ctx.core.ids.next_var_id();
                let name = format!("w_{:06x}", v.raw);
                ctx.core.register_id_name(v, ustr(&name), Span::default());
                Expr::Var(v)
            }
            PatKind::DataCon(n, ps) => {
                let con_id = unwrap_resolved_ident(n)?.data_con_id();
                let var = Expr::Var(ctx.ast.id_var_ids[&con_id.into()]);
                Expr::apply_n(var, ps.lower(ctx)?)
            }
            PatKind::Tuple(ps) => {
                let ty_name = format!("Tuple{}", ps.len());
                let tuple_n = Expr::Var(expect_var(ctx, &ty_name)?);
                Expr::apply_n(tuple_n, ps.lower(ctx)?)
            }
            PatKind::List(ps) => {
                let nil = Expr::Var(expect_var(ctx, "Nil")?);
                let cons = Expr::Var(expect_var(ctx, "Cons")?);

                let mut expr = nil;
                for p in ps.lower(ctx)?.into_iter().rev() {
                    expr = Expr::apply_n(cons.clone(), vec![p, expr]);
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
                let cons = Expr::Var(expect_var(ctx, "Cons")?);
                let x = x.lower(ctx)?;
                let xs = xs.lower(ctx)?;
                Expr::apply_n(cons, vec![x, xs])
            }
            PatKind::Lit(l) => l.lower(ctx)?,
            PatKind::Ident(n) => {
                let id = unwrap_resolved_ident(n)?.var_id();
                Expr::Var(id)
            }
        };
        Ok(e.into())
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
        Ok((pat, expr))
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
        Ok((ef, expr))
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
                (Expr::Lit(Lit::Bool(true)), t),
                (Expr::Lit(Lit::Bool(false)), f),
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
        let body = Expr::lambda_n(self.params.lower(ctx)?, self.body.lower(ctx)?);
        Ok(Expr::Let(vec![Bind::Rec(id, body.into())], None))
    }
}

impl Lower for ast::Lambda {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::Expr;
        let ps = self.params.lower(ctx)?;
        let body = self.body.lower(ctx)?;
        Ok(Expr::lambda_n(ps, body))
    }
}

impl Lower for ast::Var {
    type Target = core::Expr;

    fn lower(&self, ctx: &mut Context) -> diag::Result<Self::Target> {
        use core::{Bind, Expr};
        let pat = self.pat.lower(ctx)?;
        let e = self.expr.lower(ctx)?;
        let b = Bind::NonRec(pat.into(), e.into());
        Ok(Expr::Let(vec![b], None))
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

pub fn make_ty_func(
    ctx: &mut Context,
    params: impl IntoIterator<Item = core::TyE>,
    ret_t: core::TyE,
) -> core::TyE {
    use ast::TyKind;
    use core::{Ty, TyE};
    let params = params.into_iter().collect::<Vec<_>>();
    if params.is_empty() {
        TyE::func(TyE::pure(Ty::Unit), ret_t)
    } else {
        let mut t = ret_t;
        for p in params.into_iter().rev() {
            t = Ty::Func(p.into(), t.into()).into();
        }
        t
    }
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
    let nil = Expr::Var(expect_var(ctx, "Nil")?);
    let cons = Expr::Var(expect_var(ctx, "Cons")?);

    let mut expr = nil;
    for c in s.as_ref().chars().rev().map(|c| Lit::Char(c)) {
        expr = Expr::apply_n(cons.clone(), vec![Expr::Lit(c), expr]);
    }
    Ok(expr)
}

fn primitive(t: &str) -> core::Ty {
    core::Ty::Sym(ustr(t))
}

fn expect_var(ctx: &Context, name: &str) -> diag::Result<VarId> {
    expect_id_defined(&ctx.core.global_names, "variable", name, |id| match id {
        &Id::DataCon(id) => Some(ctx.ast.id_var_ids[&id.into()]),
        &Id::Decl(id) => Some(ctx.ast.id_var_ids[&id.into()]),
        &Id::Var(id) => Some(id),
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
