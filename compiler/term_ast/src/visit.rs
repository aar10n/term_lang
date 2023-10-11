use crate::{ast::*, Context};

use std::{
    borrow::BorrowMut,
    cell::RefCell,
    ops::{Deref, DerefMut},
    thread::scope,
};

macro_rules! scoped {
    ($visitor:expr, $($body:tt)*) => {{
        $visitor.push_scope();
        let result = { $($body)* };
        $visitor.pop_scope();
        result
    }};
}

/// A visitor for AST nodes.
pub trait Visitor<'a, S: Default, E>: Sized {
    fn context(&mut self) -> &mut Context<'a>;
    fn push_scope(&mut self) {}
    fn pop_scope(&mut self) {}

    fn visit_module(&mut self, module: &mut Module) -> Result<S, E> {
        module.walk(self)
    }
    fn visit_item(&mut self, item: &mut Item) -> Result<S, E> {
        item.walk(self)
    }
    fn visit_ty(&mut self, ty: &mut Ty) -> Result<S, E> {
        ty.walk(self)
    }
    fn visit_ef(&mut self, ef: &mut Ef) -> Result<S, E> {
        ef.walk(self)
    }
    fn visit_data_decl(&mut self, data: &mut DataDecl) -> Result<S, E> {
        data.walk(self)
    }
    fn visit_data_con(&mut self, con: &mut DataConDecl) -> Result<S, E> {
        con.walk(self)
    }
    fn visit_effect_decl(&mut self, effect: &mut EffectDecl) -> Result<S, E> {
        effect.walk(self)
    }
    fn visit_effect_op_decl(&mut self, op: &mut EffectOpDecl) -> Result<S, E> {
        op.walk(self)
    }
    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> Result<S, E> {
        handler.walk(self)
    }
    fn visit_effect_op_impl(&mut self, op: &mut EffectOpImpl) -> Result<S, E> {
        op.walk(self)
    }
    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> Result<S, E> {
        class.walk(self)
    }
    fn visit_member_decl(&mut self, member: &mut MemberDecl) -> Result<S, E> {
        member.walk(self)
    }
    fn visit_method_decl(&mut self, method: &mut MethodDecl) -> Result<S, E> {
        method.walk(self)
    }
    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> Result<S, E> {
        inst.walk(self)
    }
    fn visit_member_impl(&mut self, member: &mut MemberImpl) -> Result<S, E> {
        member.walk(self)
    }
    fn visit_method_impl(&mut self, method: &mut MethodImpl) -> Result<S, E> {
        method.walk(self)
    }
    fn visit_var_decl(&mut self, var: &mut VarDecl) -> Result<S, E> {
        var.walk(self)
    }
    fn visit_ty_params(&mut self, ty_params: &mut TyParams) -> Result<S, E> {
        ty_params.walk(self)
    }
    fn visit_ty_param(&mut self, param: &mut Ident) -> Result<S, E> {
        self.visit_ty_ident(param)
    }
    fn visit_ty_args(&mut self, ty_args: &mut TyArgs) -> Result<S, E> {
        ty_args.walk(self)
    }
    fn visit_constraint(&mut self, constraint: &mut Constraint) -> Result<S, E> {
        constraint.walk(self)
    }
    fn visit_expr(&mut self, expr: &mut Expr) -> Result<S, E> {
        expr.walk(self)
    }
    fn visit_pat(&mut self, pat: &mut Pat) -> Result<S, E> {
        pat.walk(self)
    }
    fn visit_case(&mut self, case_expr: &mut Case) -> Result<S, E> {
        case_expr.walk(self)
    }
    fn visit_case_alt(&mut self, alt: &mut CaseAlt) -> Result<S, E> {
        alt.walk(self)
    }
    fn visit_handle(&mut self, handle_expr: &mut Handle) -> Result<S, E> {
        handle_expr.walk(self)
    }
    fn visit_handle_alt(&mut self, alt: &mut HandleAlt) -> Result<S, E> {
        alt.walk(self)
    }
    fn visit_do(&mut self, do_expr: &mut Do) -> Result<S, E> {
        do_expr.walk(self)
    }
    fn visit_if(&mut self, if_expr: &mut If) -> Result<S, E> {
        if_expr.walk(self)
    }
    fn visit_func(
        &mut self,
        ident: &mut Ident,
        params: &mut Vec<P<Pat>>,
        body: &mut P<Expr>,
    ) -> Result<S, E> {
        self.visit_var_ident(ident)?;
        scoped! {self,
            params.visit(self)?;
            body.visit(self)
        }
    }
    fn visit_var(&mut self, bind: &mut P<Pat>, body: &mut P<Expr>) -> Result<S, E> {
        bind.visit(self)?;
        body.visit(self)
    }
    fn visit_lambda(&mut self, params: &mut Vec<P<Pat>>, body: &mut P<Expr>) -> Result<S, E> {
        scoped! {self,
            params.visit(self)?;
            body.visit(self)
        }
    }
    fn visit_list(&mut self, items: &mut Vec<P<Expr>>) -> Result<S, E> {
        items.visit(self)
    }
    fn visit_tuple(&mut self, items: &mut Vec<P<Expr>>) -> Result<S, E> {
        items.visit(self)
    }
    fn visit_record(&mut self, fields: &mut Vec<(Ident, P<Expr>)>) -> Result<S, E> {
        fields.visit(self)
    }
    fn visit_record_field(&mut self, field: &mut (Ident, P<Expr>)) -> Result<S, E> {
        self.visit_var_ident(&mut field.0)?;
        field.1.visit(self)
    }
    fn visit_lit(&mut self, lit: &mut Lit) -> Result<S, E> {
        Ok(S::default())
    }

    fn visit_class_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_data_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_con_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_effect_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_handler_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_ty_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }
    fn visit_var_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        self.visit_ident(ident)
    }

    fn visit_ident(&mut self, ident: &mut Ident) -> Result<S, E> {
        Ok(S::default())
    }
}

/// A trait for visitable AST nodes.
pub trait Visit {
    /// Invokes the visitor method on this node.
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E>;
    /// Visits all of the children of this node.
    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E>;
}

impl<T: Visit> Visit for Vec<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in self {
            item.visit(visitor)?;
        }
        Ok(S::default())
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for item in self {
            item.walk(visitor)?;
        }
        Ok(S::default())
    }
}

impl<T: Visit> Visit for Option<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        if let Some(item) = self {
            item.visit(visitor);
        }
        Ok(S::default())
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        if let Some(item) = self {
            item.walk(visitor);
        }
        Ok(S::default())
    }
}

impl<T: Visit> Visit for Box<T> {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.deref_mut().visit(visitor)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.deref_mut().walk(visitor)
    }
}

//
//

impl Visit for Module {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_module(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.items.visit(visitor)
    }
}

impl Visit for Item {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_item(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            ItemKind::Command(_, _) => Ok(S::default()),
            ItemKind::DataDecl(data) => data.visit(visitor),
            ItemKind::EffectDecl(effect) => effect.visit(visitor),
            ItemKind::EffectHandler(handler) => handler.visit(visitor),
            ItemKind::ClassDecl(class) => class.visit(visitor),
            ItemKind::ClassInst(inst) => inst.visit(visitor),
            ItemKind::VarDecl(decl) => match decl {
                Left(decl) => decl.visit(visitor),
                Right(id) => {
                    let decl = visitor
                        .context()
                        .decls
                        .get(id)
                        .expect("invalid decl node id")
                        .to_owned();
                    let mut decl = RefCell::borrow_mut(&decl);
                    decl.visit(visitor)
                }
            },
            ItemKind::Expr(expr) => expr.visit(visitor),
        }
    }
}

impl Visit for Ty {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_ty(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            TyKind::Name(n) => visitor.visit_ty_ident(n),
            TyKind::Inst(n, ts) => {
                visitor.visit_ty_ident(n)?;
                ts.visit(visitor)
            }
            TyKind::Func(from, to) => {
                from.visit(visitor)?;
                to.visit(visitor)
            }
            TyKind::List(ty) => ty.visit(visitor),
            TyKind::Tuple(tys) => tys.visit(visitor),
            TyKind::Effect(ty, efs) => {
                ty.visit(visitor)?;
                efs.visit(visitor)
            }
            TyKind::Forall(ty_params, ty) => {
                scoped! {visitor,
                    ty_params.visit(visitor)?;
                    ty.visit(visitor)
                }
            }
            _ => Ok(S::default()),
        }
    }
}

impl Visit for Ef {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_ef(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            EfKind::Name(n, ts) => {
                visitor.visit_effect_ident(n)?;
                ts.visit(visitor)
            }
            EfKind::Union(fs) => fs.visit(visitor),
            _ => Ok(S::default()),
        }
    }
}

impl Visit for DataDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_data_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_data_ident(&mut self.name);
        scoped! {visitor,
            self.ty_params.visit(visitor)?;
            self.cons.visit(visitor)
        }
    }
}

impl Visit for DataConDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_data_con(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_con_ident(&mut self.name)?;
        scoped! {visitor,
            self.fields.visit(visitor)
        }
    }
}

impl Visit for EffectDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty_params.visit(visitor)?;
            self.ops.visit(visitor)
        }
    }
}

impl Visit for EffectOpDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_op_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty.visit(visitor)
        }
    }
}

impl Visit for EffectHandler {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_handler(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_ident(&mut self.effect)?;
        visitor.visit_handler_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty_args.visit(visitor)?;
            self.ops.visit(visitor)
        }
    }
}

impl Visit for EffectOpImpl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_effect_op_impl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.name);
        scoped! {visitor,
            self.params.visit(visitor)?;
            self.expr.visit(visitor)
        }
    }
}

impl Visit for ClassDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_class_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_class_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty_params.visit(visitor)?;
            self.members.visit(visitor)
        }
    }
}

impl Visit for MemberDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_member_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            MemberDeclKind::AssocTy(ident) => todo!(),
            MemberDeclKind::Method(method) => method.visit(visitor),
        }
    }
}

impl Visit for MethodDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_method_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty_params.visit(visitor)?;
            self.ty.visit(visitor)
        }
    }
}

impl Visit for ClassInst {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_class_inst(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_class_ident(&mut self.class)?;
        scoped! {visitor,
            self.ty_args.visit(visitor)?;
            self.members.visit(visitor)
        }
    }
}

impl Visit for MemberImpl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_member_impl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            MemberImplKind::AssocTy(..) => todo!(),
            MemberImplKind::Method(method) => method.visit(visitor),
        }
    }
}

impl Visit for MethodImpl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_method_impl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.name)?;
        scoped! {visitor,
            self.params.visit(visitor)?;
            self.expr.visit(visitor)
        }
    }
}

impl Visit for VarDecl {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_decl(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.name)?;
        scoped! {visitor,
            self.ty.visit(visitor)
        }
    }
}

impl Visit for TyParams {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_ty_params(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        // println!("visiting type params ({:?})", self.params);

        for param in &mut self.params {
            visitor.visit_ty_param(param)?;
        }
        self.constraints.visit(visitor)
    }
}

impl Visit for TyArgs {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_ty_args(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        for param in &mut self.params {
            visitor.visit_ty_param(param)?;
        }
        self.args.visit(visitor)?;
        self.constraints.visit(visitor)
    }
}

impl Visit for Constraint {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_constraint(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            ConstraintKind::Class(n, t) => {
                visitor.visit_class_ident(n)?;
                t.visit(visitor)
            }
        }
    }
}

impl Visit for Expr {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_expr(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            ExprKind::Apply(x, y) => {
                x.visit(visitor)?;
                y.visit(visitor)
            }
            ExprKind::Binary(_, x, y) => {
                x.visit(visitor)?;
                y.visit(visitor)
            }
            ExprKind::Unary(_, x) => x.visit(visitor),
            ExprKind::Case(case) => case.visit(visitor),
            ExprKind::Handle(handle) => handle.visit(visitor),
            ExprKind::Do(block) => block.visit(visitor),
            ExprKind::If(if_) => if_.visit(visitor),
            ExprKind::Func(ident, params, body) => visitor.visit_func(ident, params, body),
            ExprKind::Var(bind, expr) => visitor.visit_var(bind, expr),
            ExprKind::Lambda(pats, expr) => visitor.visit_lambda(pats, expr),
            ExprKind::List(exprs) => exprs.visit(visitor),
            ExprKind::Tuple(exprs) => exprs.visit(visitor),
            ExprKind::Record(fields) => fields.visit(visitor),
            ExprKind::Lit(lit) => visitor.visit_lit(lit),
            ExprKind::Ident(n) => visitor.visit_var_ident(n),
        }
    }
}

impl Visit for Pat {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_pat(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        match &mut self.kind {
            PatKind::DataCon(n, pats) => {
                visitor.visit_con_ident(n)?;
                pats.visit(visitor)
            }
            PatKind::Tuple(pats) => pats.visit(visitor),
            PatKind::List(pats) => pats.visit(visitor),
            PatKind::Cons(head, tail) => {
                head.visit(visitor)?;
                tail.visit(visitor)
            }
            PatKind::Lit(lit) => visitor.visit_lit(lit),
            PatKind::Ident(n) => visitor.visit_var_ident(n),
            _ => Ok(S::default()),
        }
    }
}

impl Visit for Case {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_case(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.expr.visit(visitor)?;
        self.alts.visit(visitor)
    }
}

impl Visit for CaseAlt {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_case_alt(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        scoped! {visitor,
            self.pat.visit(visitor)?;
            self.expr.visit(visitor)
        }
    }
}

impl Visit for Handle {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_handle(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.expr.visit(visitor)?;
        self.alts.visit(visitor)
    }
}

impl Visit for HandleAlt {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_handle_alt(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        scoped! {visitor,
            self.ef.visit(visitor)?;
            self.expr.visit(visitor)
        }
    }
}

impl Visit for Do {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_do(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        scoped! {visitor,
            for e in &mut self.exprs {
                e.visit(visitor)?;
            }
        }
        Ok(S::default())
    }
}

impl Visit for If {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_if(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        self.cond.visit(visitor)?;
        scoped!(visitor, self.then.visit(visitor)?);
        scoped!(visitor, self.else_.visit(visitor))
    }
}

impl Visit for (Ident, P<Expr>) {
    fn visit<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_record_field(self)
    }

    fn walk<'a, V: Visitor<'a, S, E>, S: Default, E>(&mut self, visitor: &mut V) -> Result<S, E> {
        visitor.visit_var_ident(&mut self.0)?;
        self.1.visit(visitor)
    }
}
