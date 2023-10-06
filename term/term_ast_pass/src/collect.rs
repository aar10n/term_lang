use crate::{Context, DuplicateDeclErr, PassResult};
use term_ast as ast;
use term_core as core;
use term_diag as diag;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::DeclId;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use term_print::{PrettyPrint, PrettyString};

use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{ustr, UstrMap};

struct CollectVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
    all_decls: UstrMap<(DeclId, Span)>,
    scope_id: Option<core::Id>,
    level: usize,
}

impl<'v, 'ast> CollectVisitor<'v, 'ast> {
    pub fn new(ctx: &'v mut Context<'ast>) -> Self {
        Self {
            ctx,
            all_decls: UstrMap::default(),
            scope_id: None,
            level: 0,
        }
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for CollectVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }
    fn push_scope(&mut self) {
        self.level += 1
    }
    fn pop_scope(&mut self) {
        self.level -= 1;
    }

    fn visit_item(&mut self, item: &mut Item) -> diag::Result<()> {
        item.walk(self)?;
        match &mut item.kind {
            ItemKind::VarDecl(decl) => {
                let id = match decl {
                    Left(decl) => decl.name.id.unwrap().decl_id(),
                    Right(_) => return Ok(()),
                };

                let decl = match std::mem::replace(decl, Right(id)) {
                    Left(decl) => Rc::new(RefCell::new(decl)),
                    Right(_) => unreachable!(),
                };
                self.ctx.decls.insert(id, decl);
            }
            _ => {}
        }
        Ok(())
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let id = self.ctx.ids.next_data_id();
        let name = data.name.raw;
        let span = data.name.span();

        if let Err(existing_id) = self.ctx.register_global_name(id, name, span) {
            return DuplicateDeclErr {
                kind: "data",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };

        self.scope_id = Some(id.into());
        data.name.id = Some(id.into());
        data.ty_params.visit(self)?;
        data.cons.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_data_con(&mut self, con: &mut DataConDecl) -> diag::Result<()> {
        let data_id = self.scope_id.unwrap().data_id();
        let id = self.ctx.ids.next_data_con_id(data_id);
        let name = con.name.raw;
        let span = con.name.span();

        if let Err(existing_id) = self.ctx.register_global_name(id, name, span) {
            return DuplicateDeclErr {
                kind: "data constructor",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };

        // register and associate variable id with this constructor. the variable
        // will become associated with a constructor function used in type checking.
        let var_id = self.ctx.ids.next_var_id();
        self.ctx.register_id_name(var_id, name, span);
        self.ctx.con_var_ids.insert(id, var_id);

        con.name.id = Some(id.into());
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect: &mut EffectDecl) -> diag::Result<()> {
        let id = self.ctx.ids.next_effect_id();
        let name = effect.name.raw;
        let span = effect.name.span();

        if let Err(existing_id) = self.ctx.register_global_name(id, name, span) {
            return DuplicateDeclErr {
                kind: "effect",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };

        self.scope_id = Some(id.into());
        effect.name.id = Some(id.into());
        effect.ty_params.visit(self)?;
        effect.ops.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_effect_op_decl(&mut self, op: &mut EffectOpDecl) -> diag::Result<()> {
        let effect_id = self.scope_id.unwrap().effect_id();
        let id = self.ctx.ids.next_decl_id();
        let name = op.name.raw;
        let span = op.name.span();

        if let Err(existing_id) = self.ctx.register_scoped_name(effect_id, id, name, span) {
            return DuplicateDeclErr {
                kind: "effect operation",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };
        op.name.id = Some(id.into());
        Ok(())
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let effect_id = handler.effect.id.unwrap().effect_id();
        let id = self.ctx.ids.next_handler_id();
        let name = handler.name.raw;
        let span = handler.name.span();

        if let Err(existing_id) = self.ctx.register_global_name(id, name, span) {
            return DuplicateDeclErr {
                kind: "effect handler",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };

        self.scope_id = Some(id.into());
        handler.name.id = Some(id.into());
        handler.ty_args.visit(self)?;
        handler.ops.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_effect_op_impl(&mut self, op: &mut EffectOpImpl) -> diag::Result<()> {
        let handler_id = self.scope_id.unwrap().handler_id();
        let id = self.ctx.ids.next_var_id();
        let name = op.name.raw;
        let span = op.name.span();

        if let Err(existing_id) = self.ctx.register_scoped_name(handler_id, id, name, span) {
            return DuplicateDeclErr {
                kind: "effect operation",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };
        op.name.id = Some(id.into());
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let id = self.ctx.ids.next_class_id();
        let name = class.name.raw;
        let span = class.name.span();

        if let Err(existing_id) = self.ctx.register_global_name(id, name, span) {
            return DuplicateDeclErr {
                kind: "class",
                span,
                first: self.ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err();
        };

        self.scope_id = Some(id.into());
        class.name.id = Some(id.into());
        class.ty_params.visit(self)?;
        class.members.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_method_decl(&mut self, method: &mut MethodDecl) -> diag::Result<()> {
        let class_id = self.scope_id.unwrap().class_id();
        let id = self.ctx.ids.next_decl_id();
        let name = method.name.raw;
        let span = method.name.span();

        match self.ctx.register_scoped_name(class_id, id, name, span) {
            Err(existing_id) => {
                return DuplicateDeclErr {
                    kind: "member",
                    span,
                    first: self.ctx.id_as_span(existing_id).unwrap(),
                }
                .into_err();
            }
            Ok(_) => {}
        };

        self.ctx.ambiguous_names.insert(name);
        self.all_decls.insert(name, (id, method.span()));

        method.name.id = Some(id.into());
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let id = self.ctx.ids.next_inst_id();
        let name = ustr(&inst.ty_args.args.plain_string(&self.ctx));
        let span = inst.span();
        self.ctx.register_id_name(id, name, span);

        self.scope_id = Some(id.into());
        inst.id = Some(id.into());
        inst.ty_args.visit(self)?;
        inst.members.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_method_impl(&mut self, method: &mut MethodImpl) -> Result<(), Diagnostic> {
        let inst_id = self.scope_id.unwrap().inst_id();
        let id = self.ctx.ids.next_var_id();
        let name = method.name.raw;
        let span = method.name.span();

        match self.ctx.register_scoped_name(inst_id, id, name, span) {
            Err(existing_id) => {
                return DuplicateDeclErr {
                    kind: "member",
                    span,
                    first: self.ctx.id_as_span(existing_id).unwrap(),
                }
                .into_err();
            }
            Ok(_) => {}
        };

        method.name.id = Some(id.into());
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut VarDecl) -> diag::Result<()> {
        if self.level > 0 {
            return Diagnostic::error(
                "variable declarations may only occur at the top level",
                var.span(),
            )
            .into_err();
        }

        let id = self.ctx.ids.next_decl_id();
        let name = var.name.raw;
        let span = var.name.span();
        if let Some((_, decl_span)) = self.all_decls.get(&name).copied() {
            return DuplicateDeclErr {
                kind: "variable",
                span,
                first: decl_span,
            }
            .into_err();
        }

        self.ctx.register_id_name(id, name, span);
        self.all_decls.insert(name, (id, var.span()));

        // this declaration is for a compiler builtin
        if self.ctx.builtins.contains(&name) {
            // create a variable id associated with the declaration to use in resolution
            let var_id = self.ctx.ids.next_var_id();
            self.ctx.register_global_name(var_id, name, span).unwrap();
            self.ctx.decl_var_ids.insert(id, var_id);
            self.ctx.var_decl_ids.insert(var_id, id);
        }

        var.name.id = Some(id.into());
        var.ty.visit(self)
    }

    fn visit_ty_param(&mut self, param: &mut Ident) -> diag::Result<()> {
        let id = self.ctx.ids.next_poly_var_id();
        let name = param.raw;
        let span = param.span();
        self.ctx.register_id_name(id, name, span);
        param.id = Some(id.into());
        Ok(())
    }

    //
    //

    fn visit_func(
        &mut self,
        ident: &mut Ident,
        params: &mut Vec<P<Pat>>,
        body: &mut P<Expr>,
    ) -> diag::Result<()> {
        let id = self.ctx.ids.next_var_id();
        let name = ident.raw;
        let span = ident.span();
        self.ctx.register_id_name(id, name, span);
        ident.id = Some(id.into());
        if self.level > 0 {
            // local function, we dont have to do anything else
            params.visit(self)?;
            return body.visit(self);
        }

        // check if there is a declaration
        if let Some((decl_id, span)) = self.all_decls.get(&name) {
            // check if declared variable is redefined
            if let Some(var_id) = self.ctx.decl_var_ids.insert(*decl_id, id) {
                return DuplicateDeclErr {
                    kind: "variable",
                    span: ident.span(),
                    first: self.ctx.id_as_span(var_id).unwrap(),
                }
                .into_err();
            }
            self.ctx.var_decl_ids.insert(id, *decl_id);
        }

        params.visit(self)?;
        body.visit(self)
    }

    fn visit_pat(&mut self, pat: &mut Pat) -> diag::Result<()> {
        use PatKind::*;
        match &mut pat.kind {
            DataCon(_, ps) => ps.visit(self),
            Tuple(ps) => ps.visit(self),
            List(p) => p.visit(self),
            Cons(x, xs) => {
                x.visit(self)?;
                xs.visit(self)
            }
            Ident(ident) => {
                let id = self.ctx.ids.next_var_id();
                let name = ident.raw;
                let span = ident.span();
                self.ctx.register_id_name(id, name, span);
                ident.id = Some(id.into());
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

pub fn collect<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut module = module;
    let mut results = vec![];
    let mut visitor = CollectVisitor::new(ctx);
    for item in &mut module.items {
        if let Err(e) = item.visit(&mut visitor) {
            results.push(e);
        }
    }

    if results.is_empty() {
        PassResult::Ok(vec![])
    } else {
        PassResult::Err(results)
    }
}
