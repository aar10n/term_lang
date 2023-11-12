use crate::{DuplicateDeclErr, PassResult};
use term_ast as ast;
use term_ast_lower as lower;
use term_common as common;
use term_core as core;
use term_diag as diag;

use ast::visit::{Visit, Visitor};
use ast::*;
use common::{id::Identifiable, RcRef};
use core::{ClassId, DeclId, EffectId, Exclusivity, Id};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use term_print::{PrettyPrint, PrettyString};

use ustr::{ustr, UstrMap};

/// Collects all declarations and registers them in the context.
struct CollectVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,

    all_decls: UstrMap<(DeclId, Span)>,
    scope_id: Option<core::Id>,
    level: usize,
}

impl<'ctx> CollectVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self {
            ast,
            core,

            all_decls: UstrMap::default(),
            scope_id: None,
            level: 0,
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for CollectVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }
    fn push_scope(&mut self) {
        self.level += 1
    }
    fn pop_scope(&mut self) {
        self.level -= 1;
    }

    fn visit_item(&mut self, item: &mut Item) -> diag::Result<()> {
        item.walk(self)?;

        // lift declaration and definition nodes out of the module and into the ast
        // context replacing them with an id reference
        match &mut item.kind {
            ItemKind::DataDecl(data) => {
                let data = swap_identifiable_with_id(data);
                self.ast.datas.insert(data.id(), RcRef::new(data));
            }
            ItemKind::ClassDecl(class) => {
                let class = swap_identifiable_with_id(class);
                self.ast.classes.insert(class.id(), RcRef::new(class));
            }
            ItemKind::ClassInst(inst) => {
                let inst = swap_identifiable_with_id(inst);
                self.ast.insts.insert(inst.id(), RcRef::new(inst));
            }
            ItemKind::EffectDecl(effect) => {
                let effect = swap_identifiable_with_id(effect);
                self.ast.effects.insert(effect.id(), RcRef::new(effect));
            }
            ItemKind::EffectHandler(handler) => {
                let handler = swap_identifiable_with_id(handler);
                self.ast.handlers.insert(handler.id(), RcRef::new(handler));
            }
            ItemKind::Decl(decl) => {
                let decl = swap_identifiable_with_id(decl);
                self.ast.decls.insert(decl.id(), RcRef::new(decl));
            }
            _ => {}
        }
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let id = self.core.ids.next_class_id();
        let name = class.name.raw;
        let span = class.name.span();

        self.core
            .register_global_type(id, name, span)
            .ok_or_duplicate_decl_err(&self.core, "class")?;

        self.scope_id = Some(id.into());
        class.name.id = Some(id.into());
        class.ty_params.visit(self)?;
        class.members.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_method_decl(&mut self, method: &mut MethodDecl) -> diag::Result<()> {
        let class_id = self.scope_id.unwrap().class_id();
        let id = self.core.ids.next_decl_id();
        let name = method.name.raw;
        let span = method.name.span();

        self.core
            .register_scoped_name(class_id, id, name, span, Exclusivity::Name)
            .ok_or_duplicate_decl_err(&self.core, "member")?;

        self.core.functions.entry(name).or_default();
        self.all_decls.insert(name, (id, method.span()));

        method.name.id = Some(id.into());
        method.ty_params.visit(self)?;
        method.ty.visit(self)
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let id = self.core.ids.next_inst_id();
        let name = ustr(&format!(
            "{}'{}",
            inst.class.raw,
            &inst.ty_args.args.plain_string(&self.ast)
        ));
        let span = inst.span();
        self.core.register_id_name(id, name, span);

        let var_id = self.core.ids.next_var_id();
        self.core.register_id_name(var_id, name, span);
        self.ast.id_var_ids.insert(id.into(), var_id);

        self.scope_id = Some(id.into());
        inst.inst_id = Some(id.into());
        inst.ty_args.visit(self)?;
        inst.members.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_method_impl(&mut self, method: &mut MethodImpl) -> diag::Result<()> {
        let inst_id = self.scope_id.unwrap().inst_id();
        let inst_var_id = self.ast.id_var_ids[&inst_id.into()];
        let id = self.core.ids.next_var_id();
        let name = method.name.raw;
        let span = method.name.span();

        self.core
            .register_scoped_name(inst_id, id, name, span, Exclusivity::None)
            .ok_or_duplicate_decl_err(&self.core, "member")?;

        self.core
            .functions
            .entry(name)
            .or_default()
            .push((id, inst_id));

        method.name.id = Some(id.into());
        method.params.visit(self)?;
        method.expr.visit(self)
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let id = self.core.ids.next_data_id();
        let name = data.name.raw;
        let span = data.name.span();

        self.core
            .register_global_type(id, name, span)
            .ok_or_duplicate_decl_err(&self.core, "data")?;

        self.scope_id = Some(id.into());
        data.name.id = Some(id.into());
        data.ty_params.visit(self)?;
        data.cons.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_data_con(&mut self, con: &mut DataConDecl) -> diag::Result<()> {
        let data_id = self.scope_id.unwrap().data_id();
        let id = self.core.ids.next_data_con_id(data_id);
        let name = con.name.raw;
        let span = con.name.span();

        self.core
            .register_global_name(id, name, span)
            .ok_or_duplicate_decl_err(&self.core, "data constructor")?;

        let var_id = self.core.ids.next_var_id();
        self.core.register_id_name(var_id, name, span);
        self.ast.id_var_ids.insert(id.into(), var_id);

        con.name.id = Some(id.into());
        con.fields.visit(self)
    }

    fn visit_effect_decl(&mut self, effect: &mut EffectDecl) -> diag::Result<()> {
        let id = self.core.ids.next_effect_id();
        let name = effect.name.raw;
        let span = effect.name.span();

        self.core
            .register_global_type(id, name, span)
            .ok_or_duplicate_decl_err(self.core, "effect")?;

        let var_id = self.core.ids.next_var_id();
        self.core.register_id_name(var_id, name, span);
        self.ast.id_var_ids.insert(id.into(), var_id);
        effect.name.id = Some(id.into());

        self.scope_id = Some(id.into());
        effect.ty_params.visit(self)?;
        effect.side_efs.visit(self)?;
        effect.ops.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_effect_op_decl(&mut self, op: &mut EffectOpDecl) -> diag::Result<()> {
        let effect_id = self.scope_id.unwrap().effect_id();
        let op_id = self.core.ids.next_effect_op_id(effect_id);
        let name = op.name.raw;
        let span = op.name.span();

        self.core
            .register_scoped_name(effect_id, op_id, name, span, Exclusivity::None)
            .ok_or_duplicate_decl_err(self.core, "effect operation")?;

        let var_id = self.core.ids.next_var_id();
        self.ast.id_var_ids.insert(op_id.into(), var_id);

        self.core
            .register_global_name(var_id, name, span)
            .ok_or_duplicate_decl_err(&self.core, "effect operation")?;

        op.name.id = Some(op_id.into());
        op.ty.visit(self)
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let id = self.core.ids.next_handler_id();
        let name = handler.name.raw;
        let span = handler.name.span();

        self.core
            .register_global_name(id, name, span)
            .ok_or_duplicate_decl_err(&self.core, "effect handler")?;

        let var_id = self.core.ids.next_var_id();
        self.core.register_id_name(var_id, name, span);
        self.ast.id_var_ids.insert(id.into(), var_id);

        self.scope_id = Some(id.into());
        handler.name.id = Some(id.into());
        handler.ty_args.visit(self)?;
        handler.ops.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_effect_op_impl(&mut self, op: &mut EffectOpImpl) -> diag::Result<()> {
        let handler_id = self.scope_id.unwrap().handler_id();
        let id = self.core.ids.next_var_id();
        let name = op.name.raw;
        let span = op.name.span();

        self.core
            .register_scoped_name(handler_id, id, name, span, Exclusivity::None)
            .ok_or_duplicate_decl_err(&self.core, "effect operation")?;

        op.name.id = Some(id.into());
        op.params.visit(self)?;
        op.expr.visit(self)
    }

    fn visit_var_decl(&mut self, var: &mut Decl) -> diag::Result<()> {
        if self.level > 0 {
            return Diagnostic::error(
                "variable declarations may only occur at the top level",
                var.span(),
            )
            .into_err();
        }

        let id = self.core.ids.next_decl_id();
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

        self.core.register_id_name(id, name, span);
        self.all_decls.insert(name, (id, var.span()));

        // this declaration is for a compiler builtin
        if self.core.builtins.contains(&name) {
            // create a variable id associated with the declaration to use in resolution
            let var_id = self.core.ids.next_var_id();
            self.core.register_global_name(var_id, name, span).unwrap();
            self.ast.id_var_ids.insert(id.into(), var_id);
            self.ast.var_decl_ids.insert(var_id, id);
        }

        var.name.id = Some(id.into());
        var.ty.visit(self)
    }

    fn visit_ty_param(&mut self, param: &mut Ident) -> diag::Result<()> {
        let id = self.core.ids.next_poly_var_id();
        let name = param.raw;
        let span = param.span();
        self.core.register_id_name(id, name, span);
        param.id = Some(id.into());
        Ok(())
    }

    //
    //

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        let id = self.core.ids.next_var_id();
        let name = func.name.raw;
        let span = func.name.span();
        self.core.register_id_name(id, name, span);
        func.name.id = Some(id.into());
        if self.level > 0 {
            // local function, we dont have to do anything else
            func.params.visit(self)?;
            return func.body.visit(self);
        }

        // check if there is a declaration
        if let Some((decl_id, span)) = self.all_decls.get(&name) {
            // check if declared variable is redefined
            if let Some(var_id) = self.ast.id_var_ids.insert((*decl_id).into(), id) {
                return DuplicateDeclErr {
                    kind: "variable",
                    span: func.name.span(),
                    first: self.core.id_as_span(var_id).unwrap(),
                }
                .into_err();
            }
            self.ast.var_decl_ids.insert(id, *decl_id);
        } else {
            // this is a global function, register it
            self.core
                .register_global_name(id, name, span)
                .ok_or_duplicate_decl_err(&self.core, "function")?;
        }

        func.params.visit(self)?;
        func.body.visit(self)
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
                let id = self.core.ids.next_var_id();
                let name = ident.raw;
                let span = ident.span();
                self.core.register_id_name(id, name, span);
                ident.id = Some(id.into());
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

pub fn collect<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut module = module;
    let mut results = vec![];
    let mut visitor = CollectVisitor::new(ast, core);
    for item in &mut module.items {
        if let Err(e) = item.visit(&mut visitor) {
            results.push(e);
        }
    }

    if results.is_empty() {
        PassResult::Ok(())
    } else {
        PassResult::Err(results)
    }
}

trait OkOrDuplicateDeclErr {
    type Ok;
    fn ok_or_duplicate_decl_err(
        self,
        ctx: &core::Context,
        kind: &'static str,
    ) -> Result<Self::Ok, Diagnostic>;
}

impl<T> OkOrDuplicateDeclErr for Result<T, (Span, Id)> {
    type Ok = T;
    fn ok_or_duplicate_decl_err(
        self,
        ctx: &core::Context,
        kind: &'static str,
    ) -> Result<T, Diagnostic> {
        match self {
            Ok(x) => Ok(x),
            Err((span, existing_id)) => DuplicateDeclErr {
                kind,
                span,
                first: ctx.id_as_span(existing_id).unwrap(),
            }
            .into_err(),
        }
    }
}

fn swap_identifiable_with_id<L, R>(e: &mut Either<L, R>) -> L
where
    L: Identifiable<Id = R>,
    R: std::fmt::Debug + Copy + Eq + Ord,
{
    let id = match e {
        Left(l) => l.id(),
        Right(_) => unreachable!(),
    };

    match std::mem::replace(e, Right(id)) {
        Left(l) => l,
        Right(_) => unreachable!(),
    }
}
