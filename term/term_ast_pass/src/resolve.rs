use crate::{Context, PassResult, UnresolvedNameErr};
use term_ast as ast;
use term_core as core;
use term_diag as diag;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::{Id, ParentId, PolyVarId, VarId};
use diag::{Diagnostic, IntoDiagnostic, IntoError};

use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct ResolveVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
    scopes: Vec<Scope>,
    scope_id: Option<Id>,
}

impl<'v, 'ast> ResolveVisitor<'v, 'ast> {
    pub fn new(ctx: &'v mut Context<'ast>) -> Self {
        Self {
            ctx,
            scopes: vec![Scope::default()],
            scope_id: None,
        }
    }

    pub fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn resolve_global<F>(&self, kind: &'static str, name: &Ident, pred: F) -> diag::Result<Id>
    where
        F: FnOnce(&Id) -> bool,
    {
        match self.ctx.name_to_id(name) {
            Some(id) if pred(&id) => Ok(id),
            Some(id) => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.ctx.id_as_span(id),
            }
            .into_err(),
            None => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: None,
            }
            .into_err(),
        }
    }

    pub fn resolve_scoped<T, F>(
        &self,
        kind: &'static str,
        parent_id: T,
        name: &Ident,
        pred: F,
    ) -> diag::Result<Id>
    where
        T: Into<ParentId> + Copy,
        F: FnOnce(&Id) -> bool,
    {
        match self.ctx.resolve_scoped_name(parent_id, &name.raw) {
            Some(id) if pred(&id) => Ok(id),
            Some(id) => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.ctx.id_as_span(id),
            }
            .into_err(),
            None => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: None,
            }
            .into_err(),
        }
    }

    pub fn resolve_var(&self, name: &Ident) -> diag::Result<Option<VarId>> {
        // first try resolving in local vars
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.vars.get(&name.raw) {
                return Ok(Some(*id));
            }
        }

        // now check the global namespace
        if let Some(id) = self.ctx.globals.get(&name.raw) {
            match *id {
                Id::DataCon(id) => {
                    // map data_con to its var_id first
                    Ok(Some(*self.ctx.con_var_ids.get(&id).unwrap()))
                }
                Id::Decl(id) => match self.ctx.decl_var_ids.get(&id) {
                    Some(var_id) => Ok(Some(*var_id)),
                    None => {
                        // the name is declared but not defined
                        Diagnostic::error("name is declared but is not defined", name.span())
                            .with_note("declared here", self.ctx.id_as_span(id).unwrap())
                            .into_err()
                    }
                },
                Id::Var(id) => Ok(id.into()),
                id => Diagnostic::error("expected variable", self.ctx.id_as_span(id).unwrap())
                    .with_inline_note("name is defined but is not a variable")
                    .into_err(),
            }
        } else {
            Ok(None)
        }
    }

    pub fn resolve_ty(&self, name: &Ustr) -> Option<Id> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.tys.get(name) {
                return Some((*id).into());
            }
        }
        None
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for ResolveVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }
    fn push_scope(&mut self) {
        self.scopes.push(Scope::default())
    }
    fn pop_scope(&mut self) {
        let s = self.scopes.pop();
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        self.visit_effect_ident(&mut handler.effect)?;
        self.visit_handler_ident(&mut handler.name)?;

        self.scope_id = handler.effect.id;
        handler.ty_args.visit(self)?;
        handler.ops.visit(self)?;
        self.scope_id = None;
        Ok(())
    }

    fn visit_effect_op_impl(&mut self, op: &mut EffectOpImpl) -> diag::Result<()> {
        let effect_id = self.scope_id.unwrap().effect_id();
        let op_id = self
            .resolve_scoped("operation", effect_id, &mut op.name, Id::is_effect_op)?
            .effect_op_id();

        op.op_id = Some(op_id);
        op.params.visit(self)?;
        op.expr.visit(self)
    }

    fn visit_class_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global("class", ident, Id::is_class)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_data_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global("data", ident, Id::is_data)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_con_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global("constructor", ident, Id::is_data_con)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_effect_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global("effect", ident, Id::is_effect)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_handler_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global("handler", ident, Id::is_handler)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_ty_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if let Some(id) = ident.id {
            self.scope_mut().tys.insert(ident.raw, id.poly_var_id());
            return Ok(());
        } else if let Some(id) = self.resolve_ty(ident) {
            ident.id = Some(id);
            return Ok(());
        }

        let id = self.resolve_global("type", ident, |id| id.is_data() | id.is_poly_var())?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_var_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if let Some(id) = ident.id {
            if let Id::Var(id) = id {
                self.scope_mut().vars.insert(ident.raw, id);
            }
            return Ok(());
        }

        if let Some(id) = self.resolve_var(ident)? {
            ident.id = Some(id.into());
            Ok(())
        } else if self.ctx.ambiguous_names.contains(&ident.raw) {
            // ambiguous - resolve later
            ident.id = None;
            Ok(())
        } else {
            UnresolvedNameErr {
                kind: "variable",
                name: ident.raw.to_string(),
                span: ident.span(),
                conflict: None,
            }
            .into_err()
        }
    }
}

#[derive(Debug, Default)]
struct Scope {
    pub vars: UstrMap<VarId>,
    pub tys: UstrMap<PolyVarId>,
}

pub fn resolve<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut results = vec![];
    let mut resolver = ResolveVisitor::new(ctx);
    for item in &mut module.items {
        if let Err(e) = item.visit(&mut resolver) {
            results.push(e);
        }
    }

    if results.is_empty() {
        PassResult::Ok(vec![])
    } else {
        PassResult::Err(results)
    }
}
