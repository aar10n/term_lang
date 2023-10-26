use crate::{PassResult, UnresolvedNameErr};
use term_ast as ast;
use term_ast_lower as lower;
use term_core as core;
use term_diag as diag;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::{Id, ParentId, PolyVarId, VarId};
use diag::{Diagnostic, IntoDiagnostic, IntoError};

use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

enum Vis {
    Local,
    Global,
}

struct ResolveVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,

    scopes: Vec<Scope>,
    scope_id: Option<Id>,
    current_var: Option<VarId>,
}

impl<'ctx> ResolveVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self {
            ast,
            core,

            scopes: vec![Scope::default()],
            scope_id: None,
            current_var: None,
        }
    }

    pub fn scope(&self) -> &Scope {
        self.scopes.last().unwrap()
    }

    pub fn scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    pub fn resolve_global_name<F>(
        &self,
        kind: &'static str,
        name: &Ident,
        pred: F,
    ) -> diag::Result<Id>
    where
        F: FnOnce(&Id) -> bool,
    {
        match self.core.global_names.get(name) {
            Some(id) if pred(&id) => Ok(*id),
            Some(id) => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.core.id_as_span(*id),
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

    pub fn resolve_global_type<F>(
        &self,
        kind: &'static str,
        name: &Ident,
        pred: F,
    ) -> diag::Result<Id>
    where
        F: FnOnce(&Id) -> bool,
    {
        match self.core.global_types.get(name) {
            Some(id) if pred(&id) => Ok(*id),
            Some(id) => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.core.id_as_span(*id),
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
        match self.core.resolve_scoped_name(parent_id, &name.raw) {
            Some(id) if pred(&id) => Ok(id),
            Some(id) => UnresolvedNameErr {
                kind,
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.core.id_as_span(id),
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

    pub fn resolve_var(&self, name: &Ident) -> diag::Result<Option<(VarId, Vis)>> {
        // first try resolving in local vars
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.vars.get(&name.raw) {
                if self.core.global_names.contains_key(&name.raw) {
                    return Ok(Some((*id, Vis::Global)));
                } else {
                    return Ok(Some((*id, Vis::Local)));
                }
            }
        }

        // now check the global namespace
        if let Some(id) = self.core.global_names.get(&name.raw) {
            match *id {
                Id::DataCon(id) => {
                    // map data_con to its var_id first
                    Ok(Some((*self.ast.con_var_ids.get(&id).unwrap(), Vis::Global)))
                }
                Id::Decl(id) => match self.ast.decl_var_ids.get(&id) {
                    Some(var_id) => Ok(Some((*var_id, Vis::Global))),
                    None => {
                        // the name is declared but not defined
                        Diagnostic::error("name is declared but is not defined", name.span())
                            .with_note("declared here", self.core.id_as_span(id).unwrap())
                            .into_err()
                    }
                },
                Id::Handler(id) => {
                    // map data_con to its var_id first
                    Ok(Some((
                        *self.ast.handler_var_ids.get(&id).unwrap(),
                        Vis::Global,
                    )))
                }
                Id::Var(id) => Ok(Some((id, Vis::Global))),
                id => Diagnostic::error("expected variable", self.core.id_as_span(id).unwrap())
                    .with_inline_note("name is defined but is not a variable")
                    .into_err(),
            }
        } else {
            Ok(None)
        }
    }

    pub fn resolve_ty(&self, name: &Ident) -> diag::Result<Option<Id>> {
        for scope in self.scopes.iter().rev() {
            if let Some(id) = scope.tys.get(&name.raw) {
                return Ok(Some((*id).into()));
            }
        }

        match self.core.global_types.get(name) {
            Some(id) if id.is_data() || id.is_poly_var() => Ok(Some(*id)),
            Some(id) => UnresolvedNameErr {
                kind: "type",
                name: name.raw.to_string(),
                span: name.span(),
                conflict: self.core.id_as_span(*id),
            }
            .into_err(),
            None => Ok(None),
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for ResolveVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
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

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        if self.scopes.len() > 1 {
            return Ok(());
        }

        self.current_var = Some(func.name.id.unwrap().var_id());
        func.walk(self)?;
        self.current_var = None;
        Ok(())
    }

    //
    //

    fn visit_class_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global_type("class", ident, Id::is_class)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_data_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global_type("data", ident, Id::is_data)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_con_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global_name("constructor", ident, Id::is_data_con)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_effect_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global_type("effect", ident, Id::is_effect)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_handler_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if ident.id.is_some() {
            return Ok(());
        }

        let id = self.resolve_global_name("handler", ident, Id::is_handler)?;
        ident.id = Some(id);
        Ok(())
    }

    fn visit_ty_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if let Some(id) = ident.id {
            self.scope_mut().tys.insert(ident.raw, id.poly_var_id());
            return Ok(());
        }

        if let Some(id) = self.resolve_ty(ident)? {
            ident.id = Some(id);
        } else if ident.raw.chars().next().unwrap().is_lowercase() {
            // implicit type variable
            let id = self.core.ids.next_poly_var_id();
            self.core.register_id_name(id, ident.raw, ident.span());
            ident.id = Some(id.into());
        } else {
            return UnresolvedNameErr {
                kind: "type",
                name: ident.raw.to_string(),
                span: ident.span(),
                conflict: None,
            }
            .into_err();
        }
        Ok(())
    }

    fn visit_var_ident(&mut self, ident: &mut Ident) -> diag::Result<()> {
        if let Some(id) = ident.id {
            // this is a declaration
            if let Id::Var(id) = id {
                if self.scopes.len() == 1 {
                    // only add to dep graph if its declared at the global scope
                    self.core.dep_graph.insert(id, BTreeSet::new());
                }
                self.scope_mut().vars.insert(ident.raw, id);
            }
            return Ok(());
        }

        if let Some((id, vis)) = self.resolve_var(ident)? {
            if let Some(parent_id) = self.current_var && matches!(vis, Vis::Global) {
                self.core.dep_graph.entry(parent_id).or_default().insert(id);
            }

            ident.id = Some(id.into());
            Ok(())
        } else if self.ast.ambiguous_names.contains(&ident.raw) {
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

pub fn resolve<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut results = vec![];
    let mut resolver = ResolveVisitor::new(ast, core);
    for item in &mut module.items {
        if let Err(e) = item.visit(&mut resolver) {
            results.push(e);
        }
    }

    if results.is_empty() {
        PassResult::Ok(())
    } else {
        PassResult::Err(results)
    }
}
