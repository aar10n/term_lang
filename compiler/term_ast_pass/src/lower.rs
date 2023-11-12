use crate::PassResult;
use term_ast as ast;
use term_ast_lower as lower;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::{Ef, TyE};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use lower::Lower;
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct LowerVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,
}

impl<'ctx> LowerVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self { ast, core }
    }

    pub fn register_def(&mut self, def: core::Def) {
        println!(
            "registering def {} : {}",
            def.id.pretty_string(self.core),
            def.ty.pretty_string(self.core)
        );
        self.core.defs.insert(def.id, RefCell::new(def).into());
    }

    pub fn register_defs(&mut self, defs: impl IntoIterator<Item = core::Def>) {
        for def in defs {
            self.register_def(def);
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for LowerVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let class = class.lower_ast_core(self.ast, self.core)?;
        self.core
            .classes
            .insert(class.id, RefCell::new(class).into());
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let class_id = inst.class.id.unwrap().class_id();
        let inst_id = inst.inst_id.unwrap();

        let (members, defs) = inst.lower_ast_core(self.ast, self.core)?;
        let class = self.core.classes[&class_id].clone();
        class.borrow_mut().insts.push(inst_id);

        self.core.instances.insert(inst_id, members);
        self.register_defs(defs);
        Ok(())
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let defs = data.lower_ast_core(self.ast, self.core)?;
        self.register_defs(defs);
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect_decl: &mut EffectDecl) -> diag::Result<()> {
        let (effect, defs) = effect_decl.lower_ast_core(self.ast, self.core)?;

        self.core
            .effects
            .insert(effect.id, RefCell::new(effect).into());
        self.register_defs(defs);
        Ok(())
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let effect_id = handler.effect.id.unwrap().effect_id();
        let han_id = handler.name.id.unwrap().handler_id();
        let var_id = self.ast.id_var_ids[&han_id.into()];

        let (ops, defs) = handler.lower_ast_core(self.ast, self.core)?;
        if handler.default {
            let effect = self.core.effects.get(&effect_id).cloned().unwrap();
            let mut effect = effect.borrow_mut();
            effect.default = Some((han_id, var_id));
        }

        self.core.handlers.insert(han_id, ops);
        self.register_defs(defs);
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut Decl) -> diag::Result<()> {
        if let Some(def) = var.lower_ast_core(self.ast, self.core)? {
            self.register_def(def);
        }
        Ok(())
    }

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        let id = func.name.id.unwrap().var_id();
        let ty = TyE::infer();
        let expr = func.lower_ast_core(self.ast, self.core)?;
        let def = core::Def::new(id, ty, expr);
        self.register_def(def);
        Ok(())
    }
}

pub fn lower_all<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut visitor = LowerVisitor::new(ast, core);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(()),
        Err(err) => PassResult::Err(vec![err]),
    }
}
