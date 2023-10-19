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
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for LowerVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

    //
    // Decls
    //

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let (data, defs) = data.lower_ast_core(self.ast, self.core)?;
        self.core.datas.insert(data.id, RefCell::new(data).into());

        for def in defs {
            self.register_def(def);
        }
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect_decl: &mut EffectDecl) -> diag::Result<()> {
        let (effect, defs) = effect_decl.lower_ast_core(self.ast, self.core)?;

        self.core
            .effects
            .insert(effect.id, RefCell::new(effect).into());

        for def in defs {
            self.register_def(def);
        }
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let class = class.lower_ast_core(self.ast, self.core)?;
        self.core
            .classes
            .insert(class.id, RefCell::new(class).into());
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut VarDecl) -> diag::Result<()> {
        println!(
            "lowering var decl: {} : {}",
            var.name.pretty_string(self.ast),
            var.ty.pretty_string(self.ast)
        );
        if let Some(def) = var.lower_ast_core(self.ast, self.core)? {
            self.register_def(def);
        }
        Ok(())
    }

    //
    // Impls
    //

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let is_default = handler.default;
        let (handler, defs) = handler.lower_ast_core(self.ast, self.core)?;
        if is_default {
            let effect = self.core.effects.get(&handler.effect_id).cloned().unwrap();
            let mut effect = effect.borrow_mut();
            let var_id = self.ast.handler_var_ids[&handler.id];
            effect.default = Some(var_id);
        }

        self.core
            .handlers
            .insert(handler.id, RefCell::new(handler).into());

        for def in defs {
            self.register_def(def);
        }
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let inst = inst.lower_ast_core(self.ast, self.core)?;
        self.core.insts.insert(inst.id, RefCell::new(inst).into());
        Ok(())
    }

    //
    // Exprs
    //

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
