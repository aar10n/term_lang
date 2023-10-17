use crate::PassResult;
use term_ast as ast;
use term_ast_lower as lower;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::TyE;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use lower::Lower;
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct LowerImplVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,
}

impl<'ctx> LowerImplVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self { ast, core }
    }

    pub fn register_defs(&mut self, defs: Vec<core::Def>) {
        for def in defs {
            println!(
                "registering def {} : {}",
                def.id.pretty_string(self.core),
                def.ty.pretty_string(self.core)
            );
            self.core.defs.insert(def.id, RefCell::new(def).into());
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for LowerImplVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

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
        self.register_defs(defs);
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let inst = inst.lower_ast_core(self.ast, self.core)?;
        self.core.insts.insert(inst.id, RefCell::new(inst).into());
        Ok(())
    }
}

pub fn lower_impls<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut visitor = LowerImplVisitor::new(ast, core);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(()),
        Err(err) => PassResult::Err(vec![err]),
    }
}
