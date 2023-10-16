use crate::{ast_lower, Context, PassResult, UnresolvedNameErr};
use term_ast as ast;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::*;
use ast_lower::Lower;
use core::TyE;
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct LowerImplVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
}

impl<'v, 'ast> LowerImplVisitor<'v, 'ast> {
    pub fn new(ctx: &'v mut Context<'ast>) -> Self {
        Self { ctx }
    }

    pub fn register_defs(&mut self, defs: Vec<core::Def>) {
        for def in defs {
            println!(
                "registering def {} : {}",
                def.id.pretty_string(self.ctx),
                def.ty.pretty_string(self.ctx)
            );
            self.ctx.defs.insert(def.id, RefCell::new(def).into());
        }
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for LowerImplVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut ast::Context {
        self.ctx.ast
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let is_default = handler.default;
        let (handler, defs) = handler.lower(&mut self.ctx)?;
        if is_default {
            let effect = self.ctx.effects.get(&handler.effect_id).cloned().unwrap();
            let mut effect = effect.borrow_mut();
            let var_id = self.ctx.ast.handler_var_ids[&handler.id];
            effect.default = Some(var_id);
        }

        self.ctx
            .handlers
            .insert(handler.id, RefCell::new(handler).into());
        self.register_defs(defs);
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let inst = inst.lower(&mut self.ctx)?;
        self.ctx.insts.insert(inst.id, RefCell::new(inst).into());
        Ok(())
    }
}

pub fn lower_impls<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut visitor = LowerImplVisitor::new(ctx);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(vec![]),
        Err(err) => PassResult::Err(vec![err]),
    }
}
