use crate::{lower, Context, PassResult, UnresolvedNameErr};
use term_ast as ast;
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
}

impl<'ast> Visitor<'ast, (), Diagnostic> for LowerImplVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let handler = handler.lower(&mut self.ctx)?;
        self.ctx.handlers.insert(handler.id, handler);
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let inst = inst.lower(&mut self.ctx)?;
        self.ctx.insts.insert(inst.id, inst);
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
