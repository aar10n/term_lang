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

struct LowerExprVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,
}

impl<'ctx> LowerExprVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self { ast, core }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for LowerExprVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        let id = func.name.id.unwrap().var_id();
        let ty = TyE::infer();
        let expr = func.lower_ast_core(self.ast, self.core)?;
        let def = core::Def::new(id, ty, expr);
        self.core.defs.insert(id, RefCell::new(def).into());
        Ok(())
    }
}

pub fn lower_exprs<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut visitor = LowerExprVisitor::new(ast, core);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(()),
        Err(err) => PassResult::Err(vec![err]),
    }
}
