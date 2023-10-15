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

struct LowerExprVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
}

impl<'v, 'ast> LowerExprVisitor<'v, 'ast> {
    pub fn new(ctx: &'v mut Context<'ast>) -> Self {
        Self { ctx }
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for LowerExprVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut ast::Context {
        self.ctx.ast
    }

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        let (expr, ty) = func.lower(self.ctx)?;
        let id = func.name.id.unwrap().var_id();

        println!(
            "type of {} inferred to be {}",
            id.pretty_string(self.ctx),
            ty.pretty_string(self.ctx)
        );

        let def = core::Def::new(id, ty, expr);
        self.ctx.defs.insert(id, def);
        Ok(())
    }
}

pub fn lower_exprs<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut visitor = LowerExprVisitor::new(ctx);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(vec![]),
        Err(err) => PassResult::Err(vec![err]),
    }
}
