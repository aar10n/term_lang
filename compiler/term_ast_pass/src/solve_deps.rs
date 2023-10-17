use crate::PassResult;
use term_ast as ast;
use term_ast_lower as lower;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::{Ef, TyE, VarId};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct SolveDependencyVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,
    graph: BTreeMap<VarId, Vec<VarId>>,
}

impl<'ctx> SolveDependencyVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self {
            ast,
            core,
            graph: Default::default(),
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for SolveDependencyVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

    // fn visit_expr(&mut self, expr: &mut Expr) -> diag::Result<()> {

    // }
}

pub fn solve_deps<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult<BTreeMap<VarId, Vec<VarId>>> {
    let mut visitor = SolveDependencyVisitor::new(ast, core);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(visitor.graph),
        Err(err) => PassResult::Err(vec![err]),
    }
}
