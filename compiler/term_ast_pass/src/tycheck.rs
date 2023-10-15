use crate::{lower, Context, PassResult};
use term_ast as ast;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::*;
use core::{Def, Id, TyE, VarId};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use lower::Lower;
use print::{PrettyPrint, PrettyString};
use std::cell::RefCell;
use std::rc::Rc;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct TyCheckVisitor<'v, 'ctx> {
    ctx: &'v mut Context<'ctx>,
}

impl<'v, 'ctx> TyCheckVisitor<'v, 'ctx> {
    pub fn new(ctx: &'v mut Context<'ctx>) -> Self {
        Self { ctx }
    }

    fn get_def_mut(&mut self, id: VarId) -> diag::Result<Rc<RefCell<Def>>> {
        let def = if let Some(def) = self.ctx.defs.get(&id) {
            def.clone()
        } else {
            return Diagnostic::error(
                format!("no definition found for {}", id.pretty_string(self.ctx)),
                self.ctx.id_as_span(id).unwrap(),
            )
            .into_err();
        };
        Ok(def)
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for TyCheckVisitor<'_, 'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ctx.ast
    }

    fn visit_func(&mut self, func: &mut Func) -> diag::Result<()> {
        let id = func.name.id.unwrap().var_id();
        let def = self.get_def_mut(id)?;
        let mut def = def.borrow_mut();

        println!("inferring type of func: {}", id.pretty_string(self.ctx));
        println!("{}", def.body.pretty_string(&self.ctx.solve.ctx));
        Ok(())
    }
}

pub fn ty_check<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut module = module;
    let mut results = vec![];
    let mut visitor = TyCheckVisitor::new(ctx);
    for item in &mut module.items {
        if let Err(e) = item.visit(&mut visitor) {
            results.push(e);
        }
    }

    if results.is_empty() {
        PassResult::Ok(vec![])
    } else {
        PassResult::Err(results)
    }
}
