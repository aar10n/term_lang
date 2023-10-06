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
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }

    fn visit_func(
        &mut self,
        ident: &mut Ident,
        params: &mut Vec<P<Pat>>,
        body: &mut P<Expr>,
    ) -> diag::Result<()> {
        let id = ident.id.unwrap().var_id();
        let body = lower::lower_lambda(&mut self.ctx, params, body)?;
        println!("inferring type of body: {}", body.pretty_string(self.ctx));
        let ty = solve::infer(&mut self.ctx, &body)?;
        println!("type inferred to be: {}", ty.pretty_string(self.ctx));

        // let ty = if let Some(decl_id) = self.ctx.var_decl_ids.get(&id) {
        //     let decl = self.ctx.decls[decl_id].clone();
        //     let decl = decl.borrow();

        //     decl.ty.lower(&mut self.ctx)?
        // } else {
        //     TyE::infer()
        // };

        // let def = core::Def::new(id, ty, body);
        // self.ctx.defs.insert(id, def);
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
