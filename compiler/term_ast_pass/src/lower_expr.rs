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

    fn visit_func(
        &mut self,
        ident: &mut Ident,
        params: &mut Vec<P<Pat>>,
        body: &mut P<Expr>,
    ) -> diag::Result<()> {
        self.ctx.solve.typings.push_empty();
        let id = ident.id.unwrap().var_id();
        let body = lower::lower_lambda(&mut self.ctx, params, body)?;

        let ut = {
            let a = TyE::pure(self.ctx.solve.new_ty_var());
            let b = TyE::pure(self.ctx.solve.new_ty_var());
            let f = self.ctx.solve.new_ef_var();
            TyE::pure_func(a, b).with_ef(f)
        };
        self.ctx.solve.typings.insert(core::Expr::Var(id), ut);

        println!("inferring type of : {}", id.pretty_string(self.ctx));
        // panic!();
        println!("{}", body.pretty_string(self.ctx));

        let ty = solve::infer_partial(&mut self.ctx.solve, &body)?;
        let ty = solve::generalize(&mut self.ctx.solve, ty, &mut Default::default());
        // let ty = solve::infer_recursive(&mut self.ctx.solve, id, &body)?;
        let ty = lower::fix_ty(self.ctx, ty);

        println!("type inferred to be: {}", ty.pretty_string(self.ctx));
        let def = core::Def::new(id, ty, body);
        self.ctx.defs.insert(id, def);
        self.ctx.solve.typings.pop();
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
