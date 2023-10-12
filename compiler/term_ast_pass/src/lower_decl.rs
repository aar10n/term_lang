use crate::{lower, Context, PassResult, UnresolvedNameErr};
use term_ast as ast;
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

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct LowerDeclVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
}

impl<'v, 'ast> LowerDeclVisitor<'v, 'ast> {
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
            self.ctx.defs.insert(def.id, def);
        }
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for LowerDeclVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let (data, defs) = data.lower(&mut self.ctx)?;
        solve::check_valid_type_params(&mut self.ctx, &data.params, &data.constraints)?;

        self.ctx.datas.insert(data.id, data);
        self.register_defs(defs);
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect_decl: &mut EffectDecl) -> diag::Result<()> {
        let (effect, defs) = effect_decl.lower(&mut self.ctx)?;
        solve::check_valid_type_params(&mut self.ctx, &effect.params, &effect.constraints)?;

        self.ctx.effects.insert(effect.id, effect);
        self.register_defs(defs);
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let class = class.lower(&mut self.ctx)?;
        self.ctx.classes.insert(class.id, class);
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut VarDecl) -> diag::Result<()> {
        if let Some(builtin) = var.lower(&mut self.ctx)? {
            self.register_defs(vec![builtin]);
        }
        Ok(())
    }
}

pub fn lower_decls<'v, 'ast>(ctx: &'v mut Context<'ast>, module: &'v mut Module) -> PassResult {
    let mut visitor = LowerDeclVisitor::new(ctx);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(vec![]),
        Err(err) => PassResult::Err(vec![err]),
    }
}
