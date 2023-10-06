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

struct LowerDeclVisitor<'v, 'ast> {
    ctx: &'v mut Context<'ast>,
}

impl<'v, 'ast> LowerDeclVisitor<'v, 'ast> {
    pub fn new(ctx: &'v mut Context<'ast>) -> Self {
        Self { ctx }
    }
}

impl<'ast> Visitor<'ast, (), Diagnostic> for LowerDeclVisitor<'_, 'ast> {
    fn context(&mut self) -> &mut Context<'ast> {
        self.ctx
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        use core::Ty;
        let data = data.lower(&mut self.ctx)?;
        solve::check_valid_type_params(&mut self.ctx, &data.params, &data.constraints)?;

        // register constructor function types
        for con in data.cons.iter() {
            let mut ty = Ty::Data(
                data.id,
                data.params
                    .iter()
                    .map(|p| TyE::pure(Ty::Poly(*p)))
                    .collect(),
            );
            for field in con.fields.iter().rev() {
                ty = Ty::Func(TyE::pure(field.ty.clone()).into(), TyE::pure(ty).into());
            }

            println!(
                "registering constructor {} : {}",
                self.ctx.id_as_str(con.var_id),
                ty.pretty_string(self.ctx)
            );
            let def = core::Def::new_builtin(con.var_id, TyE::pure(ty));
            self.ctx.defs.insert(con.var_id, def);
        }

        self.ctx.datas.insert(data.id, data);
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect: &mut EffectDecl) -> diag::Result<()> {
        let effect = effect.lower(&mut self.ctx)?;
        solve::check_valid_type_params(&mut self.ctx, &effect.params, &effect.constraints)?;
        self.ctx.effects.insert(effect.id, effect);
        Ok(())
    }

    fn visit_effect_handler(&mut self, handler: &mut EffectHandler) -> diag::Result<()> {
        let handler = handler.lower(&mut self.ctx)?;
        self.ctx.handlers.insert(handler.id, handler);
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let class = class.lower(&mut self.ctx)?;
        self.ctx.classes.insert(class.id, class);
        Ok(())
    }

    fn visit_class_inst(&mut self, inst: &mut ClassInst) -> diag::Result<()> {
        let inst = inst.lower(&mut self.ctx)?;
        self.ctx.insts.insert(inst.id, inst);
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut VarDecl) -> diag::Result<()> {
        if !self.ctx.builtins.contains(&var.name.raw) {
            return Ok(());
        }

        let id = var.name.id.unwrap().decl_id();
        let var_id = self.ctx.decl_var_ids[&id];
        let name = var.name.raw;
        let ty = var.ty.lower(self.ctx)?;

        println!(
            "registering built-in {} : {}",
            var.name.pretty_string(self.ctx),
            var.ty.pretty_string(self.ctx)
        );

        let def = core::Def::new_builtin(var_id, ty);
        self.ctx.defs.insert(var_id, def);
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
