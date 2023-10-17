use crate::PassResult;
use term_ast as ast;
use term_ast_lower as lower;
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
use std::cell::RefCell;

use std::collections::BTreeMap;
use std::ops::{Deref, DerefMut};
use ustr::{Ustr, UstrMap};

struct LowerDeclVisitor<'ctx> {
    ast: &'ctx mut ast::Context,
    core: &'ctx mut core::Context,
}

impl<'ctx> LowerDeclVisitor<'ctx> {
    pub fn new(ast: &'ctx mut ast::Context, core: &'ctx mut core::Context) -> Self {
        Self { ast, core }
    }

    pub fn register_defs(&mut self, defs: Vec<core::Def>) {
        for def in defs {
            println!(
                "registering def {} : {}",
                def.id.pretty_string(self.core),
                def.ty.pretty_string(self.core)
            );
            self.core.defs.insert(def.id, RefCell::new(def).into());
        }
    }
}

impl<'ctx> Visitor<'ctx, (), Diagnostic> for LowerDeclVisitor<'ctx> {
    fn context(&mut self) -> &mut ast::Context {
        self.ast
    }

    fn visit_data_decl(&mut self, data: &mut DataDecl) -> diag::Result<()> {
        let (data, defs) = data.lower_ast_core(self.ast, self.core)?;
        self.core.datas.insert(data.id, RefCell::new(data).into());
        self.register_defs(defs);
        Ok(())
    }

    fn visit_effect_decl(&mut self, effect_decl: &mut EffectDecl) -> diag::Result<()> {
        let (effect, defs) = effect_decl.lower_ast_core(self.ast, self.core)?;

        self.core
            .effects
            .insert(effect.id, RefCell::new(effect).into());
        self.register_defs(defs);
        Ok(())
    }

    fn visit_class_decl(&mut self, class: &mut ClassDecl) -> diag::Result<()> {
        let class = class.lower_ast_core(self.ast, self.core)?;
        self.core
            .classes
            .insert(class.id, RefCell::new(class).into());
        Ok(())
    }

    fn visit_var_decl(&mut self, var: &mut VarDecl) -> diag::Result<()> {
        println!("lowering var decl: {}", var.pretty_string(self.ast));
        if let Some(def) = var.lower_ast_core(self.ast, self.core)? {
            self.register_defs(vec![def]);
        }
        Ok(())
    }
}

pub fn lower_decls<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    let mut visitor = LowerDeclVisitor::new(ast, core);
    match module.visit(&mut visitor) {
        Ok(()) => PassResult::Ok(()),
        Err(err) => PassResult::Err(vec![err]),
    }
}
