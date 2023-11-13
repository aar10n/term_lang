use crate::PassResult;
use term_ast as ast;
use term_ast_lower as lower;
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use ast::visit::{Visit, Visitor};
use ast::Module;
use common::dep_graph::DependencyGraph;
use core::{ClassId, DeclId, Def, EffectId, Exclusivity, Id, TyE, VarId};
use diag::{Diagnostic, IntoDiagnostic, IntoError};
use print::{PrettyPrint, PrettyString};

use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;
use ustr::{ustr, UstrMap};

pub fn infer_all<'a>(
    ast: &'a mut ast::Context,
    core: &'a mut core::Context,
    module: &'a mut Module,
) -> PassResult {
    // println!(">>> infering method types <<<");
    for (var_id, inst_id) in ast.method_ids.clone() {
        let name = core.id_as_ustr(var_id);
        match infer_def_ty(core, var_id) {
            Ok(ty) => core.signatures.entry(ty).or_default().push(name),
            Err(e) => return PassResult::Err(vec![e]),
        };
    }

    // println!("------ signatures methods ------");
    // for (ty, names) in core.signatures.iter() {
    //     println!(
    //         "{}: {}",
    //         ty.pretty_string(core),
    //         names
    //             .iter()
    //             .cloned()
    //             .map(|n| n.to_string())
    //             .collect::<Vec<_>>()
    //             .join(", ")
    //     );
    // }
    // println!("-------------------------------");

    // println!("{:-^1$}", "", 32);
    // core.functions.print_stdout(&core);
    // println!("{:-^1$}", "", 32);

    // println!(">>> infering function types <<<");
    let method_ids = ast.method_ids.iter().map(|(id, _)| *id).collect::<Vec<_>>();
    let ids = DependencyGraph::from(&ast.dep_graph)
        .removing(&method_ids)
        .total_order();
    for id in ids {
        if let Err(e) = infer_def_ty(core, id) {
            return PassResult::Err(vec![e]);
        }
    }
    PassResult::Ok(())
}

fn infer_def_ty<'a>(core: &'a mut core::Context, id: VarId) -> diag::Result<TyE> {
    let def = core.defs[&id].clone();
    {
        let def = def.borrow();
        if !def.ty.is_infer() {
            return Ok(def.ty.clone());
        }
    }

    let (body, trace) = {
        let def = def.borrow();
        (def.body.clone(), !def.builtin)
    };
    println!("inferring type of {}", id.pretty_string(core),);
    let (body, ty) = solve::infer(core, body, trace)?;
    println!("=> {}", ty.pretty_string(core));

    let mut def = def.borrow_mut();
    def.body = body;
    def.ty = ty.clone();
    Ok(ty)
}
