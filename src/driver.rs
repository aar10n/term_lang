use term_ast as ast;
use term_ast_lower as lower;
use term_ast_pass as pass;
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_parse as parse;
use term_print as print;
use term_rewrite as rewrite;
use term_solve as solve;

use ast::ItemKind;
use common::source::{SourceFile, SourceId, SourceMap};
use common::span::Span;
use core::{Context, Expr, Ty, TyE, VarId};
use diag::{IntoDiagnostic, Report};
use lower::Lower;
use print::ansi::{GREEN, RESET};
use print::{PrettyPrint, PrettyString};
use solve::topo_sort::TopologicalSort;

use std::collections::BTreeMap;

pub fn new_context() -> Context {
    let mut ctx = Context::new();

    ctx.register_builtin("panic");
    ctx.register_builtin("builtin_put_char");
    ctx.register_builtin("builtin_get_char");
    ctx
}

pub fn evaluate(core: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let file = core.sources.get(source_id).unwrap();
    let module = &mut parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let ast = &mut ast::Context::new();
    pass::collect(ast, core, module).into_result()?;
    pass::resolve(ast, core, module).into_result()?;
    pass::lower_all(ast, core, module).into_result()?;
    // module.print_stdout(&ast);
    // core.print_stdout(&());

    let solve = &mut solve::TyContext::new();

    println!("Dependencies:");
    let mut deps = TopologicalSort::<VarId>::new();
    for (id, dep_on) in &core.dep_graph {
        for dep_id in dep_on {
            println!(
                "  {} depends on {}",
                id.pretty_string(core),
                dep_id.pretty_string(core)
            );
            deps.add_dependency(*dep_id, *id);
        }
        // println!(
        //     "  {} -> {}",
        //     id.pretty_string(core),
        //     dep_on
        //         .iter()
        //         .map(|id| id.pretty_string(core))
        //         .collect::<Vec<_>>()
        //         .join(", ")
        // );
    }

    println!("Order:");
    while let mut ids = deps.pop() && !ids.is_empty() {
        ids.sort();

        for id in ids {
            println!("-> {}", id.pretty_string(core));
        }
    }

    // let entry = core.global_names[&ustr::ustr("main")].var_id();

    println!("{GREEN}Done{RESET}");
    Ok(())
}
