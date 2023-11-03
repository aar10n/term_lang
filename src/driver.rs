use term_ast as ast;
use term_ast_lower as lower;
use term_ast_pass as pass;
use term_common as common;
use term_core as core;
use term_cps as cps;
use term_diag as diag;
use term_parse as parse;
use term_print as print;
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

    ctx.register_builtin("builtin_add_int");
    ctx.register_builtin("builtin_sub_int");
    ctx
}

pub fn evaluate(ctx: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let file = ctx.sources.get(source_id).unwrap();
    let module = &mut parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let ast = &mut ast::Context::new();
    pass::collect(ast, ctx, module).into_result()?;
    pass::resolve(ast, ctx, module).into_result()?;
    pass::lower_all(ast, ctx, module).into_result()?;
    // ctx.print_stdout(&());

    let methods = ast
        .method_ids
        .clone()
        .into_iter()
        .filter(|id| {
            ast.dep_graph
                .get(id)
                .and_then(|v| if v.is_empty() { None } else { Some(()) })
                .is_none()
        })
        .collect::<Vec<_>>();

    let order = solve::sort_dependencies(&ast.dep_graph);
    for id in methods.into_iter().chain(order.into_iter()) {
        let def = ctx.defs[&id].clone();
        if !def.borrow().ty.is_infer() {
            continue;
        }

        println!("inferring type of {}", id.pretty_string(ctx));
        let body = {
            let def = ctx.defs[&id].clone();
            let def = def.borrow();
            def.body.clone()
        };

        println!("{}", body.pretty_string(ctx));
        let (body, ty) = solve::infer(ctx, body, false)?;
        println!("  {}", ty.pretty_string(ctx));

        let mut def = def.borrow_mut();
        def.body = body.clone();
        def.ty = ty;
    }

    // ctx.print_stdout(&());
    println!("{GREEN}Done{RESET}");
    Ok(())
}
