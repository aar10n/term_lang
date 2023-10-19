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

pub fn evaluate(ctx: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let actx = &mut ast::Context::new();
    let tctx = &mut solve::TyContext::new();

    let file = ctx.sources.get(source_id).unwrap();
    let module = &mut parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    pass::collect(actx, ctx, module).into_result()?;
    pass::resolve(actx, ctx, module).into_result()?;
    pass::lower_all(actx, ctx, module).into_result()?;
    // module.print_stdout(&ast);
    // ctx.print_stdout(&());

    let order = solve::sort_dependencies(&ctx);
    for id in order {
        let body = {
            let def = ctx.defs[&id].clone();
            let def = def.borrow();
            def.body.clone()
        };

        println!("inferring type of {}", id.pretty_string(ctx));
        let (body, ty) = solve::infer(ctx, tctx, body)?;
        println!("  {}", ty.pretty_string(ctx));

        let def = ctx.defs[&id].clone();
        let mut def = def.borrow_mut();
        def.body = body;
        def.ty = ty;
    }

    println!("{GREEN}Done{RESET}");
    Ok(())
}
