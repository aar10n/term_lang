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

use std::collections::BTreeMap;

pub fn new_context() -> Context {
    let mut ctx = Context::new();
    ctx.register_builtin_type("Bool");

    ctx.register_builtin_type("Char");
    ctx.register_builtin_name("builtin_char_put");
    ctx.register_builtin_name("builtin_char_get");

    ctx.register_builtin_type("Int");
    ctx.register_builtin_name("builtin_int_add");
    ctx.register_builtin_name("builtin_int_sub");

    ctx.register_builtin_type("Double");
    ctx.register_builtin_name("builtin_double_add");
    ctx.register_builtin_name("builtin_double_sub");
    ctx.register_builtin_name("builtin_double_div");
    ctx.register_builtin_name("builtin_double_from_int");

    ctx.register_builtin_name("panic");
    ctx
}

pub fn evaluate(ctx: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let file = ctx.sources.get(source_id).unwrap();
    let module = &mut parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let ast = &mut ast::Context::new();
    pass::collect(ast, ctx, module).into_result()?;
    pass::resolve(ast, ctx, module).into_result()?;
    pass::lower_all(ast, ctx, module).into_result()?;
    pass::infer_all(ast, ctx, module).into_result()?;
    // ctx.print_stdout(&());

    println!("{GREEN}Done{RESET}");
    Ok(())
}
