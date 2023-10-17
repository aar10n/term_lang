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

    let mut pass1 = pass::compose![pass::collect, pass::resolve];
    pass1(ast, core, module).into_result()?;

    module.print_stdout(&ast);

    let mut pass2 = pass::compose![pass::lower_decls, pass::lower_impls, pass::lower_exprs];
    pass2(ast, core, module).into_result()?;

    let deps = pass::solve_deps(ast, core, module).into_result()?;
    for (id, deps) in deps {
        println!("{} -> {:?}", id.pretty_string(core), deps);
    }

    // for item in module.items {
    //     if let ItemKind::Command(name, args) = item.kind {
    //         crate::command::eval_command(&mut ctx, name, &args)?;
    //     }
    // }

    println!("{GREEN}Done{RESET}");
    Ok(())
}
