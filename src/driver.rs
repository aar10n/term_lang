use term_ast as ast;
use term_ast_pass as pass;
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_parse as parse;
use term_print as print;
use term_rewrite_csp as rewrite;
use term_solve as solve;

use ast::ItemKind;
use common::source::{SourceFile, SourceId, SourceMap};
use common::span::Span;
use core::{Context, Expr, Ty, TyE};
use diag::{IntoDiagnostic, Report};
use pass::compose;
use pass::lower::Lower;
use print::ansi::{GREEN, RESET};
use print::{PrettyPrint, PrettyString};

pub fn new_context() -> Context {
    let mut ctx = Context::new();

    ctx.register_builtin("panic");
    ctx.register_builtin("builtin_put_char");
    ctx.register_builtin("builtin_get_char");
    ctx.register_builtin("builtin_handle_default");
    ctx
}

pub fn evaluate(core: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let mut ast = ast::Context::new();
    let mut ctx = pass::Context::new(&mut ast, core);

    let file = ctx.sources.get(source_id).unwrap();
    let mut module = parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let phase1 = compose![pass::collect, pass::resolve];
    if let Err(errs) = pass::apply(&mut ctx, &mut module, phase1) {
        return Err(Report::from(errs));
    }

    module.print_stdout(&ctx.ast);

    let phase2 = compose![pass::lower_decls, pass::lower_impls, pass::lower_exprs];
    if let Err(errs) = pass::apply(&mut ctx, &mut module, phase2) {
        // ctx.print_stdout(&());
        return Err(Report::from(errs));
    }

    // ctx.print_stdout(&());

    let phase3 = compose![pass::ty_check];
    if let Err(errs) = pass::apply(&mut ctx, &mut module, phase3) {
        // ctx.print_stdout(&());
        return Err(Report::from(errs));
    }

    for item in module.items {
        if let ItemKind::Command(name, args) = item.kind {
            crate::command::eval_command(&mut ctx, name, &args)?;
        }
    }

    println!("{GREEN}Done{RESET}");
    Ok(())
}
