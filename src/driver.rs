use term_ast as ast;
use term_ast_pass as pass;
use term_common as common;
use term_core as core;
use term_diag as diag;
use term_parse as parse;
use term_print as print;
use term_solve as solve;

use common::source::{SourceFile, SourceId, SourceMap};
use common::span::Span;
use core::{Context, Expr, Ty, TyE};
use diag::{IntoDiagnostic, Report};
use pass::compose;
use pass::lower::Lower;
use print::{PrettyPrint, PrettyString};

pub fn new_context() -> Context {
    let mut ctx = Context::new();

    ctx.register_builtin("panic");
    ctx
}

pub fn evaluate(ctx: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let mut ctx = ast::Context::new(ctx);
    let file = ctx.sources.get(source_id).unwrap();
    let mut module = parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let full = compose![
        pass::collect,
        pass::resolve,
        pass::lower_decls /*pass::lower_exprs*/
    ];
    if let Err(errs) = pass::full_pass(&mut ctx, &mut module, full) {
        return Err(Report::from(errs));
    }

    module.print_stdout(&ctx);
    ctx.print_stdout(&());
    println!("Done");
    Ok(())
}
