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

    ctx.declare_builtin("panic");
    ctx
}

pub fn evaluate(ctx: &mut Context, source_id: SourceId, repl: bool) -> Result<(), Report> {
    let mut ctx = ast::Context::new(ctx);
    let file = ctx.sources.get(source_id).unwrap();
    let mut module = parse::parse_source(file).map_err(|e| Report::from(e.into_diagnostic()))?;

    let full = compose![
        pass::collect,
        pass::resolve,
        pass::lower_decls,
        pass::lower_exprs
    ];
    if let Err(errs) = pass::full_pass(&mut ctx, &mut module, full) {
        return Err(Report::from(errs));
    }

    module.print_stdout(&ctx);
    ctx.print_stdout(&());
    println!("Done");
    Ok(())
}

fn parse_expr(src: impl AsRef<str>) -> core::Expr {
    let mut ctx = Context::new();
    parse_lower_unwrap(&mut ctx, src, parse::parse_expr)
}

fn parse_ty(src: impl AsRef<str>) -> core::TyE {
    let mut ctx = Context::new();
    parse_lower_unwrap(&mut ctx, src, parse::parse_ty)
}

fn parse_lower_unwrap<'a, T, E>(
    ctx: &'a mut Context,
    src: impl AsRef<str>,
    parse_fn: fn(&SourceFile) -> Result<T, E>,
) -> T::Target
where
    T: Lower + PrettyPrint<ast::Context<'a>, usize>,
    E: IntoDiagnostic,
{
    let sources = SourceMap::from(src.as_ref());
    let file = sources.first().unwrap();
    let result = match parse_fn(file) {
        Ok(o) => o,
        Err(e) => {
            println!("Failed to parse: {:?}", src.as_ref());
            Report::from(e).print_stderr(&sources).unwrap();
            std::process::exit(1);
        }
    };

    let mut ctx = ast::Context::new(ctx);
    println!("{}", result.pretty_string(&ctx));
    match result.lower(&mut ctx) {
        Ok(o) => o,
        Err(e) => {
            println!("Failed to lower: {:?}", src.as_ref());
            Report::from(e).print_stderr(&sources).unwrap();
            std::process::exit(1);
        }
    }
}
