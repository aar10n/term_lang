use term_ast as ast;
use term_ast_pass as pass;
use term_core as core;
use term_diag as diag;
use term_parse as parse;
use term_print as print;

use core::{Ef, MonoVarId, Ty};
use diag::{IntoDiagnostic, IntoError};
use pass::Context;
use print::{PrettyPrint, PrettyString};

use ustr::Ustr;

pub fn eval_command(ctx: &mut Context<'_>, name: Ustr, args: &str) -> diag::Result<()> {
    match name.as_str() {
        "ty_mono" => {
            let raw = arg_parse_usize(args)?;
            let id = MonoVarId::new(raw);
            let ty = ctx.solve.ty_set.find_no_mut(Ty::Mono(id));
            println!("{}", ty.pretty_string(ctx));
            Ok(())
        }
        "ef_mono" => {
            let raw = arg_parse_usize(args)?;
            let id = MonoVarId::new(raw);
            let ef = ctx.solve.ef_set.find_no_mut(Ef::Mono(id));
            println!("{id} -> {}", ef.pretty_string(ctx));
            Ok(())
        }
        _ => Err(format!("unknown command `{}`", name).into_diagnostic()),
    }
}

pub fn arg_parse_usize(s: &str) -> diag::Result<usize> {
    s.parse()
        .map_err(|_| "invalid integer literal".into_diagnostic())
}
