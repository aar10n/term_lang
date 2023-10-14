use term_ast as ast;
use term_core as core;
use term_diag as diag;
use term_print as print;
use term_solve as solve;

use core::{Ef, Expr, Ty, TyE, VarId};
use print::PrettyString;

use std::collections::{BTreeSet, HashMap};
use std::ops::{Deref, DerefMut};
use ustr::Ustr;

/// Ast pass context.
#[derive(Debug)]
pub struct Context<'a> {
    /// Ast context.
    pub ast: &'a mut ast::Context,
    /// Solve context.
    pub solve: solve::Context<'a>,
}

impl<'a> Context<'a> {
    pub fn new(ast: &'a mut ast::Context, core: &'a mut core::Context) -> Self {
        Self {
            ast,
            solve: solve::Context::new(core),
        }
    }
}

impl<'a> Deref for Context<'a> {
    type Target = core::Context;

    fn deref(&self) -> &Self::Target {
        &self.solve.ctx
    }
}

impl DerefMut for Context<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.solve.ctx
    }
}

fn nth_monotype_var(mut n: usize) -> Ustr {
    let var = "t";
    let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
    // let var = "𝜏";
    // let digits = ["₀", "₁", "₂", "₃", "₄", "₅", "₆", "₇", "₈", "₉"];
    let mut s = String::new();
    while n > 0 {
        s.push_str(digits[n % 10]);
        n /= 10;
    }
    let s = s.chars().rev().collect::<String>();
    Ustr::from(&format!("{}{}", var, s))
}
