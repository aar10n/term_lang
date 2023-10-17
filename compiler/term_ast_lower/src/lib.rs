#![allow(unused)]
#![feature(trait_alias)]
#![feature(box_patterns)]
mod lower;

pub use lower::Lower;

use term_ast as ast;
use term_core as core;

pub struct Context<'ctx> {
    pub ast: &'ctx mut ast::Context,
    pub core: &'ctx mut core::Context,
}

impl<'ctx> From<(&'ctx mut ast::Context, &'ctx mut core::Context)> for Context<'ctx> {
    fn from((ast, core): (&'ctx mut ast::Context, &'ctx mut core::Context)) -> Self {
        Self { ast, core }
    }
}
