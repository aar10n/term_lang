use crate as ast;
use term_common as common;
use term_print as print;

use ast::{Context, Expr, Ty, VarDecl};
use common::span::{Span, Spanned};
use print::PrettyPrint;

use const_format::concatcp;
use std::{
    io,
    ops::{Deref, DerefMut},
};

/// An id for an AST node.

#[derive(Clone, Debug, PartialEq)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }
}

impl<T> From<T> for Node<T> {
    fn from(kind: T) -> Self {
        Self {
            kind,
            span: Span::default(),
        }
    }
}

impl<T> Spanned for Node<T> {
    fn with_span(self, span: Span) -> Self {
        Self { span, ..self }
    }

    fn span(&self) -> Span {
        self.span
    }
}

impl<T> Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.kind
    }
}
