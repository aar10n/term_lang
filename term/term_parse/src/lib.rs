mod parser;

use term_ast::*;
use term_common::{source::SourceFile, span::Span};
use term_diag::{Diagnostic, IntoDiagnostic, Level};

pub struct SyntaxError {
    pub expected: String,
    pub span: Span,
}

impl IntoDiagnostic for SyntaxError {
    fn into_diagnostic(self) -> Diagnostic {
        println!("{:?}", self.span);
        Diagnostic::new(Level::Error, "syntax error".to_owned(), self.span)
            .with_inline_note(format!("expected {}", self.expected))
    }
}

//

macro_rules! parse_rule_impl {
    ($rule:ident, $file:expr) => {
        match parser::parser::$rule(&$file.source(), $file.id()) {
            Ok(result) => Ok(result),
            Err(err) => {
                let offset = err.location.offset;
                let expected: Vec<&str> = err.expected.tokens().collect();
                let expected = expected.join(" or ");
                let span = Span::new($file.id(), offset, offset);
                Err(SyntaxError { expected, span })
            }
        }
    };
}

pub fn parse_source(file: &SourceFile) -> Result<Module, SyntaxError> {
    parse_rule_impl!(module, file)
}

pub fn parse_ty(file: &SourceFile) -> Result<Ty, SyntaxError> {
    Ok(*parse_rule_impl!(ty, file)?)
}

pub fn parse_expr(file: &SourceFile) -> Result<Expr, SyntaxError> {
    Ok(*parse_rule_impl!(expr, file)?)
}
