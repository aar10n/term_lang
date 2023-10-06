#![feature(let_chains)]
#![feature(try_trait_v2)]
#![feature(trait_alias)]
pub mod diagnostic;
pub mod report;

pub use diagnostic::*;
pub use report::*;

use std::result;
use term_common::span::{Span, Spanned};

pub type Result<T> = result::Result<T, Diagnostic>;

/// A trait for types that can be converted into a diagnostic.
pub trait IntoDiagnostic {
    fn into_diagnostic(self) -> Diagnostic;
}

impl IntoDiagnostic for Diagnostic {
    fn into_diagnostic(self) -> Diagnostic {
        self
    }
}

impl IntoDiagnostic for &str {
    fn into_diagnostic(self) -> Diagnostic {
        Diagnostic::error(self.to_string(), Span::default())
    }
}

//

pub trait IntoError<T> {
    fn into_err(self) -> Result<T>;
}

impl<T, E: IntoDiagnostic> IntoError<T> for E {
    fn into_err(self) -> Result<T> {
        Err(self.into_diagnostic())
    }
}

//

pub fn error_for<T: Spanned, E: AsRef<str>>(spanned: &T, err: E) -> Diagnostic {
    err.as_ref().into_diagnostic().with_span(spanned.span())
}
