use crate::{Diagnostic, IntoDiagnostic, Level};
use term_common::ansi::{BOLD, GREEN, RED, RESET, YELLOW};
use term_common::source::SourceMap;

use std::io;

/// A report is collection of diagnostics.
#[derive(Debug, Clone)]
pub struct Report {
    /// The diagnostics that were attached to this report.
    pub(crate) diagnostics: Vec<Diagnostic>,
}

impl Report {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.diagnostics.is_empty()
    }

    pub fn render<Output: io::Write>(
        &self,
        out: &mut Output,
        source_map: &SourceMap,
    ) -> io::Result<()> {
        let (nerrs, nwarns) =
            self.diagnostics
                .iter()
                .fold((0, 0), |(nerrs, nwarns), d| match d.level {
                    Level::Error => (nerrs + 1, nwarns),
                    Level::Warning => (nerrs, nwarns + 1),
                });

        if nerrs > 0 {
            write!(out, "{BOLD}{RED}{} error(s){RESET} ", nerrs)?;
        }
        if nwarns > 0 {
            write!(out, "{YELLOW}{} warning(s){RESET}", nwarns)?;
        }

        if nerrs == 0 && nwarns == 0 {
            writeln!(out, "{BOLD}{GREEN}Ok{RESET}")?;
        } else {
            writeln!(out)?;
        }

        for diagnostic in self.diagnostics.iter() {
            diagnostic.render(out, source_map)?;
        }
        Ok(())
    }

    pub fn print_stderr(&self, source_map: &SourceMap) -> io::Result<()> {
        self.render(&mut io::stderr(), source_map)
    }
}

impl<T: IntoDiagnostic> From<T> for Report {
    fn from(value: T) -> Self {
        Self {
            diagnostics: vec![value.into_diagnostic()],
        }
    }
}

impl<T: IntoDiagnostic> From<Vec<T>> for Report {
    fn from(value: Vec<T>) -> Self {
        Self {
            diagnostics: value.into_iter().map(|d| d.into_diagnostic()).collect(),
        }
    }
}
