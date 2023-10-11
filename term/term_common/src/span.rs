pub use crate::source::SourceId;

/// A span in a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub source_id: SourceId,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(source_id: SourceId, start: usize, end: usize) -> Self {
        Self {
            source_id,
            start,
            end,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn is_invalid(&self) -> bool {
        self.source_id == SourceId::INVALID
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn from_end(&self) -> Self {
        Self {
            source_id: self.source_id,
            start: self.end,
            end: self.end,
        }
    }

    pub fn hash(&self) -> u64 {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let mut hasher = DefaultHasher::new();
        Hash::hash(&self, &mut hasher);
        hasher.finish()
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            source_id: SourceId::INVALID,
            start: 0,
            end: 0,
        }
    }
}

/// A trait for spannable nodes.   
pub trait Spanned: Sized {
    fn with_span(self, span: Span) -> Self;
    fn span(&self) -> Span;
}

#[macro_export]
macro_rules! impl_spanned {
    ($ty:ty) => {
        impl $crate::span::Spanned for $ty {
            fn with_span(self, span: $crate::span::Span) -> Self {
                Self { span, ..self }
            }

            fn span(&self) -> $crate::span::Span {
                self.span
            }
        }
    };
}
