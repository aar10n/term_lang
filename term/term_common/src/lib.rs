#![feature(const_trait_impl)]
pub mod ansi;
#[macro_use]
pub mod id;
pub mod source;
pub mod span;

pub type P<T> = Box<T>;

pub use paste::paste;
