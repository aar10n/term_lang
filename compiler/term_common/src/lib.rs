#![feature(let_chains)]
#![feature(const_trait_impl)]
pub mod ansi;
#[macro_use]
pub mod id;
pub mod dep_graph;
pub mod ordered_set;
pub mod source;
pub mod span;

use std::cell::RefCell;
use std::ops::Deref;
use std::rc::Rc;

pub use paste::paste;

pub type P<T> = Box<T>;

#[derive(Debug, Clone)]
pub struct RcRef<T>(Rc<RefCell<T>>);

impl<T> RcRef<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }
}

impl<T> Deref for RcRef<T> {
    type Target = RefCell<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
