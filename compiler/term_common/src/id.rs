use std::fmt::Debug;

pub trait Identifiable {
    type Id: Debug + Copy + Eq + Ord;
    fn id(&self) -> Self::Id;
}

#[macro_export]
macro_rules! impl_identifiable {
    ($name:ident, $id_ty:ty, $($field:ident).+) => {
        impl $crate::id::Identifiable for $name {
            type Id = $id_ty;

            fn id(&self) -> Self::Id {
                self.$($field).+
            }
        }
    };
    ($name:ident, $id_ty:ty, $func:expr) => {
        impl $crate::id::Identifiable for $name {
            type Id = $id_ty;

            fn id(&self) -> Self::Id {
                let f: fn(&Self) -> Self::Id = $func;
                $func(self)
            }
        }
    };
}

#[macro_export]
macro_rules! declare_id {
    ($name:ident) => {
        declare_id!($name, usize);
    };
    ($name:ident, $id_ty:ty) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub struct $name {
            pub raw: $id_ty,
        }

        impl $name {
            pub const INVALID: Self = Self { raw: <$id_ty>::MAX };

            pub fn new(raw: $id_ty) -> Self {
                Self { raw }
            }

            pub fn is_invalid(&self) -> bool {
                self.raw == <$id_ty>::MAX
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.raw.fmt(f)
            }
        }
    };
}
pub(crate) use declare_id;

#[macro_export]
macro_rules! declare_child_id {
    ($name:ident, $t_ty:ty) => {
        declare_child_id!($name, usize, $t_ty);
    };
    ($name:ident, $id_ty:ty, $p_ty:ty) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct $name {
            pub raw: $id_ty,
            pub parent: $p_ty,
        }

        impl $name {
            pub fn new(raw: $id_ty, parent: $p_ty) -> Self {
                Self { raw, parent }
            }
        }

        impl std::cmp::PartialOrd for $name {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                self.raw.partial_cmp(&other.raw)
            }
        }

        impl std::cmp::Ord for $name {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                self.raw.cmp(&other.raw)
            }
        }

        impl std::hash::Hash for $name {
            fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
                self.raw.hash(state);
            }
        }

        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                self.raw.fmt(f)
            }
        }
    };
}

#[macro_export(local_inner_macros)]
macro_rules! declare_union_id {
    ($name:ident { $($rest:tt)* }) => {
        declare_union_id!{
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
            $name { $($rest)* }
        }
    };
    (#[$meta:meta] $name:ident { $($rest:tt)* }) => {
        declare_union_id!(@__enum $meta $name { $($rest)* } -> ());

        impl $name {
            declare_union_id!(@__func { $($rest)* } );
        }

        declare_union_id!(@__trait $name { $($rest)* } );
    };
    // enum definition
    (@__enum $meta:meta $name:ident { } -> ($($result:tt)*)) => {
        #[$meta]
        pub enum $name {
            $($result)*
        }
    };
    (@__enum $meta:meta $name:ident { $case:ident($id:ident) $(,$($rest:tt)*)? } -> ($($result:tt)*)) => {
        paste!{
            declare_union_id!{@__enum $meta $name { $($($rest)*)? } -> (
                $($result)*
                $case($id),
            )}
        }
    };
    // trait implementations
    (@__trait $name:ident { }) => {};
    (@__trait $name:ident { $case:ident($id:ident), $($rest:tt)* }) => {paste!{
        impl From<$id> for $name {
            fn from(id: $id) -> Self {
                Self::$case(id)
            }
        }

        declare_union_id!(@__trait $name { $($rest)* });
    }};
    // function implementations
    (@__func { }) => {};
    (@__func { $case:ident($id:ident), $($rest:tt)* }) => {paste!{
        pub fn [< is_ $case:snake >](&self) -> bool {
            std::matches!(self, Self::$case(_))
        }
        pub fn [< as_ $case:snake >](&self) -> Option<$id> {
            match self {
                Self::$case(id) => Some(*id),
                _ => None,
            }
        }
        pub fn [< $id:snake >](&self) -> $id {
            match self {
                Self::$case(id) => *id,
                _ => std::panic!("expected {} but found {:?}", std::stringify!($case), self),
            }
        }
        declare_union_id!(@__func { $($rest)* });
    }};
    // fmt display
    (@__display { } -> ($($result:tt)*)) => {
        impl std::fmt::Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $($result)*
                }
            }
        }
    };
    (@__display { $case:ident($id:ident), $($rest:tt)* } -> ($($result:tt)*)) => {paste!{
        declare_union_id!(@__display { $($rest)* } -> (
            $($result)*
            Self::$case(id) => id.fmt(f),
        ));
    }}
}

/// Declares a struct which represents a collection of incrementable ids.
#[macro_export(local_inner_macros)]
macro_rules! declare_id_collection {
    ($vis:vis $name:ident { $($rest:tt)* }) => {
        declare_id_collection!(@__struct $vis $name { $($rest)* } -> ());

        impl $name {
            pub fn new() -> Self {
                Self::default()
            }

            declare_id_collection!(@__func { $($rest)* } );
        }
    };
    // struct definition
    (@__struct $vis:vis $name:ident { } -> ($($result:tt)*)) => {
        #[derive(Clone, Debug, Default)]
        $vis struct $name {
            $($result)*
        }
    };
    (@__struct $vis:vis $name:ident { $id:ident $(($($_:tt)+))? $(,$($rest:tt)*)? } -> ($($result:tt)*)) => {
        paste!{
            declare_id_collection!{@__struct $vis $name { $($($rest)*)? } -> (
                $($result)*
                [< raw_ $id:snake >]: usize,
            )}
        }
    };
    // function implementations
    (@__func { }) => {};
    (@__func { $id:ident, $($rest:tt)* }) => {paste!{
        // id
        pub fn [< next_ $id:snake >](&mut self) -> $id {
            let n = self.[< raw_ $id:snake >];
            self.[< raw_ $id:snake >] += 1;
            <$id>::new(n)
        }
        declare_id_collection!(@__func { $($rest)* });
    }};
    (@__func { $id:ident($p_id:ident), $($rest:tt)* }) => {paste!{
        // child id
        pub fn [< next_ $id:snake >](&mut self, parent: $p_id) -> $id {
            let n = self.[< raw_ $id:snake >];
            self.[< raw_ $id:snake >] += 1;
            <$id>::new(n, parent)
        }
        declare_id_collection!(@__func { $($rest)* });
    }};
}
