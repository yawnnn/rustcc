#![allow(dead_code)]
#![allow(unused_imports)]
#![allow(unused_macros)]

use std::{fmt, ops};

#[derive(Clone, Copy)]
pub struct IndexKey(usize);

impl fmt::Debug for IndexKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Default)]
pub struct IndexList<T>(Vec<T>);

impl<T> IndexList<T> {
    pub fn new() -> Self {
        IndexList(Vec::new())
    }

    pub fn push(&mut self, value: T) -> IndexKey {
        let key = self.0.len();
        self.0.push(value);

        IndexKey(key)
    }

    pub fn get(&self, key: IndexKey) -> &T {
        self.0.get(key.0).unwrap()
    }

    pub fn get_mut(&mut self, key: IndexKey) -> &mut T {
        self.0.get_mut(key.0).unwrap()
    }

    pub fn find<F: Fn(&T) -> bool>(&self, f: F) -> Option<IndexKey> {
        self.0.iter().position(f).map(IndexKey)
    }
}

impl<T> ops::Deref for IndexList<T> {
    type Target = Vec<T>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T: fmt::Debug> fmt::Debug for IndexList<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#?}", self.0.iter().enumerate().collect::<Vec<_>>())
    }
}

/// current function's name (without namespace)
macro_rules! fnname {
    () => {{
        fn z() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }

        let name = type_name_of(z);
        let name = &name[..name.len() - "::z".len()];
        match name.rfind(':') {
            Some(pos) => &name[pos + 1..],
            None => name,
        }
    }};
}

pub(crate) use fnname;

/// eprintln!() with prefix of "`fnname!()`: "
macro_rules! eprintfn {
    ($($arg:tt)*) => {{
        eprint!("{}: ", fnname!());
        eprintln!($($arg)*);
    }};
}

pub(crate) use eprintfn;
