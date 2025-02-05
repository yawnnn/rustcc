#![allow(dead_code)]

use crate::common::*;
use crate::parse::*;

#[derive(Debug, Default)]
struct LocalVar {
    name: String,
}

impl LocalVar {
    fn new(name: &str) -> Self {
        Self { name: name.to_owned() }
    }
}

#[derive(Default)]
struct Scope {
    ident: String,
    vars: IndexList<LocalVar>,
}

impl Scope {
    fn new(ident: &str) -> Self {
        Self { ident: ident.to_owned(), vars: IndexList::new() }
    }
}

struct Module {
    scope_list: IndexList<Scope>,
}

struct State {
    module: Module,
    scope_stack: Vec<IndexKey>,
}

impl State {
    fn new_scope(&mut self, ident: &str) -> IndexKey {
        let key = self.module.scope_list.push(Scope::new(ident));
        self.scope_stack.push(key);

        key
    }

    fn current_scope(&self) -> Option<IndexKey> {
        self.scope_stack.last().copied()
    }

    fn new_local(&mut self, ident: &str) -> IndexKey {
        let key = self.scope_stack.last().unwrap();
        let scope = self.module.scope_list.get_mut(*key);

        scope.vars.push(LocalVar::new(ident))
    }

    fn find_local(&self, ident: &str) -> Option<IndexKey> {
        let current_scope = self.current_scope().unwrap();
        let scope = self.module.scope_list.get(current_scope);

        scope.vars.find(|lv| lv.name == ident)
    }
}

fn check(_ast: &Ast) -> Module {
    todo!()
}
