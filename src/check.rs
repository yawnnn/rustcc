#![allow(unused)]

use std::ops::ControlFlow;

use crate::parse::{self, Ast, AstVisitor};

pub struct StackRef {
    pub pos: usize,
    pub size: usize,
}

impl StackRef {
    fn new(pos: usize, size: usize) -> Self {
        StackRef { pos, size }
    }

    pub fn offset(&self) -> usize {
        (self.pos + 1) * self.size
    }
}

pub struct Var {
    pub name: String,
    pub size: usize,
}

impl Var {
    fn new(name: &str) -> Self {
        Self { name: name.to_owned(), size: 8 }
    }
}

pub struct Scope {
    pub name: String,
    pub vars: Vec<Var>,
}

impl Scope {
    fn new(name: &str) -> Self {
        Self { name: name.to_owned(), vars: Vec::new() }
    }
}

#[derive(Default)]
pub struct Ctx {
    scopes: Vec<Scope>,
    scope_stack: Vec<usize>,
}

impl Ctx {
    fn new_scope(&mut self, name: &str)  {
        if self.scopes.iter().any(|s| s.name == name) {
            panic!("Scope exists already");
        }

        self.scopes.push(Scope::new(name));
        self.scope_stack.push(self.scopes.len() - 1);
    }

    fn current_scope(&self) -> Option<usize> {
        self.scope_stack.last().copied()
    }

    fn new_var(&mut self, name: &str) {
        let key = self.scope_stack.last().unwrap();
        let scope = self.scopes.get_mut(*key).unwrap();

        if scope.vars.iter().any(|v| v.name == name) {
            panic!("Local already exists");
        }

        scope.vars.push(Var::new(name));
    }

    pub fn get_var(&self, ident: &str) -> Option<StackRef> {
        let current_scope = self.current_scope().unwrap();
        let scope = self.scopes.get(current_scope).unwrap();

        scope.vars.iter().position(|lv| lv.name == ident).map(|kvar| {
            let var = scope.vars.get(kvar).unwrap();
            StackRef::new(kvar, var.size)
        })
    }
}

impl AstVisitor for Ctx {
    fn visit_decl(&mut self, decl: &parse::Decl) -> ControlFlow<(), ()> { 
        self.new_var(decl.name.value);

        ControlFlow::Continue(())
    }

    fn visit_fn_def(&mut self, fn_def: &parse::FnDef) -> ControlFlow<(), ()> {
        self.new_scope(fn_def.name.value);
        
        ControlFlow::Continue(())
    }
}

pub fn check(ast: &Ast) -> Ctx {
    let mut ctx = Ctx::default();

    ctx.walk_ast(ast);

    ctx
}