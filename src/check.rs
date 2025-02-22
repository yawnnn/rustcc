#![allow(dead_code)]

use crate::common::*;
use crate::parse::*;

pub enum Literal {
    I32(i32),
}

pub struct StackRef {
    pub pos: usize,
    pub size: usize,
}

impl StackRef {
    fn new(key: IndexKey, size: usize) -> Self {
        StackRef { pos: key.inner(), size }
    }

    pub fn offset(&self) -> usize {
        (self.pos + 1) * self.size
    }
}

pub enum Exp {
    BinOp(BinOpKind, Box<Exp>, Box<Exp>),
    UnOp(UnOpKind, Box<Exp>),
    Var(StackRef),
    Assignment(StackRef, Box<Exp>),
    Literal(Literal),
}

pub enum Stmt {
    Return(Exp),
    Decl(StackRef, Option<Exp>),
    Exp(Exp),
}

pub enum IRNode<'a> {
    Func(&'a str, Vec<Stmt>),
}

/// Intermediate representation
pub type IR<'a> = Vec<IRNode<'a>>;

#[derive(Debug, Default)]
pub struct ScopeVar {
    pub name: String,
    pub size: usize,
}

impl ScopeVar {
    fn new(name: &str) -> Self {
        Self { name: name.to_owned(), size: 8 }
    }
}

#[derive(Default)]
pub struct Scope {
    pub name: String,
    pub vars: IndexList<ScopeVar>,
}

impl Scope {
    fn new(name: &str) -> Self {
        Self { name: name.to_owned(), vars: IndexList::new() }
    }
}

#[derive(Default)]
struct State {
    scope_list: IndexList<Scope>,
    scope_stack: Vec<IndexKey>,
}

impl State {
    fn new_scope(&mut self, name: &str) -> Option<IndexKey> {
        if self.scope_list.find(|s| s.name == name).is_some() {
            return None;
        }

        let key = self.scope_list.push(Scope::new(name));
        self.scope_stack.push(key);

        Some(key)
    }

    fn current_scope(&self) -> Option<IndexKey> {
        self.scope_stack.last().copied()
    }

    fn new_local(&mut self, name: &str) -> Option<StackRef> {
        let key = self.scope_stack.last().unwrap();
        let scope = self.scope_list.get_mut(*key);

        if scope.vars.find(|v| v.name == name).is_some() {
            return None;
        }
        let var = ScopeVar::new(name);
        let size = var.size;
        let kvar = scope.vars.push(var);

        Some(StackRef { pos: kvar.inner(), size })
    }

    fn get_local(&self, ident: &str) -> Option<StackRef> {
        let current_scope = self.current_scope().unwrap();
        let scope = self.scope_list.get(current_scope);

        scope.vars.find(|lv| lv.name == ident).map(|kvar| {
            let var = scope.vars.get(kvar);
            StackRef::new(kvar, var.size)
        })
    }
}

fn check_exp(ast: &Ast, state: &mut State, _ir: &mut IR, exp: &Expression) -> Exp {
    match exp {
        Expression::BinOp(kind, kop1, kop2) => {
            let AstData::Exp(exp1) = ast.get(*kop1) else {
                panic!();
            };
            let AstData::Exp(exp2) = ast.get(*kop2) else {
                panic!();
            };
            let exp1 = check_exp(ast, state, _ir, exp1);
            let exp2 = check_exp(ast, state, _ir, exp2);
            Exp::BinOp(*kind, Box::new(exp1), Box::new(exp2))
        }
        Expression::UnOp(kind, kop) => {
            let AstData::Exp(exp) = ast.get(*kop) else {
                panic!();
            };
            let exp = check_exp(ast, state, _ir, exp);
            Exp::UnOp(*kind, Box::new(exp))
        }
        Expression::Var { name } => {
            let var = state.get_local(name.value).unwrap();
            Exp::Var(var)
        }
        Expression::Assignment { name, value } => {
            let AstData::Exp(exp) = ast.get(*value) else {
                panic!();
            };
            let exp = check_exp(ast, state, _ir, exp);
            let var = state.get_local(name.value).unwrap();
            Exp::Assignment(var, Box::new(exp))
        }
        Expression::Literal(literal) => {
            let value = literal.value.parse::<i32>().unwrap();
            Exp::Literal(Literal::I32(value))
        }
    }
}

fn check_stmt(ast: &Ast, state: &mut State, ir: &mut IR, stmt: &Statement) -> Stmt {
    match stmt {
        Statement::Return(kexp) => {
            let AstData::Exp(exp) = ast.get(*kexp) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);
            Stmt::Return(exp)
        }
        Statement::Decl { name, value } => {
            let exp = value.and_then(|kexp| {
                let AstData::Exp(exp) = ast.get(kexp) else {
                    panic!();
                };
                Some(check_exp(ast, state, ir, exp))
            });
            let var = state.new_local(name.value).unwrap();
            Stmt::Decl(var, exp)
        }
        Statement::Exp(kexp) => {
            let AstData::Exp(exp) = ast.get(*kexp) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);
            Stmt::Exp(exp)
        }
    }
}

fn check_func<'a>(
    ast: &Ast,
    state: &mut State,
    ir: &mut IR<'a>,
    name: &'a str,
    stmts: &[IndexKey],
) {
    state.new_scope(name).unwrap();
    
    let body = stmts
        .iter()
        .map(|k| {
            let AstData::Stmt(stmt) = ast.get(*k) else {
                panic!();
            };
            check_stmt(ast, state, ir, stmt)
        })
        .collect::<Vec<_>>();

    ir.push(IRNode::Func(name, body));
}

pub fn check<'a>(ast: &'a Ast) -> IR<'a> {
    let mut state: State = Default::default();
    let mut ir: IR = Default::default();

    for (data, _) in ast.iter() {
        #[allow(clippy::single_match)]
        match data {
            AstData::Func { name, statements } => {
                check_func(ast, &mut state, &mut ir, name.value, statements);
            }
            _ => (),
        }
    }

    ir
}
