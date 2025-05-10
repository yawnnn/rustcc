#![allow(dead_code)]

use crate::common::*;
use crate::parse;
use crate::parse::{BinOpKind, UnOpKind};

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
    Ternary(Box<Exp>, Box<Exp>, Box<Exp>),
}

pub enum Stmt {
    Return(Exp),
    Exp(Exp),
    If(Exp, Box<Stmt>, Option<Box<Stmt>>),
}

pub struct Decl(pub StackRef, pub Option<Exp>);

pub enum BlockItem {
    Delc(Decl),
    Stmt(Stmt),
}

pub enum IRNode<'a> {
    Func(&'a str, Vec<BlockItem>),
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

fn check_exp(ast: &parse::Ast, state: &mut State, ir: &mut IR, exp: &parse::Expression) -> Exp {
    match *exp {
        parse::Expression::BinOp { kind, op1, op2 } => {
            let parse::AstData::Exp(exp1) = ast.get(op1) else {
                panic!();
            };
            let parse::AstData::Exp(exp2) = ast.get(op2) else {
                panic!();
            };
            let exp1 = check_exp(ast, state, ir, exp1);
            let exp2 = check_exp(ast, state, ir, exp2);
            Exp::BinOp(kind, Box::new(exp1), Box::new(exp2))
        }
        parse::Expression::UnOp { kind, op } => {
            let parse::AstData::Exp(exp) = ast.get(op) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);
            Exp::UnOp(kind, Box::new(exp))
        }
        parse::Expression::Var(name)=> {
            let var = state.get_local(name.value).unwrap();
            Exp::Var(var)
        }
        parse::Expression::Assignment { name, value } => {
            let parse::AstData::Exp(exp) = ast.get(value) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);
            let var = state.get_local(name.value).unwrap();
            Exp::Assignment(var, Box::new(exp))
        }
        parse::Expression::Literal(literal) => {
            let value = literal.value.parse::<i32>().unwrap();
            Exp::Literal(Literal::I32(value))
        }
        parse::Expression::Ternary { cond, ifb, elseb } => {
            let parse::AstData::Exp(cond) = ast.get(cond) else {
                panic!();
            };
            let cond = check_exp(ast, state, ir, cond);

            let parse::AstData::Exp(ifb) = ast.get(ifb) else {
                panic!();
            };
            let ifb = check_exp(ast, state, ir, ifb);

            let parse::AstData::Exp(elseb) = ast.get(elseb) else {
                panic!();
            };
            let elseb = check_exp(ast, state, ir, elseb);

            Exp::Ternary(Box::new(cond), Box::new(ifb), Box::new(elseb))
        }
    }
}

fn check_stmt(ast: &parse::Ast, state: &mut State, ir: &mut IR, stmt: &parse::Statement) -> Stmt {
    match stmt {
        parse::Statement::Return(kexp) => {
            let parse::AstData::Exp(exp) = ast.get(*kexp) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);

            Stmt::Return(exp)
        }
        parse::Statement::Exp(kexp) => {
            let parse::AstData::Exp(exp) = ast.get(*kexp) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);

            Stmt::Exp(exp)
        }
        parse::Statement::If { cond, ifb, elseb } => {
            let parse::AstData::Exp(exp) = ast.get(*cond) else {
                panic!();
            };
            let exp = check_exp(ast, state, ir, exp);

            let parse::AstData::BlockItem(parse::BlockItem::Stmt(ifb)) = ast.get(*ifb) else {
                panic!();
            };
            let ifb = Box::new(check_stmt(ast, state, ir, ifb));

            let elseb = elseb.map(|elseb| {
                let parse::AstData::BlockItem(parse::BlockItem::Stmt(elseb)) = ast.get(elseb)
                else {
                    panic!();
                };

                Box::new(check_stmt(ast, state, ir, elseb))
            });

            Stmt::If(exp, ifb, elseb)
        }
    }
}

fn check_decl(ast: &parse::Ast, state: &mut State, ir: &mut IR, decl: &parse::Decl) -> Decl {
    let exp = decl.value.and_then(|kexp| {
        let parse::AstData::Exp(exp) = ast.get(kexp) else {
            panic!();
        };

        Some(check_exp(ast, state, ir, exp))
    });
    let var = state.new_local(decl.name.value).unwrap();

    Decl(var, exp)
}

fn check_block_item(
    ast: &parse::Ast,
    state: &mut State,
    ir: &mut IR,
    block_item: &parse::BlockItem,
) -> BlockItem {
    match block_item {
        parse::BlockItem::Decl(decl) => BlockItem::Delc(check_decl(ast, state, ir, decl)),
        parse::BlockItem::Stmt(stmt) => BlockItem::Stmt(check_stmt(ast, state, ir, stmt)),
    }
}

fn check_func<'a>(
    ast: &parse::Ast,
    state: &mut State,
    ir: &mut IR<'a>,
    name: &'a str,
    block_items: &[IndexKey],
) {
    state.new_scope(name).unwrap();

    let body = block_items
        .iter()
        .map(|k| {
            let parse::AstData::BlockItem(block_item) = ast.get(*k) else {
                panic!();
            };
            check_block_item(ast, state, ir, block_item)
        })
        .collect::<Vec<_>>();

    ir.push(IRNode::Func(name, body));
}

pub fn check<'a>(ast: &'a parse::Ast) -> IR<'a> {
    let mut state: State = Default::default();
    let mut ir: IR = Default::default();

    for (data, _) in ast.iter() {
        #[allow(clippy::single_match)]
        match data {
            parse::AstData::Func { name, block_items } => {
                check_func(ast, &mut state, &mut ir, name.value, block_items);
            }
            _ => (),
        }
    }

    ir
}
