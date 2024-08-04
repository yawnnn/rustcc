use core::slice::Iter;

use crate::lex::*;

// #[derive(Debug)]
// pub enum ArithmeticOp {
//     Minus,
// }

// #[derive(Debug)]
// pub enum LogicalOp {
//     Not,
// }

// #[derive(Debug)]
// pub enum BitwiseOp {
//     Not,
// }

// #[derive(Debug)]
// pub enum UnOpKind {
//     Arithmetic(ArithmeticOp),
//     Logical(LogicalOp),
//     Bitwise(BitwiseOp),
// }

#[derive(Debug)]
pub enum UnOpKind {
    Minus,
    BitwiseNot,
    LogicalNot,
}

impl UnOpKind {
    fn try_from(kind: &TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(UnOpKind::Minus),
            TokenKind::Tilde => Some(UnOpKind::BitwiseNot),
            TokenKind::Bang => Some(UnOpKind::LogicalNot),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Expression {
    Constant(i64),
    UnOp(UnOpKind),
}

#[derive(Debug)]
pub struct Statement();

#[derive(Debug)]
pub struct Function(pub String);

#[derive(Debug)]
pub struct Program();

#[derive(Debug)]
pub enum AstData {
    Exp(Expression),
    Stmt(Statement),
    Func(Function),
    Prog(Program),
}

pub type AstKey = usize;

#[derive(Debug)]
pub struct AstNode {
    pub data: AstData,
    pub parent: AstKey,
    pub children: Vec<AstKey>,
}

#[derive(Debug)]
pub struct Ast {
    data: Vec<AstNode>,
}

impl<'a> IntoIterator for &'a Ast {
    type Item = &'a AstNode;
    type IntoIter = std::slice::Iter<'a, AstNode>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

impl Ast {
    fn new() -> Ast {
        Ast { data: vec![] }
    }

    fn insert(&mut self, parent: AstKey, data: AstData) -> AstKey {
        self.data.push(AstNode { data, parent, children: vec![] });

        self.data.len() - 1
    }

    pub fn get(&self, key: AstKey) -> &AstNode {
        &self.data[key]
    }

    fn get_mut(&mut self, key: AstKey) -> &mut AstNode {
        &mut self.data[key]
    }
}

fn parse_expression(tokens: &mut Iter<Token>, ast: &mut Ast, parent: AstKey) -> Option<AstKey> {
    let token = tokens.next()?;
    match token {
        Token { kind: TokenKind::Literal, value } => {
            let exp = AstData::Exp(Expression::Constant(value.parse::<i64>().ok()?));
            Some(ast.insert(parent, exp))
        }
        Token { kind, .. } => {
            let unopkind = UnOpKind::try_from(kind)?;
            let exp = AstData::Exp(Expression::UnOp(unopkind));
            let kexp = ast.insert(parent, exp);
            let ksubexp = parse_expression(tokens, ast, kexp)?;
            ast.get_mut(kexp).children.push(ksubexp);
            Some(kexp)
        }
    }
}

fn parse_statement(tokens: &mut Iter<Token>, ast: &mut Ast, parent: AstKey) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement());
    let kstmt = ast.insert(parent, stmt);

    if !matches!(tokens.next()?.kind, TokenKind::Keyword(KeywordKind::Return)) {
        return None;
    }

    let kexp = parse_expression(tokens, ast, kstmt)?;

    if !matches!(tokens.next()?.kind, TokenKind::Semicolon) {
        return None;
    }

    ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

fn parse_function(tokens: &mut Iter<Token>, ast: &mut Ast, parent: AstKey) -> Option<AstKey> {
    if !matches!(tokens.next()?.kind, TokenKind::Keyword(KeywordKind::Int)) {
        return None;
    }

    let name = match tokens.next()? {
        Token { kind: TokenKind::Ident, value } => value.clone(),
        _ => return None,
    };

    let func = AstData::Func(Function(name));
    let kfunc = ast.insert(parent, func);

    if !matches!(tokens.next()?.kind, TokenKind::OpenParen) {
        return None;
    }

    if !matches!(tokens.next()?.kind, TokenKind::CloseParen) {
        return None;
    }

    if !matches!(tokens.next()?.kind, TokenKind::OpenBrace) {
        return None;
    }

    let kstmt = parse_statement(tokens, ast, kfunc)?;

    if !matches!(tokens.next()?.kind, TokenKind::CloseBrace) {
        return None;
    }

    ast.get_mut(kfunc).children.push(kstmt);
    Some(kfunc)
}

fn parse_program(tokens: &mut Iter<Token>, ast: &mut Ast, parent: AstKey) -> Option<AstKey> {
    let prog = AstData::Prog(Program());
    let kprog = ast.insert(parent, prog);

    let funck = parse_function(tokens, ast, kprog)?;

    ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(input: Vec<Token>) -> Option<Ast> {
    let mut ast = Ast::new();
    let mut tokens = input.iter();
    parse_program(&mut tokens, &mut ast, 0)?;

    //println!("ast: \n{ast:?}\n");

    Some(ast)
}
