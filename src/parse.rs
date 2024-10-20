use std::{fmt, iter::Peekable};

use crate::lex::*;

#[derive(Debug)]
pub enum UnOpKind {
    Negation,
    BitwiseNot,
    LogicalNot,
}

impl UnOpKind {
    fn try_from(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(Self::Negation),
            TokenKind::Tilde => Some(Self::BitwiseNot),
            TokenKind::Bang => Some(Self::LogicalNot),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum BinOpKind {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

impl BinOpKind {
    fn try_from(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Addition),
            TokenKind::Minus => Some(Self::Subtraction),
            TokenKind::Star => Some(Self::Multiplication),
            TokenKind::Slash => Some(Self::Division),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum Literal {
    Integer(i32),
}

#[derive(Debug)]
pub enum Expression {
    BinOp(BinOpKind),
    UnOp(UnOpKind),
    Literal(Literal),
}

#[derive(Debug)]
pub enum Statement {
    Return,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
}

#[derive(Debug)]
pub struct Program();

#[derive(Debug)]
pub enum AstData {
    Prog(Program),
    Func(Function),
    Stmt(Statement),
    Exp(Expression),
    Placeholder,
}

#[derive(Clone, Copy)]
pub struct AstKey(usize);

impl fmt::Debug for AstKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug)]
pub struct AstNode {
    #[cfg(debug_assertions)]
    pub pos: AstKey,

    pub data: AstData,
    pub parent: AstKey,
    pub children: Vec<AstKey>,
}

#[derive(Debug)]
pub struct Ast {
    data: Vec<AstNode>,
}

impl Ast {
    fn new() -> Ast {
        Ast { data: Vec::new() }
    }

    fn root(&self) -> AstKey {
        AstKey(0)
    }

    fn insert(&mut self, parent: AstKey, data: AstData) -> AstKey {
        let key = AstKey(self.data.len());

        #[cfg(debug_assertions)]
        let node = AstNode { pos: key, data, parent, children: Vec::new() };
        #[cfg(not(debug_assertions))]
        let node = AstNode { data, parent, children: Vec::new() };

        self.data.push(node);

        key
    }

    pub fn get(&self, key: AstKey) -> &AstNode {
        &self.data[key.0]
    }

    fn get_mut(&mut self, key: AstKey) -> &mut AstNode {
        &mut self.data[key.0]
    }

    pub fn first(&self) -> &AstNode {
        self.get(AstKey(0))
    }
}

impl<'a> IntoIterator for &'a Ast {
    type Item = &'a AstNode;
    type IntoIter = std::slice::Iter<'a, AstNode>;

    fn into_iter(self) -> Self::IntoIter {
        self.data.iter()
    }
}

/// Moves through the tokens ignoring whitespace
#[derive(Clone)]
struct Cursor<'a>(Peekable<std::vec::IntoIter<Token<'a>>>);

impl<'a> Cursor<'a> {
    fn new(tokens: Vec<Token>) -> Cursor {
        Cursor(tokens.into_iter().peekable())
    }

    fn next_token(&mut self) -> Option<Token> {
        loop {
            match self.0.next() {
                Some(Token { kind: TokenKind::Whitespace, .. }) => (),
                token_or_none => break token_or_none,
            }
        }
    }

    fn peek_token(&mut self) -> Option<Token> {
        loop {
            match self.0.peek() {
                Some(Token { kind: TokenKind::Whitespace, .. }) => {
                    self.0.next();
                }
                token_or_none => break token_or_none.copied(),
            }
        }
    }
}

/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let token = cursor.next_token().unwrap();

    match token.kind {
        TokenKind::OpenParen => {
            let kexp = parse_expression(ast, cursor, parent).unwrap();

            if !matches!(cursor.next_token().unwrap().kind, TokenKind::CloseParen) {
                return None;
            }

            Some(kexp)
        }
        TokenKind::Literal => {
            let literal = Literal::Integer(token.value.parse::<i32>().ok().unwrap());
            let literal = AstData::Exp(Expression::Literal(literal));
            let kliteral = ast.insert(parent, literal);

            Some(kliteral)
        }
        kind => {
            let unop_kind = UnOpKind::try_from(kind).unwrap();
            let unop = AstData::Exp(Expression::UnOp(unop_kind));
            let kunop = ast.insert(parent, unop);
            let koperand = parse_factor(ast, cursor, kunop).unwrap();
            ast.get_mut(kunop).children.push(koperand);

            Some(kunop)
        }
    }
}

/// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let mut kterm = None;
    let mut kop1 = parse_factor(ast, cursor, parent).unwrap();

    while let Some(token) = cursor.peek_token() {
        match BinOpKind::try_from(token.kind) {
            Some(binop_kind @ (BinOpKind::Multiplication | BinOpKind::Division)) => {
                cursor.next_token();

                let binop = AstData::Exp(Expression::BinOp(binop_kind));
                let kbinop = ast.insert(parent, binop);

                ast.get_mut(kop1).parent = kbinop;
                ast.get_mut(kbinop).children.push(kop1);

                let kop2 = parse_factor(ast, cursor, kbinop).unwrap();

                ast.get_mut(kbinop).children.push(kop2);

                if kterm.is_none() {
                    kterm = Some(kbinop);
                }
                kop1 = kop2;
            }
            _ => break,
        }
    }

    kterm.or(Some(kop1))
}

/// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let mut kexp = None;
    let mut kop1 = parse_term(ast, cursor, parent).unwrap();

    while let Some(token) = cursor.peek_token() {
        match BinOpKind::try_from(token.kind) {
            Some(binop_kind @ (BinOpKind::Addition | BinOpKind::Subtraction)) => {
                cursor.next_token();

                let binop = AstData::Exp(Expression::BinOp(binop_kind));
                let kbinop = ast.insert(parent, binop);

                ast.get_mut(kop1).parent = kbinop;
                ast.get_mut(kbinop).children.push(kop1);

                let kop2 = parse_term(ast, cursor, kbinop).unwrap();

                ast.get_mut(kbinop).children.push(kop2);

                if kexp.is_none() {
                    kexp = Some(kbinop);
                }
                kop1 = kop2;
            }
            _ => break,
        }
    }

    kexp.or(Some(kop1))
}

/// <statement> ::= "return" <exp> ";"
fn parse_statement(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement::Return);
    let kstmt = ast.insert(parent, stmt);

    let token = cursor.next_token().unwrap();
    if !matches!(token.value, "return") {
        return None;
    }

    let kexp = parse_expression(ast, cursor, kstmt).unwrap();

    if !matches!(cursor.next_token().unwrap().kind, TokenKind::Semicolon) {
        return None;
    }

    ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let token = cursor.next_token().unwrap();
    if !matches!(token.value, "int") {
        return None;
    }

    let token = cursor.next_token().unwrap();
    let name = match token {
        Token { kind: TokenKind::Ident, value } => value.to_owned(),
        _ => return None,
    };

    let func = AstData::Func(Function { name });
    let kfunc = ast.insert(parent, func);

    if !matches!(cursor.next_token().unwrap().kind, TokenKind::OpenParen) {
        return None;
    }

    if !matches!(cursor.next_token().unwrap().kind, TokenKind::CloseParen) {
        return None;
    }

    if !matches!(cursor.next_token().unwrap().kind, TokenKind::OpenBrace) {
        return None;
    }

    let kstmt = parse_statement(ast, cursor, kfunc).unwrap();

    if !matches!(cursor.next_token().unwrap().kind, TokenKind::CloseBrace) {
        return None;
    }

    ast.get_mut(kfunc).children.push(kstmt);
    Some(kfunc)
}

/// <program> ::= <function>
fn parse_program(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let root = ast.root();
    let prog = AstData::Prog(Program());
    let kprog = ast.insert(root, prog);

    let funck = parse_function(ast, cursor, kprog).unwrap();

    ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(tokens: Vec<Token>) -> Option<Ast> {
    let mut ast = Ast::new();
    let mut cursor = Cursor::new(tokens);

    parse_program(&mut ast, &mut cursor).unwrap();

    #[cfg(debug_assertions)]
    println!("\n### AST ###\n{ast:#?}\n");

    Some(ast)
}
