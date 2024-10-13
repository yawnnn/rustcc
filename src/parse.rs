use std::iter::Peekable;

use crate::lex::*;

#[derive(Debug)]
pub enum UnOpKind {
    Negation,
    BitwiseNot,
    LogicalNot,
}

impl UnOpKind {
    fn try_from(kind: &TokenKind) -> Option<Self> {
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
    fn try_from(kind: &TokenKind) -> Option<Self> {
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
pub enum Expression {
    Constant(i64),
    UnOp(UnOpKind),
    BinOp(BinOpKind),
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

#[derive(Debug, Clone, Copy)]
pub struct AstKey(usize);

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

impl Ast {
    fn new() -> Ast {
        Ast { data: Vec::new() }
    }

    fn root(&self) -> AstKey {
        AstKey(0)
    }

    fn insert(&mut self, parent: AstKey, data: AstData) -> AstKey {
        let key = AstKey(self.data.len());
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
                token_or_none => return token_or_none,
            }
        }
    }

    // fn peek_token(&self) -> Option<Token> {
    //     self.clone().next()
    // }
}

/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    // the expression is a single integer (e.g. ~4)
    // the expression is wrapped in parentheses (e.g. ~(1+1)), or
    // the expression is itself a unary operation (e.g. ~!8, -~(2+2)).
    todo!()
}

/// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    todo!()
}

/// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let token = cursor.next_token()?;

    match token {
        Token { kind: TokenKind::Literal, value } => {
            let const_exp = Expression::Constant(value.parse::<i64>().ok()?);
            let exp = AstData::Exp(const_exp);
            Some(ast.insert(parent, exp))
        }
        Token { kind, .. } => {
            let mut exp = None;

            if let Some(unopkind) = UnOpKind::try_from(&kind) {
                let unop_exp = Expression::UnOp(unopkind);
                exp = Some(AstData::Exp(unop_exp));
            } else if let Some(binopkind) = BinOpKind::try_from(&kind) {
                let binop_exp = Expression::BinOp(binopkind);
                exp = Some(AstData::Exp(binop_exp))
            }

            let kexp = ast.insert(parent, exp?);
            let ksubexp = parse_expression(ast, cursor, kexp)?;
            ast.get_mut(kexp).children.push(ksubexp);
            Some(kexp)
        }
    }
}

/// <statement> ::= "return" <exp> ";"
fn parse_statement(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement());
    let kstmt = ast.insert(parent, stmt);

    let token = cursor.next_token()?;
    if !matches!(token.value, "return") {
        return None;
    }

    let kexp = parse_expression(ast, cursor, kstmt)?;

    if !matches!(cursor.next_token()?.kind, TokenKind::Semicolon) {
        return None;
    }

    ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(ast: &mut Ast, cursor: &mut Cursor, parent: AstKey) -> Option<AstKey> {
    let token = cursor.next_token()?;
    if !matches!(token.value, "int") {
        return None;
    }

    let token = cursor.next_token()?;
    let name = match token {
        Token { kind: TokenKind::Ident, value } => value.to_owned(),
        _ => return None,
    };

    let func = AstData::Func(Function(name));
    let kfunc = ast.insert(parent, func);

    if !matches!(cursor.next_token()?.kind, TokenKind::OpenParen) {
        return None;
    }

    if !matches!(cursor.next_token()?.kind, TokenKind::CloseParen) {
        return None;
    }

    if !matches!(cursor.next_token()?.kind, TokenKind::OpenBrace) {
        return None;
    }

    let kstmt = parse_statement(ast, cursor, kfunc)?;

    if !matches!(cursor.next_token()?.kind, TokenKind::CloseBrace) {
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

    let funck = parse_function(ast, cursor, kprog)?;

    ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(tokens: Vec<Token>) -> Option<Ast> {
    let mut ast = Ast::new();
    let mut cursor = Cursor::new(tokens);

    parse_program(&mut ast, &mut cursor)?;

    #[cfg(debug_assertions)]
    println!("ast: \n{:?}\n", &ast);

    Some(ast)
}
