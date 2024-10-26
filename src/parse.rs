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
    fn try_from_add_sub(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Plus => Some(Self::Addition),
            TokenKind::Minus => Some(Self::Subtraction),
            _ => None,
        }
    }

    fn try_from_mul_div(kind: TokenKind) -> Option<Self> {
        match kind {
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

    fn insert(&mut self, data: AstData) -> AstKey {
        let key = AstKey(self.data.len());

        #[cfg(debug_assertions)]
        let node = AstNode { pos: key, data, children: Vec::new() };
        #[cfg(not(debug_assertions))]
        let node = AstNode { data, children: Vec::new() };

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

    fn next(&mut self) -> Option<Token> {
        loop {
            match self.0.next() {
                Some(Token { kind: TokenKind::Whitespace, .. }) => (),
                token_or_none => break token_or_none,
            }
        }
    }

    fn peek(&mut self) -> Option<Token> {
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

/// Makes matching on `TokenKind` or returning more ergonomic
macro_rules! match_token {
    ($token:expr, $kind:pat $(,)?) => {{
        let _token = $token;
        match _token.kind {
            $kind => Some(_token),
            _ => None,
        }
    }};
}

/// Makes matching on value or returning more ergonomic
macro_rules! match_value {
    ($token:expr, $value:expr $(,)?) => {{
        let _token = $token;
        match _token.value {
            $value => Some(_token),
            _ => None,
        }
    }};
}

/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let token = cursor.next()?;

    match token.kind {
        TokenKind::OpenParen => {
            let kexp = parse_expression(ast, cursor)?;

            match_token!(cursor.next()?, TokenKind::CloseParen)?;

            Some(kexp)
        }
        TokenKind::Literal => {
            let literal = Literal::Integer(token.value.parse::<i32>().ok()?);
            let literal = AstData::Exp(Expression::Literal(literal));

            Some(ast.insert(literal))
        }
        kind => {
            let unop_kind = UnOpKind::try_from(kind)?;
            let unop = AstData::Exp(Expression::UnOp(unop_kind));
            let kunop = ast.insert(unop);
            let koperand = parse_factor(ast, cursor)?;
            ast.get_mut(kunop).children.push(koperand);

            Some(kunop)
        }
    }
}

type ParseBinOperandFn = fn(&mut Ast, &mut Cursor) -> Option<AstKey>;
type BinOpTokenFn = fn(TokenKind) -> Option<BinOpKind>;

fn parse_binop(ast: &mut Ast, cursor: &mut Cursor, parse_operand: ParseBinOperandFn, binop_token: BinOpTokenFn) -> Option<AstKey> {
    let mut kbinop = parse_operand(ast, cursor)?;

    while let Some(binop_kind) = binop_token(cursor.peek()?.kind) {
        cursor.next();

        let kop2 = parse_operand(ast, cursor)?;

        let binop = AstData::Exp(Expression::BinOp(binop_kind));
        let knew_binop = ast.insert(binop);

        ast.get_mut(knew_binop).children.push(kbinop);
        ast.get_mut(knew_binop).children.push(kop2);

        kbinop = knew_binop;
    }

    Some(kbinop)
}

/// <term> ::= <factor> { ("*" | "/") <factor> }
fn parse_term(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_factor, BinOpKind::try_from_mul_div)
}

/// <exp> ::= <term> { ("+" | "-") <term> }
fn parse_expression(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_term, BinOpKind::try_from_add_sub)
}

/// <statement> ::= "return" <exp> ";"
fn parse_statement(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement::Return);
    let kstmt = ast.insert(stmt);

    match_value!(cursor.next()?, "return")?;

    let kexp = parse_expression(ast, cursor)?;

    match_token!(cursor.next()?, TokenKind::Semicolon)?;

    ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    match_value!(cursor.next()?, "int")?;

    let token = match_token!(cursor.next()?, TokenKind::Ident)?;
    let name = token.value.to_owned();

    let func = AstData::Func(Function { name });
    let kfunc = ast.insert(func);

    match_token!(cursor.next()?, TokenKind::OpenParen)?;
    match_token!(cursor.next()?, TokenKind::CloseParen)?;
    match_token!(cursor.next()?, TokenKind::OpenBrace)?;

    let kstmt = parse_statement(ast, cursor)?;

    match_token!(cursor.next()?, TokenKind::CloseBrace)?;

    ast.get_mut(kfunc).children.push(kstmt);
    Some(kfunc)
}

/// <program> ::= <function>
fn parse_program(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let prog = AstData::Prog(Program());
    let kprog = ast.insert(prog);

    let funck = parse_function(ast, cursor)?;

    ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(tokens: Vec<Token>) -> Option<Ast> {
    let mut ast = Ast::new();
    let mut cursor = Cursor::new(tokens);

    parse_program(&mut ast, &mut cursor)?;

    #[cfg(debug_assertions)]
    println!("\n### AST ###\n{ast:#?}\n");

    Some(ast)
}
