use std::fmt;

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
    LogicalAnd,
    LogicalOr,
    LogicalEq,
    LogicalNotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
}

impl BinOpKind {
    fn logical_or_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;
        let second = cursor.next()?.kind;

        match (first, second) {
            (TokenKind::Or, TokenKind::Or) => Some(Self::LogicalOr),
            _ => None,
        }
    }

    fn logical_and_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;
        let second = cursor.next()?.kind;

        match (first, second) {
            (TokenKind::And, TokenKind::And) => Some(Self::LogicalAnd),
            _ => None,
        }
    }

    fn equality_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;
        let second = cursor.next()?.kind;

        match (first, second) {
            (TokenKind::Eq, TokenKind::Eq) => Some(Self::LogicalEq),
            (TokenKind::Bang, TokenKind::Eq) => Some(Self::LogicalNotEq),
            _ => None,
        }
    }

    fn relational_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;
        let second = cursor.next().map(|Token { kind, .. }| kind);

        match (first, second) {
            (TokenKind::Lt, Some(TokenKind::Eq)) => Some(Self::LtEq),
            (TokenKind::Gt, Some(TokenKind::Eq)) => Some(Self::GtEq),
            (TokenKind::Lt, _) => Some(Self::Lt),
            (TokenKind::Gt, _) => Some(Self::Gt),
            _ => None,
        }
    }

    fn additive_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;

        match first {
            TokenKind::Plus => Some(Self::Addition),
            TokenKind::Minus => Some(Self::Subtraction),
            _ => None,
        }
    }

    fn multiplicative_try_from(cursor: &mut Cursor) -> Option<Self> {
        let first = cursor.next()?.kind;

        match first {
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
}

#[derive(Clone, Copy)]
pub struct AstKey(usize);

impl fmt::Debug for AstKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

/// TODO --- remove children, embed number of children in enum
#[derive(Debug)]
pub struct AstNode {
    #[allow(dead_code)]
    #[cfg(debug_assertions)]
    pub pos: AstKey,

    pub data: AstData,
    pub children: Vec<AstKey>,
}

#[derive(Debug)]
pub struct Ast {
    root: AstKey,
    data: Vec<AstNode>,
}

impl Ast {
    fn new() -> Ast {
        Ast { root: AstKey(0), data: Vec::new() }
    }

    fn insert(&mut self, data: AstData, children: Vec<AstKey>) -> AstKey {
        let key = AstKey(self.data.len());

        #[cfg(debug_assertions)]
        let node = AstNode { pos: key, data, children };
        #[cfg(not(debug_assertions))]
        let node = AstNode { data, children };

        self.data.push(node);

        key
    }

    pub fn get(&self, key: AstKey) -> &AstNode {
        &self.data[key.0]
    }

    pub fn get_root(&self) -> &AstNode {
        self.get(self.root)
    }
}

/// Moves through the tokens ignoring whitespace
#[derive(Clone)]
struct Cursor<'a>(core::slice::Iter<'a, Token<'a>>);

impl<'a> Cursor<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Cursor(tokens.iter())
    }

    fn next(&mut self) -> Option<Token<'a>> {
        loop {
            match self.0.next() {
                Some(Token { kind: TokenKind::Whitespace, .. }) => (),
                token_or_none => break token_or_none.copied(),
            }
        }
    }
}

/// Match `TokenKind` or return
macro_rules! match_token {
    ($token:expr, $kind:pat $(,)?) => {{
        let _token = $token;
        match _token.kind {
            $kind => Some(_token),
            _ => None,
        }
    }};
}

/// Makes `value` or return
macro_rules! match_value {
    ($token:expr, $value:expr $(,)?) => {{
        let _token = $token;
        match _token.value {
            $value => Some(_token),
            _ => None,
        }
    }};
}

type ParseBinOperandFn = fn(&mut Ast, &mut Cursor) -> Option<AstKey>;
type BinOpTokenFn = fn(&mut Cursor) -> Option<BinOpKind>;

fn parse_binop(
    ast: &mut Ast,
    cursor: &mut Cursor,
    parse_operand: ParseBinOperandFn,
    binop_token: BinOpTokenFn,
) -> Option<AstKey> {
    let mut kbinop = parse_operand(ast, cursor)?;

    while let Some(binop_kind) = binop_token(&mut cursor.clone()) {
        cursor.next();
        let kop2 = parse_operand(ast, cursor)?;

        let binop = AstData::Exp(Expression::BinOp(binop_kind));
        kbinop = ast.insert(binop, vec![kbinop, kop2]);
    }

    Some(kbinop)
}

/// <unary_op> ::= "!" | "~" | "-"
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
fn parse_factor(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let token = cursor.next()?;

    match token.kind {
        TokenKind::OpenParen => {
            let kexp = parse_exp(ast, cursor)?;

            match_token!(cursor.next()?, TokenKind::CloseParen)?;

            Some(kexp)
        }
        TokenKind::Literal => {
            let literal = Literal::Integer(token.value.parse::<i32>().ok()?);
            let literal = AstData::Exp(Expression::Literal(literal));

            Some(ast.insert(literal, Vec::new()))
        }
        kind => {
            let unop_kind = UnOpKind::try_from(kind)?;
            let koperand = parse_factor(ast, cursor)?;
            let unop = AstData::Exp(Expression::UnOp(unop_kind));

            Some(ast.insert(unop, vec![koperand]))
        }
    }
}

/// <multiplicative_exp> ::= <factor> { ("*" | "/") <factor> }
fn parse_multiplicative_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_factor, BinOpKind::multiplicative_try_from)
}

/// <additive-exp> ::= <multiplicative_exp> { ("+" | "-") <multiplicative_exp> }
fn parse_additive_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_multiplicative_exp, BinOpKind::additive_try_from)
}

/// <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
fn parse_relational_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_additive_exp, BinOpKind::relational_try_from)
}

/// <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
fn parse_equality_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_relational_exp, BinOpKind::equality_try_from)
}

/// <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
fn parse_logical_and_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_equality_exp, BinOpKind::logical_and_try_from)
}

/// <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
fn parse_logical_or_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_binop(ast, cursor, parse_logical_and_exp, BinOpKind::logical_or_try_from)
}

fn parse_exp(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    parse_logical_or_exp(ast, cursor)
}

/// <statement> ::= "return" <exp> ";"
fn parse_statement(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    match_value!(cursor.next()?, "return")?;

    let kexp = parse_exp(ast, cursor)?;

    match_token!(cursor.next()?, TokenKind::Semicolon)?;

    let stmt = AstData::Stmt(Statement::Return);

    Some(ast.insert(stmt, vec![kexp]))
}

/// <function> ::= "int" <id> "(" ")" "{" <statement> "}"
fn parse_function(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    match_value!(cursor.next()?, "int")?;

    let token = match_token!(cursor.next()?, TokenKind::Ident)?;
    let name = token.value.to_owned();

    match_token!(cursor.next()?, TokenKind::OpenParen)?;
    match_token!(cursor.next()?, TokenKind::CloseParen)?;
    match_token!(cursor.next()?, TokenKind::OpenBrace)?;

    let kstmt = parse_statement(ast, cursor)?;

    match_token!(cursor.next()?, TokenKind::CloseBrace)?;

    let func = AstData::Func(Function { name });

    Some(ast.insert(func, vec![kstmt]))
}

/// <program> ::= <function>
fn parse_program(ast: &mut Ast, cursor: &mut Cursor) -> Option<AstKey> {
    let kfunc = parse_function(ast, cursor)?;
    let prog = AstData::Prog(Program());

    Some(ast.insert(prog, vec![kfunc]))
}

pub fn parse(tokens: Vec<Token>) -> Option<Ast> {
    let mut ast = Ast::new();
    let mut cursor = Cursor::new(&tokens);

    let kprog = parse_program(&mut ast, &mut cursor)?;
    ast.root = kprog;

    #[cfg(debug_assertions)]
    println!("\n### AST ###\n{ast:#?}\n");

    Some(ast)
}
