#![allow(dead_code)]

use std::fmt;

use crate::common::*;
use crate::lex::{Token, TokenKind};

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    ArithNeg,
    BitWsNot,
    LogicNot,
}

impl UnOpKind {
    fn try_from(kind: TokenKind) -> Option<Self> {
        match kind {
            TokenKind::Minus => Some(Self::ArithNeg),
            TokenKind::Tilde => Some(Self::BitWsNot),
            TokenKind::Not => Some(Self::LogicNot),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    // arithmetic
    ArithAdd,
    ArithSub,
    ArithMul,
    ArithDiv,
    ArithMod,

    // bitshift
    BitShLeft,
    BitShRight,

    // relational
    RelatLt,
    RelatLeq,
    RelatGt,
    RelatGeq,
    RelatEq,
    RelatNotEq,

    // bitwise
    BitWsAnd,
    BitWsOr,
    BitWsXor,

    // logical
    LogicAnd,
    LogicOr,
}

impl BinOpKind {
    fn logical_or_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::OrOr => Some(Self::LogicOr),
            _ => None,
        })
    }

    fn logical_and_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::AndAnd => Some(Self::LogicAnd),
            _ => None,
        })
    }

    fn equality_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::EqEq => Some(Self::RelatEq),
            TokenKind::Neq => Some(Self::RelatNotEq),
            _ => None,
        })
    }

    fn relational_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::Gt => Some(Self::RelatGt),
            TokenKind::Geq => Some(Self::RelatGeq),
            TokenKind::Lt => Some(Self::RelatLt),
            TokenKind::Leq => Some(Self::RelatLeq),
            _ => None,
        })
    }

    fn additive_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::Plus => Some(Self::ArithAdd),
            TokenKind::Minus => Some(Self::ArithSub),
            _ => None,
        })
    }

    fn multiplicative_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::Star => Some(Self::ArithMul),
            TokenKind::Slash => Some(Self::ArithDiv),
            TokenKind::Percent => Some(Self::ArithMod),
            _ => None,
        })
    }

    fn bitwise_and_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::And => Some(Self::BitWsAnd),
            _ => None,
        })
    }

    fn bitwise_or_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::Or => Some(Self::BitWsOr),
            _ => None,
        })
    }

    fn bitwise_xor_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::Caret => Some(Self::BitWsXor),
            _ => None,
        })
    }

    fn bitshift_try_from(cursor: &mut Cursor) -> Option<Self> {
        cursor.advance_if_some(|k| match k {
            TokenKind::LtLt => Some(Self::BitShLeft),
            TokenKind::GtGt => Some(Self::BitShRight),
            _ => None,
        })
    }
}

#[derive(Debug)]
pub enum Expression<'a> {
    BinOp {
        kind: BinOpKind,
        op1: AstKey,
        op2: AstKey,
    },
    UnOp {
        kind: UnOpKind,
        op: AstKey,
    },
    Var(Token<'a>),
    Assignment {
        name: Token<'a>,
        value: AstKey,
    },
    Literal(Token<'a>),
    Ternary {
        cond: AstKey,
        ifb: AstKey,
        elseb: AstKey,
    },
}

#[derive(Debug)]
pub enum Statement {
    Return(AstKey),
    Exp(AstKey),
    If {
        cond: AstKey,
        ifb: AstKey,
        elseb: Option<AstKey>,
    },
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub name: Token<'a>,
    pub value: Option<AstKey>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Stmt(Statement),
    Decl(Decl<'a>),
}

#[derive(Debug)]
pub enum AstData<'a> {
    Prog(AstKey),
    Func {
        name: Token<'a>,
        block_items: Vec<AstKey>,
    },
    BlockItem(BlockItem<'a>),
    Exp(Expression<'a>),
}

pub type AstKey = IndexKey;

#[derive(Default)]
pub struct Ast<'a> {
    root: Option<AstKey>,
    data: IndexList<AstData<'a>>,
}

impl<'a> Ast<'a> {
    fn push(&mut self, data: AstData<'a>) -> AstKey {
        self.data.push(data)
    }

    pub fn get(&self, key: AstKey) -> &AstData<'_> {
        self.data.get(key)
    }

    pub fn get_root(&self) -> &AstData<'_> {
        self.get(self.root.unwrap())
    }

    fn traverse(&self, start: IndexKey, depth: usize, keys: &mut Vec<(IndexKey, usize)>) {
        keys.push((start, depth));
        let data = self.get(start);

        match data {
            AstData::Prog(key) => {
                self.traverse(*key, depth + 1, keys);
            }
            AstData::Func { block_items, .. } => {
                block_items
                    .iter()
                    .for_each(|key| self.traverse(*key, depth + 1, keys));
            }
            AstData::BlockItem(block_item) => match block_item {
                BlockItem::Decl(decl) => {
                    if let Decl {
                        value: Some(key), ..
                    } = *decl
                    {
                        self.traverse(key, depth + 1, keys);
                    }
                }
                BlockItem::Stmt(stmt) => match *stmt {
                    Statement::Return(key) => self.traverse(key, depth + 1, keys),
                    Statement::Exp(key) => self.traverse(key, depth + 1, keys),
                    Statement::If { cond, ifb, elseb } => {
                        self.traverse(cond, depth + 1, keys);
                        self.traverse(ifb, depth + 2, keys);
                        if let Some(elseb) = elseb {
                            self.traverse(elseb, depth + 2, keys);
                        }
                    }
                },
            },
            AstData::Exp(exp) => match *exp {
                Expression::BinOp { op1, op2, .. } => {
                    self.traverse(op1, depth + 1, keys);
                    self.traverse(op2, depth + 1, keys);
                }
                Expression::UnOp { op, .. } => self.traverse(op, depth + 1, keys),
                Expression::Var(_) => (),
                Expression::Assignment { value: key, .. } => self.traverse(key, depth + 1, keys),
                Expression::Literal(_) => (),
                Expression::Ternary { cond, ifb, elseb } => {
                    self.traverse(cond, depth + 1, keys);
                    self.traverse(ifb, depth + 2, keys);
                    self.traverse(elseb, depth + 2, keys);
                }
            },
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&AstData<'_>, usize)> {
        let mut keys = Vec::with_capacity(self.data.len());

        if let Some(root) = self.root {
            self.traverse(root, 0, &mut keys);
        }

        keys.into_iter().map(|(k, depth)| (self.get(k), depth))
    }
}

impl AstData<'_> {
    fn format_indent(&self, f: &mut fmt::Formatter<'_>, depth: usize) -> fmt::Result {
        let print = format!("{:#?}", self);
        let padding = "\t".repeat(depth);
        let print_indent = print
            .lines()
            .map(|line| format!("{}{}", padding, line))
            .collect::<Vec<_>>()
            .join("\n");
        writeln!(f, "{}", print_indent)
    }
}

impl fmt::Debug for Ast<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (data, depth) in self.iter() {
            data.format_indent(f, depth)?;
        }
        Ok(())
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
                Some(Token {
                    kind: TokenKind::Whitespace,
                    ..
                }) => (),
                token_or_none => break token_or_none.copied(),
            }
        }
    }

    fn peek(&self) -> Option<Token<'a>> {
        self.clone().next()
    }

    /// peek, run `predicate` and advance if Some()
    fn advance_if_some<T>(
        &mut self,
        mut predicate: impl FnMut(TokenKind) -> Option<T>,
    ) -> Option<T> {
        let kind = self.peek().map(|t| t.kind)?;

        predicate(kind).inspect(|_| {
            self.next();
        })
    }
}

/// if `token` is of `kind`
/// fn (`Token`) -> `Option<Token>`
macro_rules! match_kind {
    ($token:expr, $kind:pat $(,)?) => {{
        let _token = $token;
        match _token.kind {
            $kind => Some(_token),
            _ => None,
        }
    }};
}

/// if `token` is with `value`
/// fn (`Token`) -> `Option<Token>`
macro_rules! match_value {
    ($token:expr, $value:expr $(,)?) => {{
        let _token = $token;
        match _token.value {
            $value => Some(_token),
            _ => None,
        }
    }};
}

type ParseBinOperandFn<'a> = fn(&mut Ast<'a>, &mut Cursor<'a>) -> Option<AstKey>;
type ParseBinOperatorFn = fn(&mut Cursor) -> Option<BinOpKind>;

fn parse_binop<'a>(
    ast: &mut Ast<'a>,
    cursor: &mut Cursor<'a>,
    parse_operand: ParseBinOperandFn<'a>,
    parse_operator: ParseBinOperatorFn,
) -> Option<AstKey> {
    let mut kbinop = parse_operand(ast, cursor).unwrap();

    while let Some(binop_kind) = parse_operator(cursor) {
        let kop2 = parse_operand(ast, cursor).unwrap();

        let binop = AstData::Exp(Expression::BinOp {
            kind: binop_kind,
            op1: kbinop,
            op2: kop2,
        });
        kbinop = ast.push(binop);
    }

    Some(kbinop)
}

/// <unary_op> ::= "!" | "~" | "-"
/// <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
fn parse_factor<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let token = cursor.next().unwrap();

    //printfn!("{token:?}");

    match token.kind {
        TokenKind::OpenParen => {
            let kexp = parse_exp(ast, cursor).unwrap();

            match_kind!(cursor.next().unwrap(), TokenKind::CloseParen).unwrap();

            Some(kexp)
        }
        TokenKind::Literal => {
            let literal = AstData::Exp(Expression::Literal(token));

            Some(ast.push(literal))
        }
        TokenKind::Ident => {
            let var = AstData::Exp(Expression::Var(token));

            Some(ast.push(var))
        }
        kind => {
            let unop_kind = UnOpKind::try_from(kind).unwrap();
            let koperand = parse_factor(ast, cursor).unwrap();
            let unop = AstData::Exp(Expression::UnOp {
                kind: unop_kind,
                op: koperand,
            });

            Some(ast.push(unop))
        }
    }
}

/// <multiplicative_exp> ::= <factor> { ("*" | "/") <factor> }
fn parse_multiplicative_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_factor,
        BinOpKind::multiplicative_try_from,
    )
}

/// <additive_exp> ::= <multiplicative_exp> { ("+" | "-") <multiplicative_exp> }
fn parse_additive_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_multiplicative_exp,
        BinOpKind::additive_try_from,
    )
}

/// <bitshift_exp> ::= <additive_exp> { ("<<" | ">>") <additive_exp> }
fn parse_bitshift_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_additive_exp,
        BinOpKind::bitshift_try_from,
    )
}

/// <relational_exp> ::= <bitshift_exp> { ("<" | ">" | "<=" | ">=") <bitshift_exp> }
fn parse_relational_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_bitshift_exp,
        BinOpKind::relational_try_from,
    )
}

/// <equality_exp> ::= <relational_exp> { ("!=" | "==") <relational_exp> }
fn parse_equality_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_relational_exp,
        BinOpKind::equality_try_from,
    )
}

/// <bitwise_and_exp> ::= <equality_exp> { "&" <equality_exp> }
fn parse_bitwise_and_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_equality_exp,
        BinOpKind::bitwise_and_try_from,
    )
}

/// <bitwise_xor_exp> ::= <bitwise_and_exp> { "^" <bitwise_and_exp> }
fn parse_bitwise_xor_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_bitwise_and_exp,
        BinOpKind::bitwise_xor_try_from,
    )
}

/// <bitwise_or_exp> ::= <bitwise_xor_exp> { "|" <bitwise_xor_exp> }
fn parse_bitwise_or_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_bitwise_xor_exp,
        BinOpKind::bitwise_or_try_from,
    )
}

/// <logical_and_exp> ::= <bitwise_or_exp> { "&&" <bitwise_or_exp> }
fn parse_logical_and_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_bitwise_or_exp,
        BinOpKind::logical_and_try_from,
    )
}

/// <logical_or_exp> ::= <logical_and_exp> { "||" <logical_and_exp> }
fn parse_logical_or_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    parse_binop(
        ast,
        cursor,
        parse_logical_and_exp,
        BinOpKind::logical_or_try_from,
    )
}

/// <ternary_exp> ::= <logical-or-exp> [ "?" <exp> ":" <ternary_exp> ]
fn parse_ternary_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let kexp = parse_logical_or_exp(ast, cursor).unwrap();

    match cursor.peek() {
        Some(Token {
            kind: TokenKind::Question,
            ..
        }) => {
            cursor.next();

            let kifb = parse_exp(ast, cursor).unwrap();

            match_kind!(cursor.next().unwrap(), TokenKind::Colon).unwrap();

            let kelseb = parse_ternary_exp(ast, cursor).unwrap();
            let ternary = AstData::Exp(Expression::Ternary {
                cond: kexp,
                ifb: kifb,
                elseb: kelseb,
            });

            Some(ast.push(ternary))
        }
        _ => Some(kexp),
    }
}

/// <exp> ::= <id> "=" <exp> | <ternary_exp>
fn parse_exp<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let bak = cursor.clone();
    let first = cursor.next().unwrap();
    let second = cursor.next().map(|t| t.kind);

    //printfn!("{first:?}, {second:?}");

    match (first.kind, second) {
        (TokenKind::Ident, Some(TokenKind::Eq)) => {
            let kexp = parse_exp(ast, cursor).unwrap();
            let exp = AstData::Exp(Expression::Assignment {
                name: first,
                value: kexp,
            });

            Some(ast.push(exp))
        }
        _ => {
            *cursor = bak;
            parse_ternary_exp(ast, cursor)
        }
    }
}

/// <statement> ::=
///     "return" <exp> ";"
///     | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
///     | <exp> ";"
fn parse_statement<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let token = cursor.peek();
    //printfn!("{token:?}");

    let stmt = match token.unwrap().value {
        "return" => {
            cursor.next();
            let kexp = parse_exp(ast, cursor).unwrap();

            match_kind!(cursor.next().unwrap(), TokenKind::Semicolon).unwrap();

            AstData::BlockItem(BlockItem::Stmt(Statement::Return(kexp)))
        }
        "if" => {
            cursor.next();

            match_kind!(cursor.next().unwrap(), TokenKind::OpenParen).unwrap();

            let kcond = parse_exp(ast, cursor).unwrap();

            match_kind!(cursor.next().unwrap(), TokenKind::CloseParen).unwrap();

            let kifb = parse_statement(ast, cursor).unwrap();
            let kelseb = match cursor.peek() {
                Some(Token { value: "else", .. }) => {
                    cursor.next();

                    Some(parse_statement(ast, cursor).unwrap())
                }
                _ => None,
            };

            AstData::BlockItem(BlockItem::Stmt(Statement::If {
                cond: kcond,
                ifb: kifb,
                elseb: kelseb,
            }))
        }
        _ => {
            let kexp = parse_exp(ast, cursor).unwrap();

            match_kind!(cursor.next().unwrap(), TokenKind::Semicolon).unwrap();

            AstData::BlockItem(BlockItem::Stmt(Statement::Exp(kexp)))
        }
    };

    Some(ast.push(stmt))
}

/// <decl> ::= "int" <id> [ = <exp> ] ";"
fn parse_decl<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let token = cursor.peek();
    //printfn!("{token:?}");

    match_value!(token.unwrap(), "int").and_then(|_| {
        cursor.next();

        let ident = match_kind!(cursor.next().unwrap(), TokenKind::Ident).unwrap();
        let value = match_kind!(cursor.peek().unwrap(), TokenKind::Eq).and_then(|_| {
            cursor.next();
            let kexp = parse_exp(ast, cursor).unwrap();

            Some(kexp)
        });

        match_kind!(cursor.next().unwrap(), TokenKind::Semicolon).unwrap();

        let decl = AstData::BlockItem(BlockItem::Decl(Decl { name: ident, value }));

        Some(ast.push(decl))
    })
}

/// <block_item> ::= <statement> | <decl>
fn parse_block_item<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    if let Some(kdecl) = parse_decl(ast, cursor) {
        return Some(kdecl);
    }

    if let Some(kstmt) = parse_statement(ast, cursor) {
        return Some(kstmt);
    }

    None
}

/// <function> ::= "int" <ident> "(" ")" "{" { <block-item> } "}"
fn parse_function<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    match_value!(cursor.next().unwrap(), "int").unwrap();

    let token = match_kind!(cursor.next().unwrap(), TokenKind::Ident).unwrap();

    match_kind!(cursor.next().unwrap(), TokenKind::OpenParen).unwrap();
    match_kind!(cursor.next().unwrap(), TokenKind::CloseParen).unwrap();
    match_kind!(cursor.next().unwrap(), TokenKind::OpenBrace).unwrap();

    let mut block_items = Vec::new();

    loop {
        match cursor.peek()? {
            Token {
                kind: TokenKind::CloseBrace,
                ..
            } => break,
            _ => {
                let kblock_item = parse_block_item(ast, cursor).unwrap();
                block_items.push(kblock_item);
            }
        }
    }

    match_kind!(cursor.next().unwrap(), TokenKind::CloseBrace).unwrap();

    let func = AstData::Func {
        name: token,
        block_items,
    };

    Some(ast.push(func))
}

/// <program> ::= <function>
fn parse_program<'a>(ast: &mut Ast<'a>, cursor: &mut Cursor<'a>) -> Option<AstKey> {
    let kfunc = parse_function(ast, cursor).unwrap();
    let prog = AstData::Prog(kfunc);

    Some(ast.push(prog))
}

/// TODO: streamline operator precendece
/// TODO: better AST traversal & pretty-printing
/// TODO: optimize Ast e IndexList
pub fn parse<'a>(tokens: &'a [Token]) -> Option<Ast<'a>> {
    let mut ast = Ast::default();
    let mut cursor = Cursor::new(tokens);

    let kprog = parse_program(&mut ast, &mut cursor).unwrap();
    ast.root = Some(kprog);

    #[cfg(debug_assertions)]
    println!("\n# AST\n```rust\n{ast:#?}\n```\n");

    Some(ast)
}

// arithmetic * ;; / ;; % --> + ;; ,
// shift: <<, >>
// relational: < ;; <= ;; > ;; >= --> == ;; !=
// bitwise: & --> ^ --> |
// logical or: && --> ||
// ternary: ?:
// assignment: = ;; += ;; -= ;; *= ;; /= ;; %= ;; <<= >>= &= ^= |=
// comma: ,
