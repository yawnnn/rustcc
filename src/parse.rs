#![allow(unused)]

use crate::lex::{Token, TokenKind};
use std::collections::VecDeque;
use std::fmt::{self, Debug, Display, Write};
use std::iter;
use std::ops::ControlFlow;

#[derive(Debug, Clone, Copy)]
pub enum UnOpKind {
    // 15
    SufIncr, // x++
    SufDecr, // x--
    Call,    // x()
    // 14
    PreIncr,  // ++x
    PreDecr,  // --x
    Positive, // +x
    Negative, // -x
    LogNot,      // !x
    BitNot,   // ~x
    Cast,     // (type)x
    Deref,    // *x
    Addr,     // &x
}

#[derive(Debug, Clone, Copy)]
pub enum BinOpKind {
    // 15
    Index,       // x[]
    MemberAcc,   // x.y
    MemberDeref, // x->y
    // 13
    Mul, // x * y
    Div, // x / y
    Mod, // x % y
    // 12
    Add, // x + y
    Sub, // x - y
    // 11
    LShift, // x << y
    RShift, // x >> y
    // 10
    Lt,  // x < y
    Leq, // x <= y
    Gt,  // x > y
    Geq, // x >= y
    // 9
    Eq,  // x == y
    Neq, // x != y
    // 8
    BitAnd, // x & y
    // 7
    BitXor, // x ^ y
    // 6
    BitOr, // x | y
    // 5
    LogAnd, // x && y
    // 4
    LogOr, // x || y
    // 2
    Assign,       // x = y
    AddAssign,    // x += y
    SubAssign,    // x -= y
    MulAssign,    // x *= y
    DivAssign,    // x /= y
    ModAssign,    // x %= y
    LShiftAssign, // x <<= y
    RShiftAssign, // x >>= y
    BitAndAssign, // x &= y
    BitXorAssign, // x ^= y
    BitOrAssign,  // x |= y
    // 1
    Comma, // x, y
}

impl BinOpKind {
    const fn is_assignment(self) -> bool {
        use BinOpKind::*;
        matches!(
            self,
            Assign
                | AddAssign
                | SubAssign
                | MulAssign
                | DivAssign
                | ModAssign
                | LShiftAssign
                | RShiftAssign
                | BitAndAssign
                | BitXorAssign
                | BitOrAssign
        )
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TerOpKind {
    // 3
    Cond, // x ? y : z
}

enum OpAssoc {
    Left,
    Right,
}

impl OpAssoc {
    const fn precedence(self, raw_prec: u8) -> (u8, u8) {
        let prec = raw_prec * 2;
        match self {
            OpAssoc::Left => (prec - 1, prec),
            OpAssoc::Right => (prec, prec - 1),
        }
    }
}

#[derive(Clone, Copy, Debug)]
enum OpKind {
    Un(UnOpKind),
    Bin(BinOpKind),
    Ter(TerOpKind),
}

impl OpKind {
    #[rustfmt::skip]
    const fn precedence_and_assoc(self) -> (u8, OpAssoc) {
        use BinOpKind::*;
        use OpAssoc as A;
        use TerOpKind::*;
        use UnOpKind::*;

        match self {
            OpKind::Un(SufIncr | SufDecr | Call | LogNot)
            | OpKind::Bin(Index | MemberAcc | MemberDeref) =>               (15, A::Left ),
            OpKind::Un(PreIncr | PreDecr | Positive | Negative | BitNot
            | Cast | Deref | Addr) =>                                       (14, A::Right),
            OpKind::Bin(Mul | Div | Mod) =>                                 (13, A::Left ),
            OpKind::Bin(Add | Sub) =>                                       (12, A::Left ),
            OpKind::Bin(LShift | RShift) =>                                 (11, A::Left ),
            OpKind::Bin(Lt | Leq | Gt | Geq) =>                             (10, A::Left ),
            OpKind::Bin(Eq | Neq) =>                                      ( 9, A::Left ),
            OpKind::Bin(BitAnd) =>                                          ( 8, A::Left ),
            OpKind::Bin(BitXor) =>                                          ( 7, A::Left ),
            OpKind::Bin(BitOr) =>                                           ( 6, A::Left ),
            OpKind::Bin(LogAnd) =>                                          ( 5, A::Left ),
            OpKind::Bin(LogOr) =>                                           ( 4, A::Left ),
            OpKind::Ter(Cond) =>                                            ( 3, A::Right),
            OpKind::Bin(Assign | AddAssign | SubAssign | MulAssign
            | DivAssign | ModAssign | LShiftAssign | RShiftAssign
            | BitAndAssign | BitXorAssign | BitOrAssign) =>                 ( 2, A::Right),
            OpKind::Bin(Comma) =>                                           ( 1, A::Left ),
        }
    }

    const fn precedence(self) -> (u8, u8) {
        let (raw_prec, assoc) = self.precedence_and_assoc();
        assoc.precedence(raw_prec)
    }

    const fn parse_prefix(kind: TokenKind) -> Option<(UnOpKind, u8)> {
        use TokenKind as T;
        use UnOpKind as U;

        //println!("parse_prefix({kind:?}");
        let kind = match kind {
            T::PlusPlus => U::PreIncr,
            T::MinusMinus => U::PreDecr,
            T::Plus => U::Positive,
            T::Minus => U::Negative,
            T::Bang => U::LogNot,
            T::Tilde => U::BitNot,
            T::OpenParen => U::Cast,
            T::Star => U::Deref,
            T::Amp => U::Addr,
            // sizeof, _Alignof, (type){list}
            _ => return None,
        };

        Some((kind, OpKind::Un(kind).precedence().1))
    }

    const fn parse_suffix(kind: TokenKind) -> Option<(OpKind, u8)> {
        use BinOpKind as B;
        use TokenKind as T;
        use UnOpKind as U;

        //println!("parse_suffix({kind:?}");
        let kind = match kind {
            T::PlusPlus => OpKind::Un(U::SufIncr),
            T::MinusMinus => OpKind::Un(U::SufDecr),
            T::OpenParen => OpKind::Un(U::Call),
            T::OpenBracket => OpKind::Bin(B::Index),
            _ => return None,
        };

        Some((kind, kind.precedence().0))
    }

    const fn parse_infix(kind: TokenKind) -> Option<(OpKind, (u8, u8))> {
        use BinOpKind as B;
        use TokenKind as T;

        //println!("parse_infix({kind:?}");
        let kind = match kind {
            T::Question => OpKind::Ter(TerOpKind::Cond),
            _ => {
                let k = match kind {
                    T::Dot => B::MemberAcc,
                    T::Arrow => B::MemberDeref,
                    T::Star => B::Mul,
                    T::Slash => B::Div,
                    T::Percent => B::Mod,
                    T::Plus => B::Add,
                    T::Minus => B::Sub,
                    T::LtLt => B::LShift,
                    T::GtGt => B::RShift,
                    T::Lt => B::Lt,
                    T::Leq => B::Leq,
                    T::Gt => B::Gt,
                    T::Geq => B::Geq,
                    T::EqEq => B::Eq,
                    T::BangEq => B::Neq,
                    T::Amp => B::BitAnd,
                    T::Caret => B::BitXor,
                    T::Pipe => B::BitOr,
                    T::AmpAmp => B::LogAnd,
                    T::PipePipe => B::LogOr,
                    T::Eq => B::Assign,
                    T::PlusEq => B::AddAssign,
                    T::MinusEq => B::SubAssign,
                    T::StarEq => B::MulAssign,
                    T::SlashEq => B::DivAssign,
                    T::PercentEq => B::ModAssign,
                    T::LtLtEq => B::LShiftAssign,
                    T::GtGtEq => B::RShiftAssign,
                    T::AmpEq => B::BitAndAssign,
                    T::CaretEq => B::BitXorAssign,
                    T::PipeEq => B::BitOrAssign,
                    T::Comma => B::Comma,
                    _ => return None,
                };
                OpKind::Bin(k)
            }
        };

        Some((kind, kind.precedence()))
    }
}

#[derive(Debug)]
pub enum Expr<'a> {
    Const(Token<'a>),
    Var(Token<'a>),
    UnOp(Token<'a>, UnOpKind, Box<Expr<'a>>),
    BinOp(Token<'a>, BinOpKind, Box<Expr<'a>>, Box<Expr<'a>>),
    TerOp(
        Token<'a>,
        TerOpKind,
        Box<Expr<'a>>,
        Box<Expr<'a>>,
        Box<Expr<'a>>,
    ),
    Assign(Token<'a>, BinOpKind, Box<Expr<'a>>, Box<Expr<'a>>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Ret(Expr<'a>),
    Expr(Expr<'a>),
    If {
        cond: Expr<'a>,
        true_b: Box<Stmt<'a>>,
        false_b: Option<Box<Stmt<'a>>>,
    },
}

#[derive(Debug)]
pub struct Decl<'a> {
    pub name: Token<'a>,
    pub value: Option<Expr<'a>>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Stmt(Stmt<'a>),
    Decl(Decl<'a>),
}

#[derive(Debug)]
pub struct FnDef<'a> {
    pub name: Token<'a>,
    //args: Vec<FnArg>,
    pub body: Vec<BlockItem<'a>>, // Vec<BlockItem>
}

#[derive(Debug, Default)]
pub struct Ast<'a> {
    pub defs: Vec<FnDef<'a>>,
}

impl<'a> Ast<'a> {
    pub fn to_sexpr(&self, indent: bool) -> String {
        let mut pp = SExprFormatter::new(indent);
        pp.walk_ast(self);
        pp.finalize()
    }
}

pub trait AstVisitor {
    fn walk_expr(&mut self, expr: &Expr) -> ControlFlow<(), ()> {
        match expr {
            Expr::Const(_) | Expr::Var(_) => ControlFlow::Continue(()),
            Expr::UnOp(_, _, op1) => self.visit_expr(op1),
            Expr::BinOp(_, _, op1, op2) => {
                self.visit_expr(op1)?;
                self.visit_expr(op2)
            }
            Expr::TerOp(_, _, cond, ifb, elseb) => {
                self.visit_expr(cond)?;
                self.visit_expr(ifb)?;
                self.visit_expr(elseb)
            }
            Expr::Assign(_, _, kvar, kvalue) => {
                self.visit_expr(kvar)?;
                self.visit_expr(kvalue)
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> ControlFlow<(), ()> {
        self.walk_expr(expr)
    }

    fn walk_decl(&mut self, decl: &Decl) -> ControlFlow<(), ()> {
        if let Some(value) = &decl.value {
            self.visit_expr(value)?
        }

        ControlFlow::Continue(())
    }

    fn visit_decl(&mut self, decl: &Decl) -> ControlFlow<(), ()> {
        self.walk_decl(decl)
    }

    fn walk_stmt(&mut self, stmt: &Stmt) -> ControlFlow<(), ()> {
        match stmt {
            Stmt::Ret(value) => self.visit_expr(value),
            Stmt::Expr(expr) => self.visit_expr(expr),
            Stmt::If {
                cond,
                true_b,
                false_b,
            } => {
                self.visit_expr(cond)?;
                self.visit_stmt(true_b)?;
                if let Some(false_b) = false_b {
                    self.visit_stmt(false_b)?
                }

                ControlFlow::Continue(())
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> ControlFlow<(), ()> {
        self.walk_stmt(stmt)
    }

    fn walk_block_item(&mut self, block_item: &BlockItem) -> ControlFlow<(), ()> {
        match block_item {
            BlockItem::Stmt(stmt) => self.visit_stmt(stmt),
            BlockItem::Decl(decl) => self.visit_decl(decl),
        }
    }

    fn visit_block_item(&mut self, block_item: &BlockItem) -> ControlFlow<(), ()> {
        self.walk_block_item(block_item)
    }

    fn walk_fn_def(&mut self, fn_def: &FnDef) -> ControlFlow<(), ()> {
        for block_item in &fn_def.body {
            self.visit_block_item(block_item)?
        }

        ControlFlow::Continue(())
    }

    fn visit_fn_def(&mut self, fn_def: &FnDef) -> ControlFlow<(), ()> {
        self.walk_fn_def(fn_def)
    }

    fn walk_ast(&mut self, ast: &Ast) {
        for fndef in &ast.defs {
            let ControlFlow::Continue(_) = self.visit_fn_def(fndef) else {
                break;
            };
        }
    }
}

// format expressions to s-expressions in Ast
struct SExprFormatter {
    indent: bool,
    depth: usize,
    acc: String,
}

impl SExprFormatter {
    fn new(indent: bool) -> Self {
        SExprFormatter {
            indent,
            depth: 0,
            acc: String::new(),
        }
    }

    fn finalize(self) -> String {
        self.acc
    }

    fn fmt_padding(&mut self) {
        let s = if self.indent {
            "\t".repeat(self.depth - 1)
        } else {
            String::new()
        };
        self.acc += &s;
    }

    fn fmt_newl(&mut self) {
        let s = if self.indent { "\n" } else { "" };
        self.acc += s;
    }

    fn fmt_op_spacing(&mut self) {
        let s = if self.indent { "" } else { " " };
        self.acc += s;
    }

    fn fmt_atom(&mut self, t: Token) {
        self.fmt_padding();
        self.acc += t.value;
    }

    fn fmt_op(&mut self, op: Token, operands: &[&Expr]) {
        self.fmt_padding();
        self.acc.push('(');
        self.acc += op.value;
        self.fmt_newl();

        for operand in operands {
            self.fmt_op_spacing();
            self.visit_expr(operand);
            self.fmt_newl();
        }

        self.fmt_padding();
        self.acc.push(')');
    }

    fn fmt_expr(&mut self, expr: &Expr) {
        match &expr {
            &Expr::Const(t) | &Expr::Var(t) => self.fmt_atom(*t),
            &Expr::BinOp(t, _, op1, op2) => self.fmt_op(*t, &[op1, op2]),
            &Expr::UnOp(t, _, op1) => self.fmt_op(*t, &[op1]),
            &Expr::TerOp(t, _, cond, true_b, false_b) => self.fmt_op(*t, &[cond, true_b, false_b]),
            &Expr::Assign(t, _, var, value) => self.fmt_op(*t, &[var, value]),
        }
    }
}

impl AstVisitor for SExprFormatter {
    fn visit_expr(&mut self, expr: &Expr) -> ControlFlow<(), ()> {
        self.depth += 1;
        self.fmt_expr(expr);
        self.depth -= 1;

        ControlFlow::Continue(())
    }
}

/// if `token` is `Some(kind)`
///
/// if this weren't a macro the borrow checker would complain most times
/// because `token` would be passed with `p`.next() which takes `&mut Parser`, and this would take `&Parser`.
/// also this allows `kind` to be a pattern, not just one kind
///
/// fn (token: Option<Token>, kind: TokenKind) -> Option<Token>
macro_rules! match_kind {
    ($token:expr, $kind:pat $(,)?) => {{
        let _token = $token;
        match _token {
            Some(Token { kind: $kind, .. }) => _token,
            _ => None,
        }
    }};
}

/// if `token`'s str value equals `value`.
///
/// if this weren't a macro the borrow checker would complain most times
/// because `token` would be passed with `p`.next() which takes `&mut Parser`, and this would take `&Parser`
///
/// fn (p: &Parser, token: Token, value: &str) -> Option<Token>
macro_rules! match_str {
    ($token:expr, $value:expr $(,)?) => {{
        let _token = $token;
        match _token {
            Some(t) if t.value == $value => Some(_token),
            _ => None,
        }
    }};
}

pub struct Parser<'a> {
    pub tokens: Vec<Token<'a>>,
    pub token_idx: usize,
    pub ast: Ast<'a>,
}

impl<'a> Parser<'a> {
    /// returns the parser or a empty ast if there's no tokens
    fn new(tokens: Vec<Token<'a>>) -> Self {
        Parser {
            tokens,
            token_idx: 0,
            ast: Ast::default(),
        }
    }

    fn next(&mut self) -> Option<Token<'a>> {
        let t = self.tokens.get(self.token_idx).copied();
        self.token_idx += 1;
        t
    }

    fn peek(&mut self) -> Option<Token<'a>> {
        self.tokens.get(self.token_idx).copied()
    }

    fn token_at(&self, pos: usize) -> Token<'a> {
        *self.tokens.get(pos).unwrap()
    }

    // <expr> ::= <id> "=" <expr> | <ternary_expr>
    // <ternary_expr> ::= <logical_or_expr> [ "?" <expr> ":" <ternary_expr> ]
    // <logical_or_expr> ::= <logical_and_expr> { "||" <logical_and_expr> }
    // <logical_and_expr> ::= <bitwise_or_expr> { "&&" <bitwise_or_expr> }
    // <bitwise_or_expr> ::= <bitwise_xor_expr> { "|" <bitwise_xor_expr> }
    // <bitwise_xor_expr> ::= <bitwise_and_expr> { "^" <bitwise_and_expr> }
    // <bitwise_and_expr> ::= <equality_expr> { "&" <equality_expr> }
    // <equality_expr> ::= <relational_expr> { ("!=" | "==") <relational_expr> }
    // <relational_expr> ::= <bitshift_expr> { ("<" | ">" | "<=" | ">=") <bitshift_expr> }
    // <bitshift_expr> ::= <additive_expr> { ("<<" | ">>") <additive_expr> }
    // <additive_expr> ::= <multiplicative_expr> { ("+" | "-") <multiplicative_expr> }
    // <multiplicative_expr> ::= <factor> { ("*" | "/") <factor> }
    // <unary_op> ::= "!" | "~" | "-"
    // <factor> ::= "(" <expr> ")" | <unary_op> <factor> | <int> | <id>
    fn parse_expr(&mut self, min_bp: u8) -> Option<Expr<'a>> {
        let tok_lhs = self.next().expect("Expected expression");

        let mut lhs = match tok_lhs.kind {
            TokenKind::Ident => Expr::Var(tok_lhs),
            TokenKind::Literal => Expr::Const(tok_lhs),
            TokenKind::OpenParen => {
                let lhs = self.parse_expr(0).unwrap();
                assert_eq!(self.next().unwrap().kind, TokenKind::CloseParen);
                lhs
            }
            kind => {
                let (opkind, r_bp) = OpKind::parse_prefix(kind).expect("Expected expression");
                let rhs = self.parse_expr(r_bp).unwrap();
                Expr::UnOp(tok_lhs, opkind, rhs.into())
            }
        };

        loop {
            let tok_op = match self.peek() {
                None => break,
                Some(
                    tok_op @ Token {
                        kind: TokenKind::Ident | TokenKind::Literal,
                        ..
                    },
                ) => panic!("Unexpected token {:?}", tok_op.value),
                Some(tok_op) => tok_op,
            };

            if let Some((kind, l_bp)) = OpKind::parse_suffix(tok_op.kind) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = match kind {
                    OpKind::Bin(kind) => {
                        let rhs = self.parse_expr(0).unwrap();
                        assert_eq!(self.next().unwrap().kind, TokenKind::CloseBracket);
                        Expr::BinOp(tok_op, kind, lhs.into(), rhs.into())
                    }
                    OpKind::Un(kind) => Expr::UnOp(tok_op, kind, lhs.into()),
                    _ => panic!("Unexpected token {:?}", tok_op.value),
                };

                continue;
            }

            if let Some((kind, (l_bp, r_bp))) = OpKind::parse_infix(tok_op.kind) {
                if l_bp < min_bp {
                    break;
                }
                self.next();

                lhs = match kind {
                    OpKind::Ter(kind) => {
                        let mhs = self.parse_expr(0).unwrap();
                        assert_eq!(self.next().unwrap().kind, TokenKind::Colon);
                        let rhs = self.parse_expr(r_bp).unwrap();
                        Expr::TerOp(tok_op, kind, lhs.into(), mhs.into(), rhs.into())
                    }
                    OpKind::Bin(kind) if kind.is_assignment() => {
                        let rhs = self.parse_expr(r_bp).unwrap();
                        match lhs {
                            Expr::Var(_) => (),
                            n => panic!("{:?}", n),
                        }
                        Expr::Assign(tok_op, kind, lhs.into(), rhs.into())
                    }
                    OpKind::Bin(kind) => {
                        let rhs = self.parse_expr(r_bp).unwrap();
                        Expr::BinOp(tok_op, kind, lhs.into(), rhs.into())
                    }
                    _ => panic!("Unexpected token {:?}", tok_op.value),
                };

                continue;
            }
            break;
        }

        Some(lhs)
    }

    // <stmt> ::=
    //     "return" <exp> ";"
    //     | "if" "(" <exp> ")" <stmt> [ "else" <stmt> ]
    //     | <exp> ";"
    fn parse_stmt(&mut self) -> Option<Stmt<'a>> {
        let token = self.peek();
        //printfn!("{token:?}");

        let stmt = match token.unwrap().value {
            "return" => {
                self.next();
                let expr = self.parse_expr(0).unwrap();

                match_kind!(self.next(), TokenKind::Semicolon).unwrap();

                Stmt::Ret(expr)
            }
            "if" => {
                self.next();
                match_kind!(self.next(), TokenKind::OpenParen).unwrap();

                let cond = self.parse_expr(0).unwrap();

                match_kind!(self.next(), TokenKind::CloseParen).unwrap();

                let true_b = self.parse_stmt().unwrap();
                let false_b = match self.peek() {
                    Some(t) if t.value == "else" => {
                        self.next();
                        let stmt = self.parse_stmt().unwrap();

                        Some(stmt)
                    }
                    _ => None,
                };

                Stmt::If {
                    cond,
                    true_b: true_b.into(),
                    false_b: false_b.map(|stmt| stmt.into()),
                }
            }
            _ => {
                let expr = self.parse_expr(0).unwrap();

                match_kind!(self.next(), TokenKind::Semicolon).unwrap();

                Stmt::Expr(expr)
            }
        };

        Some(stmt)
    }

    // <decl> ::= "int" <id> [ = <exp> ] ";"
    fn parse_decl(&mut self) -> Option<Decl<'a>> {
        let token = self.peek();
        //printfn!("{token:?}");

        match_str!(self.peek(), "int").and_then(|_| {
            self.next();

            let name = match_kind!(self.next(), TokenKind::Ident).unwrap();
            let value = match_kind!(self.peek(), TokenKind::Eq).and_then(|_| {
                self.next();
                let kexp = self.parse_expr(0).unwrap();

                Some(kexp)
            });

            match_kind!(self.next(), TokenKind::Semicolon).unwrap();

            let decl = Decl { name, value };

            Some(decl)
        })
    }

    // <block_item> ::= <stmt> | <decl>
    fn parse_block_item(&mut self) -> Option<BlockItem<'a>> {
        if let Some(decl) = self.parse_decl() {
            return Some(BlockItem::Decl(decl));
        }

        if let Some(stmt) = self.parse_stmt() {
            return Some(BlockItem::Stmt(stmt));
        }

        None
    }

    // <fn_def> ::= "int" <ident> "(" ")" "{" { <block-item> } "}"
    fn parse_fn_def(&mut self) -> Option<FnDef<'a>> {
        self.peek()?;
        match_str!(self.next(), "int").unwrap();

        let name = match_kind!(self.next(), TokenKind::Ident).unwrap();

        match_kind!(self.next(), TokenKind::OpenParen).unwrap();
        // TODO: args
        match_kind!(self.next(), TokenKind::CloseParen).unwrap();
        match_kind!(self.next(), TokenKind::OpenBrace).unwrap();

        let mut body = Vec::new();
        loop {
            match self.peek()? {
                Token {
                    kind: TokenKind::CloseBrace,
                    ..
                } => break,
                _ => {
                    let kblock_item = self.parse_block_item().unwrap();
                    body.push(kblock_item);
                }
            }
        }

        match_kind!(self.next(), TokenKind::CloseBrace).unwrap();

        Some(FnDef { name, body })
    }

    // <prog> ::= <fn_def>
    fn parse_prog(&mut self) -> Vec<FnDef<'a>> {
        vec![self.parse_fn_def().unwrap()]
    }
}

pub fn parse(tokens: Vec<Token>) -> Ast {
    let mut p = Parser::new(tokens);
    p.ast = Ast {
        defs: p.parse_prog(),
    };

    //#[cfg(debug_assertions)]
    //println!("\n# AST\n```rust\n{:#?}\n```\n", p.ast);

    p.ast
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::*;

    pub fn parse_expr_only(tokens: Vec<Token>) -> Ast {
        let mut p = Parser::new(tokens);
        let expr = p.parse_expr(0).unwrap();
        let block_item = BlockItem::Stmt(Stmt::Expr(expr));
        let name = Token {
            kind: TokenKind::Ident,
            value: "",
        };
        let body = vec![block_item];
        let defs = vec![FnDef { name, body }];
        p.ast = Ast { defs };

        p.ast
    }

    fn parse_sexpr(src: &str, indent: bool) -> String {
        let tokens = lex(src);
        let ast = parse_expr_only(tokens);
        ast.to_sexpr(indent)
    }

    fn cases<F: FnMut(&str, &str)>(mut f: F) {
        f("1 + 2 * 3", "(+ 1 (* 2 3))");
        f("1 * 2 + 3", "(+ (* 1 2) 3)");
        f("1 * 2 + 3 * 4", "(+ (* 1 2) (* 3 4))");
        f("1 + 2 + 3", "(+ (+ 1 2) 3)");
        f("f . g . h", "(. (. f g) h)");
        f(
            "1 + 2 + f . g . h * 3 * 4",
            "(+ (+ 1 2) (* (* (. (. f g) h) 3) 4))",
        );
        f("- -1 * 2", "(* (- (- 1)) 2)");
        f("- -f . g", "(- (- (. f g)))");
        f("-9++", "(- (++ 9))");
        f("f . g ++", "(++ (. f g))");
        f("(((0)))", "0");
        f("x[0][1]", "([ ([ x 0) 1)");
        f("a ? b : c ? d : e", "(? a b (? c d e))");
        //f("a = 0 ? b : c = d", "(= a (= (? 0 b c) d))")
        f("a = 0 ? b : (c = d)", "(= a (? 0 b (= c d)))");
    }

    #[test]
    fn test_expressions() {
        cases(|i, o| assert_eq!(parse_sexpr(i, false), o))
    }
}
