use crate::lex::*;

// #[derive(Debug)]
// pub enum ArithmeticOp {
//     Negation,
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

struct ParseContext<'a> {
    src: &'a str,
    tokens: std::vec::IntoIter<Token>,
    ast: Ast,
}

impl<'a> ParseContext<'a> {
    fn new(src: &'a str, tokens: Vec<Token>) -> ParseContext {
        ParseContext { src, tokens: tokens.into_iter(), ast: Ast::new() }
    }

    fn next_token(&mut self) -> Option<Token> {
        // `self.tokens` is an iterator, therefore .find() advances it up until that point
        self.tokens.find(|t| t.kind != TokenKind::Whitespace)
    }
}

fn parse_expression(ctx: &mut ParseContext, parent: AstKey) -> Option<AstKey> {
    let token = ctx.next_token()?;
    match token {
        Token { kind: TokenKind::Literal, .. } => {
            let const_exp = Expression::Constant(token.as_str(ctx.src).parse::<i64>().ok()?);
            let exp = AstData::Exp(const_exp);
            Some(ctx.ast.insert(parent, exp))
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

            let kexp = ctx.ast.insert(parent, exp?);
            let ksubexp = parse_expression(ctx, kexp)?;
            ctx.ast.get_mut(kexp).children.push(ksubexp);
            Some(kexp)
        }
    }
}

fn parse_statement(ctx: &mut ParseContext, parent: AstKey) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement());
    let kstmt = ctx.ast.insert(parent, stmt);

    let token = ctx.next_token()?;
    if !matches!(token.as_str(ctx.src), "return") {
        return None;
    }

    let kexp = parse_expression(ctx, kstmt)?;

    if !matches!(ctx.next_token()?.kind, TokenKind::Semicolon) {
        return None;
    }

    ctx.ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

fn parse_function(ctx: &mut ParseContext, parent: AstKey) -> Option<AstKey> {
    let token = ctx.next_token()?;
    if !matches!(token.as_str(ctx.src), "int") {
        return None;
    }

    let name = match ctx.next_token()? {
        token @ Token { kind: TokenKind::Ident, .. } => token.as_str(ctx.src).to_owned(),
        _ => return None,
    };

    let func = AstData::Func(Function(name));
    let kfunc = ctx.ast.insert(parent, func);

    if !matches!(ctx.next_token()?.kind, TokenKind::OpenParen) {
        return None;
    }

    if !matches!(ctx.next_token()?.kind, TokenKind::CloseParen) {
        return None;
    }

    if !matches!(ctx.next_token()?.kind, TokenKind::OpenBrace) {
        return None;
    }

    let kstmt = parse_statement(ctx, kfunc)?;

    if !matches!(ctx.next_token()?.kind, TokenKind::CloseBrace) {
        return None;
    }

    ctx.ast.get_mut(kfunc).children.push(kstmt);
    Some(kfunc)
}

fn parse_program(ctx: &mut ParseContext, parent: AstKey) -> Option<AstKey> {
    let prog = AstData::Prog(Program());
    let kprog = ctx.ast.insert(parent, prog);

    let funck = parse_function(ctx, kprog)?;

    ctx.ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(dbg: bool, src: &str, tokens: Vec<Token>) -> Option<Ast> {
    let mut ctx = ParseContext::new(src, tokens);
    parse_program(&mut ctx, 0)?;

    if dbg {
        println!("ast: \n{:?}\n", &ctx.ast);
    }

    Some(ctx.ast)
}
