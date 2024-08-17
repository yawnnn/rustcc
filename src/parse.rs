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

struct Parser<'a> {
    src: &'a str,
    tokens: std::vec::IntoIter<Token>,
    ast: Ast,
}

impl<'a> Parser<'a> {
    fn new(src: &'a str, tokens: Vec<Token>) -> Parser {
        Parser { src, tokens: tokens.into_iter(), ast: Ast::new() }
    }
}

fn parse_expression(parser: &mut Parser, parent: AstKey) -> Option<AstKey> {
    let token = parser.tokens.next()?;
    match token {
        Token { kind: TokenKind::Literal, .. } => {
            let exp = AstData::Exp(Expression::Constant(
                token.get_value(parser.src).parse::<i64>().ok()?,
            ));
            Some(parser.ast.insert(parent, exp))
        }
        Token { kind, .. } => {
            let unopkind = UnOpKind::try_from(&kind)?;
            let exp = AstData::Exp(Expression::UnOp(unopkind));
            let kexp = parser.ast.insert(parent, exp);
            let ksubexp = parse_expression(parser, kexp)?;
            parser.ast.get_mut(kexp).children.push(ksubexp);
            Some(kexp)
        }
    }
}

fn parse_statement(parser: &mut Parser, parent: AstKey) -> Option<AstKey> {
    let stmt = AstData::Stmt(Statement());
    let kstmt = parser.ast.insert(parent, stmt);

    let token = parser.tokens.next()?;
    if !matches!(token.get_value(parser.src), "return") {
        return None;
    }

    let kexp = parse_expression(parser, kstmt)?;

    if !matches!(parser.tokens.next()?.kind, TokenKind::Semicolon) {
        return None;
    }

    parser.ast.get_mut(kstmt).children.push(kexp);
    Some(kstmt)
}

fn parse_function(parser: &mut Parser, parent: AstKey) -> Option<AstKey> {
    let token = parser.tokens.next()?;
    if !matches!(token.get_value(parser.src), "int") {
        return None;
    }

    let name = match parser.tokens.next()? {
        token @ Token { kind: TokenKind::Ident, .. } => token.get_value(parser.src).to_owned(),
        _ => return None,
    };

    let func = AstData::Func(Function(name));
    let kfunc = parser.ast.insert(parent, func);

    if !matches!(parser.tokens.next()?.kind, TokenKind::OpenParen) {
        return None;
    }

    if !matches!(parser.tokens.next()?.kind, TokenKind::CloseParen) {
        return None;
    }

    if !matches!(parser.tokens.next()?.kind, TokenKind::OpenBrace) {
        return None;
    }

    let kstmt = parse_statement(parser, kfunc)?;

    if !matches!(parser.tokens.next()?.kind, TokenKind::CloseBrace) {
        return None;
    }

    parser.ast.get_mut(kfunc).children.push(kstmt);
    Some(kfunc)
}

fn parse_program(parser: &mut Parser, parent: AstKey) -> Option<AstKey> {
    let prog = AstData::Prog(Program());
    let kprog = parser.ast.insert(parent, prog);

    let funck = parse_function(parser, kprog)?;

    parser.ast.get_mut(kprog).children.push(funck);
    Some(kprog)
}

pub fn parse(dbg_mode: bool, src: &str, input: Vec<Token>) -> Option<Ast> {
    let mut parser = Parser::new(src, input);
    parse_program(&mut parser, 0)?;

    if dbg_mode {
        println!("ast: \n{:?}\n", &parser.ast);
    }

    Some(parser.ast)
}
