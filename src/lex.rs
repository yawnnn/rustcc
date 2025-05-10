use std::{fmt::Debug, str::Chars};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace,
    Ident,
    Literal,
    OpenBrace,  // {
    CloseBrace, // }
    OpenParen,  // (
    CloseParen, // )
    Semicolon,  // ;
    Colon,      // :
    Minus,      // -
    Tilde,      // ~
    Not,        // !
    Neq,        // !=
    Plus,       // +
    Star,       // *
    Slash,      // /
    And,        // &
    AndAnd,     // &&
    Or,         // |
    OrOr,       // ||
    Eq,         // =
    EqEq,       // ==
    Lt,         // <
    LtLt,       // <<
    Leq,        // <=
    Gt,         // >
    GtGt,       // >>
    Geq,        // >=
    Percent,    // %
    Caret,      // ^
    Question,   // ?
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
    pub _pos: (usize, usize),
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, value: &'a str, _pos: (usize, usize)) -> Self {
        Token { kind, value, _pos }
    }
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.value)
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
    pos: (usize, usize),
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { chars: input.chars(), pos: (0, 0) }
    }

    fn next_char(&mut self) -> Option<char> {
        let next = self.chars.next();
        match next {
            Some('\n') => {
                self.pos.0 += 1;
                self.pos.1 = 0;
            }
            _ => self.pos.1 += 1,
        }
        next
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn advance_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek_char() {
            match predicate(c) {
                true => self.next_char(),
                false => break,
            };
        }
    }

    /// peek, run `predicate` and advance if Ok()
    /// returns inner result of `predicate` both if Ok() or Err()
    fn advance_if_ok(
        &mut self,
        mut predicate: impl FnMut(char) -> Result<TokenKind, TokenKind>,
    ) -> TokenKind {
        match predicate(self.peek_char().unwrap_or('\0')) {
            Ok(inner) => {
                self.next_char();
                inner
            }
            Err(inner) => inner,
        }
    }

    fn is_ident_start(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    fn is_ident(c: char) -> bool {
        c == '_' || c.is_alphanumeric()
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        use TokenKind::*;

        let start = self.chars.as_str();

        let kind = match self.next_char()? {
            '{' => OpenBrace,
            '}' => CloseBrace,
            '(' => OpenParen,
            ')' => CloseParen,
            ';' => Semicolon,
            ':' => Colon,
            '~' => Tilde,
            '!' => self.advance_if_ok(|c| match c {
                '=' => Ok(Neq),
                _ => Err(Not),
            }),
            '-' => Minus,
            '+' => Plus,
            '*' => Star,
            '/' => Slash,
            '&' => self.advance_if_ok(|c| match c {
                '&' => Ok(AndAnd),
                _ => Err(And),
            }),
            '|' => self.advance_if_ok(|c| match c {
                '|' => Ok(OrOr),
                _ => Err(Or),
            }),
            '=' => self.advance_if_ok(|c| match c {
                '=' => Ok(EqEq),
                _ => Err(Eq),
            }),
            '<' => self.advance_if_ok(|c| match c {
                '<' => Ok(LtLt),
                '=' => Ok(Leq),
                _ => Err(Lt),
            }),
            '>' => self.advance_if_ok(|c| match c {
                '>' => Ok(GtGt),
                '=' => Ok(Geq),
                _ => Err(Gt),
            }),
            '%' => Percent,
            '^' => Caret,
            '?' => Question,
            c if c.is_numeric() => {
                self.advance_while(char::is_numeric);
                Literal
            }
            c if Self::is_ident_start(c) => {
                self.advance_while(Self::is_ident);
                Ident
            }
            c if c.is_whitespace() => {
                self.advance_while(char::is_whitespace);
                Whitespace
            }
            _ => panic!("Unhandled token"),
        };

        let end = self.chars.as_str();
        let len = start.len() - end.len();
        let str = &start[..len];

        Some(Token::new(kind, str, self.pos))
    }

    // pub fn peek_token(&self) -> Option<(Token, &str)> {
    //     let start = self.chars.as_str();
    //     let (token, _) = self.clone().next()?;
    //     let len = token.len;

    //     Some((token, &start[..len]))
    // }

    pub fn into_iter(mut self) -> impl Iterator<Item = Token<'a>> + 'a {
        std::iter::from_fn(move || self.next_token())
    }
}

/// TODO: can't i ignore spaces here
pub fn lex(src: &str) -> Vec<Token> {
    let tokens = Lexer::new(src).into_iter().collect::<Vec<_>>();

    //#[cfg(debug_assertions)]
    //println!("\n# TOKENS\n```rust\n{tokens:#?}\n```\n");

    tokens
}
