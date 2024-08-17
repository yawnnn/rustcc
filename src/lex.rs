use core::str;
use std::str::Chars;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Literal,
    Semicolon,
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Minus,
    Tilde,
    Bang,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    start: usize,
    end: usize,
}

impl Token {
    // `input` is the original string
    pub fn as_str<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..self.end]
    }
}

struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    remaining: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { src: input, chars: input.chars(), remaining: input.len() }
    }

    fn next_token(&mut self) -> Option<Token> {
        use TokenKind::*;

        let start = self.get_offset_from_start();

        let kind = match self.next_char()? {
            '{' => OpenBrace,
            '}' => CloseBrace,
            '(' => OpenParen,
            ')' => CloseParen,
            ';' => Semicolon,
            '~' => Tilde,
            '!' => Bang,
            '-' => Minus,
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
                return self.next_token();
            }
            _ => panic!("Unhandled token"),
        };

        let end = self.get_offset_from_start();
        Some(Token { kind, start, end })
    }

    fn get_offset_from_start(&self) -> usize {
        // SAFETY: both pointers are derived from the same slice
        let start = self.src.as_ptr();
        let pos = self.chars.as_str().as_ptr();
        (unsafe { pos.offset_from(start) }) as usize
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.remaining -= 1;
        Some(c)
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn advance_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek() {
            match c {
                c if predicate(c) => self.next_char(),
                _ => break,
            };
        }
    }

    fn is_ident_start(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    fn is_ident(c: char) -> bool {
        c == '_' || c.is_alphanumeric()
    }
}

// temporary
pub fn lex(dbg: bool, src: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(src);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }

    if dbg {
        println!("{tokens:?}");
    }

    tokens
}
