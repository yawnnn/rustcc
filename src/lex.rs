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
    pub value: String
}

struct Lexer<'a> {
    chars: Chars<'a>,
    len: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { chars: input.chars(), len: input.len() }
    }

    fn next_token(&mut self) -> Option<Token> {
        use TokenKind::*;

        let mut start = self.chars.as_str();

        let kind = match self.next_char()? {
            '{' => OpenBrace,
            '}' => CloseBrace,
            '(' => OpenParen,
            ')' => CloseParen,
            ';' => Semicolon,
            '~' => Tilde,
            '!' => Bang,
            '-' => Minus,
            c if c.is_whitespace() => {
                while let Some(c) = self.peek() {
                    match c {
                        c if c.is_whitespace() => self.next_char(),
                        _ => break,
                    };
                }
                start = self.chars.as_str();
                self.next_token()?.kind
            }
            c if c.is_numeric() => {
                self.advance_while(char::is_numeric);
                Literal
            }
            c if Self::is_ident_start(c) => {
                self.advance_while(Self::is_ident);
                Ident
            },
            _ => panic!("Unhandled token"),
        };

        // temporary
        let value = &start[..start.len() - self.len];
        Some(Token { kind, value: value.to_owned() })
    }

    fn next_char(&mut self) -> Option<char> {
        let c = self.chars.next()?;
        self.len -= 1;
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
pub fn lex(input: &str) -> Vec<Token> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();

    while let Some(token) = lexer.next_token() {
        tokens.push(token);
    }

    tokens
}
