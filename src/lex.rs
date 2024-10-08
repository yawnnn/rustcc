use core::str;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Whitespace,
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
    Plus,
    Star,
    Slash,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    start: usize,
    end: usize,
}

impl Token {
    // `src` is the full src string
    pub fn as_str<'a>(&self, src: &'a str) -> &'a str {
        src.get(self.start..self.end).unwrap()
    }
}

struct Lexer<'a> {
    src: &'a str,
    chars: std::str::Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { src: input, chars: input.chars() }
    }

    fn next_char(&mut self) -> Option<char> {
        self.chars.next()
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

    fn get_pos_in_str(&self) -> usize {
        let start = self.src.as_ptr();
        let curr = self.chars.as_str().as_ptr();
        // SAFETY:
        // - both pointers are derived from the same slice
        // - start is always <= curr
        unsafe { curr.offset_from(start) as usize }
    }

    fn is_ident_start(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    fn is_ident(c: char) -> bool {
        c == '_' || c.is_alphanumeric()
    }

    fn next_token(&mut self) -> Option<Token> {
        use TokenKind::*;

        let start = self.get_pos_in_str();

        let kind = match self.next_char()? {
            '{' => OpenBrace,
            '}' => CloseBrace,
            '(' => OpenParen,
            ')' => CloseParen,
            ';' => Semicolon,
            '~' => Tilde,
            '!' => Bang,
            '-' => Minus,
            '+' => Plus,
            '*' => Star,
            '/' => Slash,
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

        let end = self.get_pos_in_str();
        Some(Token { kind, start, end })
    }
}

pub fn iter_tokens(src: &str) -> impl Iterator<Item = Token> + '_ {
    let mut lexer = Lexer::new(src);
    std::iter::from_fn(move || lexer.next_token())
}

pub fn lex(dbg: bool, src: &str) -> Vec<Token> {
    let tokens = iter_tokens(src).collect::<Vec<_>>();

    if dbg {
        println!("{tokens:?}");
    }

    tokens
}
