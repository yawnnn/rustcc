use std::{fmt::Debug, str::Chars};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Eof,

    Ident,
    Literal,
    OpenBrace,    // {
    CloseBrace,   // }
    OpenParen,    // (
    CloseParen,   // )
    OpenBracket,  // [
    CloseBracket, // ]
    Dot,          // .
    Comma,        // ,
    Semicolon,    // ;
    Colon,        // :
    Minus,        // -
    MinusMinus,   // --
    MinusEq,      // -=
    Arrow,        // ->
    Tilde,        // ~
    Bang,         // !
    BangEq,       // !=
    Plus,         // +
    PlusPlus,     // ++
    PlusEq,       // +=
    Star,         // *
    StarEq,       // *=
    Slash,        // /
    SlashEq,      // /=
    Amp,          // &
    AmpAmp,       // &&
    AmpEq,        // &=
    Pipe,         // |
    PipePipe,     // ||
    PipeEq,       // |=
    Eq,           // =
    EqEq,         // ==
    Lt,           // <
    LtLt,         // <<
    LtLtEq,       // <<=
    Leq,          // <=
    Gt,           // >
    GtGt,         // >>
    GtGtEq,       // >>=
    Geq,          // >=
    Percent,      // %
    PercentEq,    // %=
    Caret,        // ^
    CaretEq,      // ^=
    Question,     // ?
}

#[derive(Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
    pub _pos: usize,
}

impl Debug for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.value)
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
    pos: usize,
    token_beg: usize,
}

impl<'a> Lexer<'a> {
    fn new(src: &'a str) -> Lexer<'a> {
        Lexer {
            src,
            chars: src.chars(),
            pos: 0,
            token_beg: 0,
        }
    }

    fn bump(&mut self) -> Option<char> {
        self.pos += 1;
        self.chars.next()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn beg_token(&mut self) {
        self.token_beg = self.pos;
    }

    fn end_token(&mut self, kind: TokenKind) -> Token<'a> {
        Token {
            kind,
            value: &self.src[self.token_beg..self.pos],
            _pos: self.token_beg,
        }
    }

    /// peek, run `predicate` and advance if Ok()
    /// returns inner result of `predicate` both if Ok() or Err()
    fn bump_if_ok(
        &mut self,
        mut predicate: impl FnMut(&mut Self, char) -> Result<TokenKind, TokenKind>,
    ) -> TokenKind {
        match predicate(self, self.peek().unwrap_or('\0')) {
            Ok(kind) => {
                self.bump();
                kind
            }
            Err(kind) => kind,
        }
    }

    fn bump_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while let Some(c) = self.peek()
            && predicate(c)
        {
            self.bump();
        }
    }

    fn is_ident_start(c: char) -> bool {
        c == '_' || c.is_alphabetic()
    }

    fn is_ident(c: char) -> bool {
        c == '_' || c.is_alphanumeric()
    }

    pub fn next(&mut self) -> Token<'a> {
        use TokenKind as K;

        self.beg_token();

        let Some(c) = self.bump() else {
            return self.end_token(K::Eof);
        };

        let kind = match c {
            '{' => K::OpenBrace,
            '}' => K::CloseBrace,
            '(' => K::OpenParen,
            ')' => K::CloseParen,
            '[' => K::OpenBracket,
            ']' => K::CloseBracket,
            '.' => K::Dot,
            ',' => K::Comma,
            ';' => K::Semicolon,
            ':' => K::Colon,
            '~' => K::Tilde,
            '!' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::BangEq),
                _ => Err(K::Bang),
            }),
            '-' => self.bump_if_ok(|_, c| match c {
                '-' => Ok(K::MinusMinus),
                '=' => Ok(K::MinusEq),
                '>' => Ok(K::Arrow),
                _ => Err(K::Minus),
            }),
            '+' => self.bump_if_ok(|_, c| match c {
                '+' => Ok(K::PlusPlus),
                '=' => Ok(K::PlusEq),
                _ => Err(K::Plus),
            }),
            '*' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::StarEq),
                _ => Err(K::Star),
            }),
            '/' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::SlashEq),
                _ => Err(K::Slash),
            }),
            '&' => self.bump_if_ok(|_, c| match c {
                '&' => Ok(K::AmpAmp),
                '=' => Ok(K::AmpEq),
                _ => Err(K::Amp),
            }),
            '|' => self.bump_if_ok(|_, c| match c {
                '|' => Ok(K::PipePipe),
                '=' => Ok(K::PipeEq),
                _ => Err(K::Pipe),
            }),
            '=' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::EqEq),
                _ => Err(K::Eq),
            }),
            '<' => self.bump_if_ok(|self_, c| match c {
                '<' => {
                    let k = self_.bump_if_ok(|_, c| match c {
                        '=' => Ok(K::LtLtEq),
                        _ => Err(K::LtLt),
                    });

                    Ok(k)
                }
                '=' => Ok(K::Leq),
                _ => Err(K::Lt),
            }),
            '>' => self.bump_if_ok(|self_, c| match c {
                '>' => {
                    let k = self_.bump_if_ok(|_, c| match c {
                        '=' => Ok(K::GtGtEq),
                        _ => Err(K::GtGt),
                    });

                    Ok(k)
                }
                '=' => Ok(K::Geq),
                _ => Err(K::Gt),
            }),
            '%' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::PercentEq),
                _ => Err(K::Percent),
            }),
            '^' => self.bump_if_ok(|_, c| match c {
                '=' => Ok(K::CaretEq),
                _ => Err(K::Caret),
            }),
            '?' => K::Question,
            c if c.is_numeric() => {
                self.bump_while(char::is_numeric);
                K::Literal
            }
            c if Self::is_ident_start(c) => {
                self.bump_while(Self::is_ident);
                K::Ident
            }
            c if c.is_whitespace() => {
                self.bump_while(char::is_whitespace);
                self.beg_token();
                return self.next();
            }
            _ => panic!("Unhandled token"),
        };

        self.end_token(kind)
    }

    pub fn into_iter(mut self) -> impl Iterator<Item = Token<'a>> {
        std::iter::from_fn(move || match self.next() {
            Token {
                kind: TokenKind::Eof,
                ..
            } => None,
            t => Some(t),
        })
    }
}

/// TODO: can't i ignore spaces here
pub fn lex(src: &str) -> Vec<Token<'_>> {
    let tokens = Lexer::new(src).into_iter().collect::<Vec<_>>();

    //#[cfg(debug_assertions)]
    //println!("\n# TOKENS\n```rust\n{tokens:#?}\n```\n");

    #[allow(clippy::let_and_return)]
    tokens
}
