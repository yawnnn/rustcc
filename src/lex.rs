use std::str::Chars;

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
}

#[derive(Debug, Clone, Copy)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub value: &'a str,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, value: &'a str) -> Self {
        Token { kind, value }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { chars: input.chars() }
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

    /// if `predicate` returns something, advance `self` and return that. otherwise return `default`
    fn predicate_or(
        &mut self,
        mut predicate: impl FnMut(char) -> Option<TokenKind>,
        default: TokenKind,
    ) -> TokenKind {
        let mut inner = || {
            let kind = predicate(self.peek_char()?)?;
            self.next_char();
            Some(kind)
        };

        inner().unwrap_or(default)
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
            '~' => Tilde,
            '!' => self.predicate_or(
                |c| match c {
                    '=' => Some(Neq),
                    _ => None,
                },
                Not,
            ),
            '-' => Minus,
            '+' => Plus,
            '*' => Star,
            '/' => Slash,
            '&' => self.predicate_or(
                |c| match c {
                    '&' => Some(AndAnd),
                    _ => None,
                },
                And,
            ),
            '|' => self.predicate_or(
                |c| match c {
                    '|' => Some(OrOr),
                    _ => None,
                },
                Or,
            ),
            '=' => self.predicate_or(
                |c| match c {
                    '=' => Some(EqEq),
                    _ => None,
                },
                Eq,
            ),
            '<' => self.predicate_or(
                |c| match c {
                    '<' => Some(LtLt),
                    '=' => Some(Leq),
                    _ => None,
                },
                Lt,
            ),
            '>' => self.predicate_or(
                |c| match c {
                    '>' => Some(GtGt),
                    '=' => Some(Geq),
                    _ => None,
                },
                Gt,
            ),
            '%' => Percent,
            '^' => Caret,
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

        Some(Token::new(kind, str))
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

pub fn lex(src: &str) -> Vec<Token> {
    let tokens = Lexer::new(src).into_iter().collect::<Vec<_>>();

    //#[cfg(debug_assertions)]
    //println!("\n// TOKENS //\n{tokens:#?}\n");

    tokens
}
