use regex::Regex;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum KeywordKind {
    Return,
    Int,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Ident,
    Keyword(KeywordKind),
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

pub struct Token {
    pub kind: TokenKind,
    pub value: String,
}

pub fn lex(input: &str) -> Vec<Token> {
    use TokenKind::*;

    #[rustfmt::skip]
    let patterns = [
        (OpenBrace,                     r"\{"),
        (CloseBrace,                    r"\}"),
        (OpenParen,                     r"\("),
        (CloseParen,                    r"\)"),
        (Semicolon,                     r";"),
        (Keyword(KeywordKind::Int),     r"int\s"),
        (Keyword(KeywordKind::Return),  r"return\s"),
        (Ident,                         r"[a-zA-Z]\w*"),
        (Literal,                       r"[0-9]+"),
        (Minus,                         r"-"),
        (Tilde,                         r"~"),
        (Bang,                          r"!"),
    ];

    let patterns =
        patterns.into_iter().map(|(t, p)| (t, Regex::new(p).unwrap())).collect::<Vec<_>>();

    let mut input = input.trim();
    let mut tokens = Vec::new();
    let mut old_len = 0;

    while input.len() != old_len {
        old_len = input.len();

        for (token_kind, pattern) in patterns.iter() {
            if let Some(m) = pattern.find(input) {
                if m.start() == 0 {
                    tokens.push(Token { kind: *token_kind, value: m.as_str().to_owned() });
                    input = input[m.len()..].trim();
                }
            }
        }
    }

    //println!("tokens: \n{tokens:?}\n");

    tokens
}
