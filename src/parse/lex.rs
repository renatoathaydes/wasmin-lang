use crate::parse::model::Token;

pub struct Lexer<'s> {
    text: &'s str,
    lines: Vec<usize>,
    index: usize,
    len: usize,
}

impl<'s> Lexer<'s> {
    pub fn new(text: &'s str) -> Lexer<'s> {
        Lexer { text, lines: Vec::with_capacity(64), index: 0, len: text.len() }
    }

    pub fn next(&mut self) -> Option<Token> {
        let mut iter = self.text.chars().skip(self.index);
        let mut text_start = self.index;
        let mut whitespace = true;
        for c in iter {
            let boundary = self.text.is_char_boundary(self.index);
            self.advance(c, boundary);
            if !boundary { continue; }
            match c {
                // whitespace
                ' ' | '\t' | '\r' | '\n' => if whitespace {
                    text_start = self.index;
                } else {
                    self.index -= 1; // ignore the whitespace
                    return self.text_token(text_start);
                },
                // token separators
                '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':' | '=' => {
                    if text_start == self.index - 1 { // single char
                        return self.text_token(text_start);
                    }
                    // rewind to consume separator later, take the previous word first.
                    self.index -= 1;
                    return self.text_token(text_start);
                }
                _ => whitespace = false,
            }
        }
        if !whitespace { return self.text_token(text_start); }
        None
    }

    fn advance(&mut self, c: char, boundary: bool) {
        if !boundary && c == '\n' {
            self.lines.push(self.index);
        }
        self.index += 1;
    }

    fn text_token(&self, start: usize) -> Option<Token> {
        let slice = &self.text[start..self.index];
        let start = start + 1; // position is 1-indexed
        let token = match slice {
            "let" => Token::Let(start),
            "set" => Token::Set(start),
            "mut" => Token::Mut(start),
            "pub" => Token::Pub(start),
            "ext" => Token::Ext(start),
            ":" => Token::Colon(start),
            ";" => Token::SemiColon(start),
            "," => Token::Comma(start),
            "=" => Token::Eq(start),
            "(" => Token::OpenParens(start),
            ")" => Token::CloseParens(start),
            "[" => Token::OpenBracket(start),
            "]" => Token::CloseBracket(start),
            "{" => Token::OpenCurly(start),
            "}" => Token::CloseCurly(start),
            _ => Token::Id(start, slice.to_owned())
        };
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parens() {
        let mut lexer = Lexer::new("()");
        assert_eq!(lexer.next(), Some(Token::OpenParens(1)));
        assert_eq!(lexer.next(), Some(Token::CloseParens(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("( )");
        assert_eq!(lexer.next(), Some(Token::OpenParens(1)));
        assert_eq!(lexer.next(), Some(Token::CloseParens(3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("  (  )  ");
        assert_eq!(lexer.next(), Some(Token::OpenParens(3)));
        assert_eq!(lexer.next(), Some(Token::CloseParens(6)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_let() {
        let mut lexer = Lexer::new("let");
        assert_eq!(lexer.next(), Some(Token::Let(1)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new(" let ");
        assert_eq!(lexer.next(), Some(Token::Let(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new(" let let ");
        assert_eq!(lexer.next(), Some(Token::Let(2)));
        assert_eq!(lexer.next(), Some(Token::Let(6)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_set() {
        let mut lexer = Lexer::new("set");
        assert_eq!(lexer.next(), Some(Token::Set(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_mut() {
        let mut lexer = Lexer::new("mut");
        assert_eq!(lexer.next(), Some(Token::Mut(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_ext() {
        let mut lexer = Lexer::new("ext");
        assert_eq!(lexer.next(), Some(Token::Ext(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_pub() {
        let mut lexer = Lexer::new("pub");
        assert_eq!(lexer.next(), Some(Token::Pub(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_colon() {
        let mut lexer = Lexer::new(":");
        assert_eq!(lexer.next(), Some(Token::Colon(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_semi_colon() {
        let mut lexer = Lexer::new(";");
        assert_eq!(lexer.next(), Some(Token::SemiColon(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_comma() {
        let mut lexer = Lexer::new(",");
        assert_eq!(lexer.next(), Some(Token::Comma(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_eq() {
        let mut lexer = Lexer::new("=");
        assert_eq!(lexer.next(), Some(Token::Eq(1)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_various() {
        let mut lexer = Lexer::new("a,bb, ccc = foo");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(2)));
        assert_eq!(lexer.next(), Some(Token::Id(3, "bb".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(5)));
        assert_eq!(lexer.next(), Some(Token::Id(7, "ccc".into())));
        assert_eq!(lexer.next(), Some(Token::Eq(11)));
        assert_eq!(lexer.next(), Some(Token::Id(13, "foo".into())));
        assert_eq!(lexer.next(), None);
        assert_eq!(lexer.next(), None);
    }
}
