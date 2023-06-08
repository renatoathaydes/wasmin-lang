use std::str::Chars;

use crate::parse::model::{Position, Token};
use crate::parse::number::number;

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

    pub fn pos(&self) -> Position {
        self.index + 1
    }

    pub fn next(&mut self) -> Option<Token> {
        let mut iter = self.text[self.index..].chars();
        let mut text_start = self.index;
        let mut whitespace = true;
        for c in iter {
            self.advance(c);
            match c {
                // whitespace
                ' ' | '\t' | '\r' | '\n' => if whitespace {
                    text_start = self.index;
                } else {
                    self.index -= 1; // ignore the whitespace
                    return self.text_token(text_start);
                },
                // token separators
                '(' | ')' | '[' | ']' | '{' | '}' | ',' | ';' | ':' | '=' | '"' | '#' => {
                    if text_start == self.index - 1 { // single char
                        return match c {
                            // string
                            '"' => self.string(),
                            // comments
                            '#' => self.take_to_end_of_line(),
                            // anything else is treated as text/number
                            _ => self.text_token(text_start)
                        };
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

    fn advance(&mut self, c: char) {
        let next_index = self.index + c.len_utf8();
        // check if index is still the same as the previous one, which can happen because
        // the index may rewind by 1.
        if c == '\n' && *self.lines.last().unwrap_or(&0) != next_index {
            self.lines.push(next_index);
        }
        self.index = next_index;
    }

    fn string(&mut self) -> Option<Token> {
        let mut iter = self.text[self.index..].chars();
        let mut text_start = self.index;
        let mut escape = false;
        let mut escape_indexes: Vec<usize> = vec![];
        for c in iter {
            self.advance(c);
            if !escape && c == '\\' {
                escape = true;
                continue;
            }
            if c == '"' {
                if escape { // only " is escaped for now
                    escape_indexes.push(self.index - 2);
                } else {
                    let text = self.make_string(text_start, escape_indexes, false);
                    return Some(Token::Str(text_start, text));
                }
            }
            escape = false;
        }
        Some(Token::Error(self.index,
                          format!("Reached end of file without closing string started at {}",
                                  text_start)))
    }

    fn take_to_end_of_line(&mut self) -> Option<Token> {
        let mut iter = self.text[self.index..].chars();
        let start = self.index;
        let mut is_eof = true;
        for c in iter {
            self.advance(c);
            if c == '\n' {
                is_eof = false;
                break;
            }
        }
        let text = self.make_string(start, vec![], is_eof);
        return Some(Token::Comment(start, text));
    }

    fn make_string(&self, start: usize, escapes: Vec<usize>, include_last: bool) -> String {
        let last_index = self.index - if include_last { 0 } else { 1 };
        if escapes.is_empty() {
            self.text[start..last_index].to_owned()
        } else {
            let mut builder = String::with_capacity(self.index - start);
            let mut index = start;
            for escape_index in escapes {
                builder.push_str(&self.text[index..escape_index]);
                index = escape_index + 1;
            }
            builder.push_str(&self.text[index..last_index]);
            builder
        }
    }

    fn text_token(&self, start: usize) -> Option<Token> {
        let slice = &self.text[start..self.index];
        let start = start + 1; // position is 1-indexed
        if let Some(c) = slice.chars().next() {
            if c.is_digit(10) {
                return Some(number(slice, start));
            }
        }
        let token = match slice {
            "let" => Token::Let(start),
            "set" => Token::Set(start),
            "mut" => Token::Mut(start),
            "pub" => Token::Pub(start),
            "ext" => Token::Ext(start),
            "fun" => Token::Fun(start),
            "if" => Token::If(start),
            "then" => Token::Then(start),
            "else" => Token::Else(start),
            "loop" => Token::Loop(start),
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
    use crate::parse::model::Numeric;

    use super::*;

    #[test]
    fn test_parens_brackets_curlies() {
        let mut lexer = Lexer::new("()");
        assert_eq!(lexer.next(), Some(Token::OpenParens(1)));
        assert_eq!(lexer.next(), Some(Token::CloseParens(2)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("[ ]");
        assert_eq!(lexer.next(), Some(Token::OpenBracket(1)));
        assert_eq!(lexer.next(), Some(Token::CloseBracket(3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("  {  }  ");
        assert_eq!(lexer.next(), Some(Token::OpenCurly(3)));
        assert_eq!(lexer.next(), Some(Token::CloseCurly(6)));
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
    fn test_fun() {
        let mut lexer = Lexer::new("fun");
        assert_eq!(lexer.next(), Some(Token::Fun(1)));
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
    fn test_id() {
        let mut lexer = Lexer::new("a");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("a->b");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a->b".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("is?!");
        assert_eq!(lexer.next(), Some(Token::Id(1, "is?!".into())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_various() {
        let mut lexer = Lexer::new("a,bb, ccc = foo , bar");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(2)));
        assert_eq!(lexer.next(), Some(Token::Id(3, "bb".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(5)));
        assert_eq!(lexer.next(), Some(Token::Id(7, "ccc".into())));
        assert_eq!(lexer.next(), Some(Token::Eq(11)));
        assert_eq!(lexer.next(), Some(Token::Id(13, "foo".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(17)));
        assert_eq!(lexer.next(), Some(Token::Id(19, "bar".into())));
        assert_eq!(lexer.next(), None);
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_groups() {
        let mut lexer = Lexer::new("(a)");
        assert_eq!(lexer.next(), Some(Token::OpenParens(1)));
        assert_eq!(lexer.next(), Some(Token::Id(2, "a".into())));
        assert_eq!(lexer.next(), Some(Token::CloseParens(3)));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("a , b; c(d)");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(3)));
        assert_eq!(lexer.next(), Some(Token::Id(5, "b".into())));
        assert_eq!(lexer.next(), Some(Token::SemiColon(6)));
        assert_eq!(lexer.next(), Some(Token::Id(8, "c".into())));
        assert_eq!(lexer.next(), Some(Token::OpenParens(9)));
        assert_eq!(lexer.next(), Some(Token::Id(10, "d".into())));
        assert_eq!(lexer.next(), Some(Token::CloseParens(11)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_count_lines() {
        let mut lexer = Lexer::new("let a = b;\nmut x\n end \n foo  ");
        assert_eq!(lexer.next(), Some(Token::Let(1)));
        assert_eq!(lexer.next(), Some(Token::Id(5, "a".into())));
        assert_eq!(lexer.next(), Some(Token::Eq(7)));
        assert_eq!(lexer.next(), Some(Token::Id(9, "b".into())));
        assert_eq!(lexer.next(), Some(Token::SemiColon(10)));
        assert_eq!(lexer.next(), Some(Token::Mut(12)));
        assert_eq!(lexer.next(), Some(Token::Id(16, "x".into())));
        assert_eq!(lexer.next(), Some(Token::Id(19, "end".into())));
        assert_eq!(lexer.next(), Some(Token::Id(25, "foo".into())));
        assert_eq!(lexer.next(), None);

        assert_eq!(lexer.lines, vec![11, 17, 23]);
        assert_eq!(lexer.index, 29);
        assert_eq!(lexer.len, 29);
    }

    #[test]
    fn test_string() {
        let mut lexer = Lexer::new("\"\"");
        assert_eq!(lexer.next(), Some(Token::Str(1, "".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("\"abc def\"");
        assert_eq!(lexer.next(), Some(Token::Str(1, "abc def".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("\"abc\ndef\"\n\"ghi\" abc d");
        assert_eq!(lexer.next(), Some(Token::Str(1, "abc\ndef".into())));
        assert_eq!(lexer.next(), Some(Token::Str(11, "ghi".into())));
        assert_eq!(lexer.next(), Some(Token::Id(17, "abc".into())));
        assert_eq!(lexer.next(), Some(Token::Id(21, "d".into())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_string_escaping() {
        let mut lexer = Lexer::new("\"\\\"\"");
        assert_eq!(lexer.next(), Some(Token::Str(1, "\"".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("\"a\\\"bcd\\\"done\"");
        assert_eq!(lexer.next(), Some(Token::Str(1, "a\"bcd\"done".into())));
        assert_eq!(lexer.next(), None);

        // for now, escapes don't work except for string double-quotes
        let mut lexer = Lexer::new("a\\b");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a\\b".into())));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_utf8() {
        let mut lexer = Lexer::new("fire 🔥 \"fruit 🍉\" surf🏄;");
        assert_eq!(lexer.next(), Some(Token::Id(1, "fire".into())));
        assert_eq!(lexer.next(), Some(Token::Id(6, "🔥".into())));
        assert_eq!(lexer.next(), Some(Token::Str(11, "fruit 🍉".into())));
        assert_eq!(lexer.next(), Some(Token::Id(24, "surf🏄".into())));
        assert_eq!(lexer.next(), Some(Token::SemiColon(32)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn test_comment() {
        let mut lexer = Lexer::new("a\n#b()comment\nc");
        assert_eq!(lexer.next(), Some(Token::Id(1, "a".into())));
        assert_eq!(lexer.next(), Some(Token::Comment(3, "b()comment".into())));
        assert_eq!(lexer.next(), Some(Token::Id(15, "c".into())));
        assert_eq!(lexer.next(), None);

        let mut lexer = Lexer::new("hi# ignore\nok  #comment");
        assert_eq!(lexer.next(), Some(Token::Id(1, "hi".into())));
        assert_eq!(lexer.next(), Some(Token::Comment(3, " ignore".into())));
        assert_eq!(lexer.next(), Some(Token::Id(12, "ok".into())));
        assert_eq!(lexer.next(), Some(Token::Comment(16, "comment".into())));
        assert_eq!(lexer.next(), None);
    }

    /// numbers are mostly tested in the number.rs file
    #[test]
    fn test_number() {
        let mut lexer = Lexer::new("let x = 10i32");
        assert_eq!(lexer.next(), Some(Token::Let(1)));
        assert_eq!(lexer.next(), Some(Token::Id(5, "x".into())));
        assert_eq!(lexer.next(), Some(Token::Eq(7)));
        assert_eq!(lexer.next(), Some(Token::Number(9, Numeric::I32(10))));

        let mut lexer = Lexer::new("set x = ten, mul 42_i64");
        assert_eq!(lexer.next(), Some(Token::Set(1)));
        assert_eq!(lexer.next(), Some(Token::Id(5, "x".into())));
        assert_eq!(lexer.next(), Some(Token::Eq(7)));
        assert_eq!(lexer.next(), Some(Token::Id(9, "ten".into())));
        assert_eq!(lexer.next(), Some(Token::Comma(12)));
        assert_eq!(lexer.next(), Some(Token::Id(14, "mul".into())));
        assert_eq!(lexer.next(), Some(Token::Number(18, Numeric::I64(42))));
    }
}
