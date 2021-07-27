use std::fmt::{Display, Formatter};
use std::fs;

use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use crate::errors::WasminError;

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression<'s> {
    Empty,
    Simple(&'s str),
    Group(Vec<Expression<'s>>),
    Multi(Vec<Expression<'s>>),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
enum NestingElement {
    Parens,
    Square,
    Curly,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
struct NestingToken {
    elem: NestingElement,
    pos: (usize, usize),
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
struct LexerState {
    nesting: Vec<NestingToken>,
    naked_level: Vec<bool>,
    line: usize,
    col: usize,
}

impl LexerState {
    fn pos(&self) -> (usize, usize) {
        (self.line, self.col)
    }
}

impl Display for NestingElement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            NestingElement::Parens => '(',
            NestingElement::Square => '[',
            NestingElement::Curly => '{',
        })
    }
}

impl Default for LexerState {
    fn default() -> Self {
        LexerState {
            nesting: vec![],
            naked_level: vec![],
            line: 1,
            col: 0,
        }
    }
}

impl NestingToken {
    fn closing_char(&self) -> char {
        match self.elem {
            NestingElement::Parens => ')',
            NestingElement::Square => ']',
            NestingElement::Curly => '}',
        }
    }

    fn pos_str(&self) -> String {
        format!("{}:{}", self.pos.0, self.pos.1)
    }
}

macro_rules! start_nesting {
    ($e:expr, $tokens:expr, $state:expr, $exprs:expr) => {{
        $state.nesting.push(NestingToken { elem: $e, pos: $state.pos() });
        $exprs.push(lexer($tokens, $state)?);
        if $state.nesting.is_empty() { break; }
    }};
}

fn lexer<'s>(tokens: &mut UWordBounds<'s>, state: &mut LexerState) -> Result<Expression<'s>, WasminError> {
    // let file = fs::read_to_string("ex.wasmin").unwrap();
    // let tokens = UnicodeSegmentation::split_word_bounds(&file);
    let mut exprs = Vec::<Expression>::new();
    loop {
        let token;
        if let Some(t) = tokens.next() {
            token = t;
        } else {
            break;
        }
        if token == "\n" || token == "\r\n" {
            state.line += 1;
            state.col = 0;
        } else {
            state.col += token.len()
        }
        let token = token.trim();
        if token.is_empty() { continue; }
        match token {
            "(" => start_nesting!(NestingElement::Parens, tokens, state, exprs),
            "[" => start_nesting!(NestingElement::Square, tokens, state, exprs),
            "{" => start_nesting!(NestingElement::Curly, tokens, state, exprs),
            ")" => {
                if let Some(n) = state.nesting.pop() {
                    return if n.elem == NestingElement::Parens {
                        Ok(join_exprs(exprs))
                    } else {
                        Err(werr_syntax!(format!("misplaced ')', expecting '{}', which started at {}",
                            n.closing_char(), n.pos_str()), state.pos()))
                    };
                } else {
                    return Err(werr_syntax!("misplaced ')', closing nothing", state.pos()));
                };
            }
            _ => exprs.push(Expression::Simple(token)),
        };
    }
    // tokens ended, emit expression if nothing is waiting to be closed
    if state.nesting.is_empty() {
        Ok(join_exprs(exprs))
    } else {
        let last_n = state.nesting.last().unwrap();
        Err(werr_syntax!(format!("unexpected EOF, unmatched '{}', which started at {}",
                last_n.elem, last_n.pos_str()), state.pos()))
    }
}

fn join_exprs(mut exprs: Vec<Expression>) -> Expression {
    match exprs.len() {
        0 => Expression::Empty,
        1 => exprs.remove(0),
        _ => Expression::Group(exprs)
    }
}

#[cfg(test)]
mod tests {
    use unicode_segmentation::UnicodeSegmentation;

    use crate::lexer::Expression;

    use super::*;

    macro_rules! group_expr {
        ($($a:expr), +) => {Expression::Group(vec![ $( $a ), + ])};
    }

    macro_rules! empty_expr {
        () => {Expression::Empty};
    }

    macro_rules! str_expr {
        ($e:literal) => {Expression::Simple($e)};
    }

    macro_rules! lex {
        ($input:literal) => {{
            let mut state = LexerState::default();
            lexer(&mut $input.split_word_bounds(), &mut state)
        }};
    }

    macro_rules! assert_ok {
        ($left:expr, $right:expr) => { assert_eq!($left, Ok($right)) };
    }

    #[test]
    fn test_empty_expr() {
        assert_ok!(lex!("()"), empty_expr!());
        assert_ok!(lex!("( )"), empty_expr!());
        assert_ok!(lex!("  (    )   "), empty_expr!());
    }

    #[test]
    fn test_simple_expr() {
        assert_ok!(lex!("hello"), str_expr!("hello"));
        assert_ok!(lex!("(hello_world)"), str_expr!("hello_world"));
    }

    #[test]
    fn test_group_expr() {
        assert_ok!(lex!("(foo bar)"), group_expr!(str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("(+ 1 2 3)"),
                   group_expr!(str_expr!("+"), str_expr!("1"), str_expr!("2"), str_expr!("3")));
    }

    #[test]
    fn test_nested_group_expr() {
        assert_ok!(lex!("foo(bar)"), group_expr!(str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   group_expr!(str_expr!("+"),
                       group_expr!(str_expr!("1"),
                           group_expr!(str_expr!("2"), str_expr!("3")))));
    }

    #[test]
    fn test_iterator_can_be_reused_and_pos() {
        let mut state = LexerState::default();
        let mut iter = "(a)\n(b)".split_word_bounds();
        assert_ok!(lexer(&mut iter, &mut state), str_expr!("a"));
        assert_ok!(lexer(&mut iter, &mut state), str_expr!("b"));
        assert_eq!(state.pos(), (2, 3));
    }

    #[test]
    fn test_new_lines_pos() {
        let mut state = LexerState::default();
        let mut iter = "\n \r\n foo\nb a r".split_word_bounds();
        assert_ok!(lexer(&mut iter, &mut state),
            group_expr!(str_expr!("foo"), str_expr!("b"), str_expr!("a"), str_expr!("r")));
        assert_eq!(state.pos(), (4, 5));
    }
}