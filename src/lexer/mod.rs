use std::fmt::{Display, Formatter};
use std::fs;

use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use crate::errors::WasminError;

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression<'s> {
    Empty(Option<NestingElement>),
    Simple(&'s str, Option<NestingElement>),
    Group(Vec<Expression<'s>>, Option<NestingElement>),
    Multi(Vec<Expression<'s>>, Option<NestingElement>),
}

impl<'s> Expression<'s> {
    fn nesting(self, nesting: Option<NestingElement>) -> Expression<'s> {
        // only replace the nesting if one is given
        if let None = nesting {
            return self;
        }
        match self {
            Expression::Empty(_) => Expression::Empty(nesting),
            Expression::Simple(t, _) => Expression::Simple(t, nesting),
            Expression::Group(e, _) => Expression::Group(e, nesting),
            Expression::Multi(e, _) => Expression::Multi(e, nesting)
        }
    }
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum NestingElement {
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
    ($e:expr, $tokens:expr, $state:expr, $exprs:expr, $nested:expr) => {{
        // remember if this expression started with nesting as if it did, we must terminate it
        let terminate = if $nested.is_none() { $nested = Some(Some($e)); true } else { false };
        $state.nesting.push(NestingToken { elem: $e, pos: $state.pos() });
        $exprs.push(lexer($tokens, $state)?);
        if terminate { break; }
    }};
}

macro_rules! end_nesting {
    ($e:expr, $token:expr, $tokens:expr, $state:expr, $exprs:expr) => {{
        return if let Some(n) = $state.nesting.pop() {
            if n.elem == $e {
                Ok(join_exprs($exprs, Some(n.elem)))
            } else {
                Err(werr_syntax!(format!("misplaced '{}', expecting '{}', which started at {}",
                    $token, n.closing_char(), n.pos_str()), $state.pos()))
            }
        } else {
            Err(werr_syntax!(format!("misplaced '{}', closing nothing", $token), $state.pos()))
        }
    }};
}

fn lexer<'s>(tokens: &mut UWordBounds<'s>, state: &mut LexerState) -> Result<Expression<'s>, WasminError> {
    // let file = fs::read_to_string("ex.wasmin").unwrap();
    // let tokens = UnicodeSegmentation::split_word_bounds(&file);
    let mut exprs = Vec::<Expression>::new();
    let mut eof = true;
    // This must be set on the first non-empty token so we know when to terminate the expression.
    // If '(', '[' or '{' start the expression, then we terminate it as soon as we handle the
    // nesting, otherwise we go all the way down to either ';' or EOF.
    let mut nested: Option<Option<NestingElement>> = None;
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
        if !matches!(token, "(" | "[" | "{") { nested = Some(None); }
        match token {
            "(" => start_nesting!(NestingElement::Parens, tokens, state, exprs, nested),
            "[" => start_nesting!(NestingElement::Square, tokens, state, exprs, nested),
            "{" => start_nesting!(NestingElement::Curly, tokens, state, exprs, nested),
            ")" => end_nesting!(NestingElement::Parens, token, tokens, state, exprs),
            "]" => end_nesting!(NestingElement::Square, token, tokens, state, exprs),
            "}" => end_nesting!(NestingElement::Curly, token, tokens, state, exprs),
            ";" => {
                eof = false;
                break;
            }
            "," => todo!(),
            _ => exprs.push(Expression::Simple(token, None)),
        };
    }
    Ok(join_exprs(exprs, nested.unwrap_or(None)))
    // } else {
    //     let last_n = state.nesting.last().unwrap();
    //     let err = if eof { "EOF" } else { "';'" };
    //     Err(werr_syntax!(format!("unexpected {}, unmatched '{}', which started at {}",
    //             err , last_n.elem, last_n.pos_str()), state.pos()))
    // }
}

fn join_exprs(mut exprs: Vec<Expression>, nesting: Option<NestingElement>) -> Expression {
    match exprs.len() {
        0 => Expression::Empty(nesting),
        1 => exprs.remove(0).nesting(nesting),
        _ => Expression::Group(exprs, nesting)
    }
}

#[cfg(test)]
mod tests {
    use unicode_segmentation::UnicodeSegmentation;

    use crate::lexer::Expression;

    use super::*;

    macro_rules! group_expr {
        ($($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], None)};
        (p $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(NestingElement::Parens))};
        (s $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(NestingElement::Square))};
        (c $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(NestingElement::Curly))};
    }

    macro_rules! empty_expr {
        () => {Expression::Empty(None)};
        (p) => {Expression::Empty(Some(NestingElement::Parens))};
        (s) => {Expression::Empty(Some(NestingElement::Square))};
        (c) => {Expression::Empty(Some(NestingElement::Curly))};
    }

    macro_rules! str_expr {
        ($e:literal) => {Expression::Simple($e, None)};
        (p $e:literal) => {Expression::Simple($e, Some(NestingElement::Parens))};
        (s $e:literal) => {Expression::Simple($e, Some(NestingElement::Square))};
        (c $e:literal) => {Expression::Simple($e, Some(NestingElement::Curly))};
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
        assert_ok!(lex!(""), empty_expr!());
        assert_ok!(lex!("()"), empty_expr!(p));
        assert_ok!(lex!("( )"), empty_expr!(p));
        assert_ok!(lex!("  [    ]   "), empty_expr!(s));
        assert_ok!(lex!("  \n{ \n  }\n   "), empty_expr!(c));
    }

    #[test]
    fn test_simple_expr() {
        assert_ok!(lex!("hello"), str_expr!("hello"));
        assert_ok!(lex!("(hello_world)"), str_expr!(p "hello_world"));
    }

    #[test]
    fn test_group_expr() {
        assert_ok!(lex!("foo bar"), group_expr!(str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("(foo bar)"), group_expr!(p str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("(+ 1 2 3)"),
                   group_expr!(p str_expr!("+"), str_expr!("1"), str_expr!("2"), str_expr!("3")));
    }

    #[test]
    fn test_group_expr_square_brackets() {
        assert_ok!(lex!("foo[bar]"), group_expr!(str_expr!("foo"), str_expr!(s "bar")));
        assert_ok!(lex!("[+ 1 2 3]"),
                   group_expr!(s str_expr!("+"), str_expr!("1"), str_expr!("2"), str_expr!("3")));
    }

    #[test]
    fn test_nested_empty_expr() {
        assert_ok!(lex!("(())"), empty_expr!(p));
        assert_ok!(lex!("([])"), empty_expr!(p));
        assert_ok!(lex!("{([])}"), empty_expr!(c));
        assert_ok!(lex!("[{([])}]"), empty_expr!(s));
    }

    #[test]
    fn test_nested_group_expr() {
        assert_ok!(lex!("foo(bar)"), group_expr!(str_expr!("foo"), str_expr!(p "bar")));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   group_expr!(p str_expr!("+"),
                       group_expr!(p str_expr!("1"),
                           group_expr!(p str_expr!("2"), str_expr!("3")))));
    }

    #[test]
    fn test_naked_expr() {
        assert_ok!(lex!(";"), empty_expr!());
        assert_ok!(lex!("foo;"), str_expr!("foo"));
        assert_ok!(lex!("foo ; ignored"), str_expr!("foo"));
        assert_ok!(lex!("foo bar 1 2 3; ignored"),
            group_expr!(str_expr!("foo"), str_expr!("bar"),
                str_expr!("1"), str_expr!("2"), str_expr!("3")));
    }

    #[test]
    fn test_naked_expr_with_nested_exprs() {
        assert_ok!(lex!("foo(bar); ignored"),
            group_expr!(str_expr!("foo"),str_expr!(p "bar")));
        assert_ok!(lex!("foo (bar 1) 2; ignored"),
            group_expr!(str_expr!("foo"),
                group_expr!(p str_expr!("bar"), str_expr!("1")),
                str_expr!("2")));
        assert_ok!(lex!("foo (bar 1)[2]{ 3 } ; ignored"),
            group_expr!(str_expr!("foo"),
                group_expr!(p str_expr!("bar"), str_expr!("1")),
                str_expr!(s "2"), str_expr!(c "3")));
    }

    #[test]
    fn test_iterator_can_be_reused_and_pos() {
        let mut state = LexerState::default();
        let mut iter = "(a)\n(b)".split_word_bounds();
        assert_ok!(lexer(&mut iter, &mut state), str_expr!(p "a"));
        assert_ok!(lexer(&mut iter, &mut state), str_expr!(p "b"));
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