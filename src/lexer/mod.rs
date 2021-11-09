use lazy_static::lazy_static;
use regex::Regex;
use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use model::*;

use crate::errors::WasminError;
use crate::lexer::model::ASTNode::Group;
use crate::lexer::str::parse_str;

pub mod model;
mod str;

lazy_static! {
    static ref NUM_REGEX: Regex = Regex::new(
        r"^[+-]?(0|[1-9][\d_]*)(\.\d[\d_]*)?([Ee][+-]?\d[\d_]*)?([fi](32|64))?$"
    ).unwrap();
}

struct LexerState<'s> {
    nesting: Vec<NestingToken>,
    line: usize,
    col: usize,
    idx: usize,
    text: &'s str,
    words: UWordBounds<'s>,
}

impl<'s> LexerState<'s> {
    fn pos(&'s self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn new(text: &'s str) -> LexerState<'s> {
        LexerState {
            nesting: vec![],
            line: 1,
            col: 0,
            idx: 0,
            text,
            words: text.split_word_bounds(),
        }
    }

    fn next(&mut self) -> Option<&'s str> {
        if let Some(token) = self.words.next() {
            self.idx += token.len();
            if token == "\n" || token == "\r\n" {
                self.line += 1;
                self.col = 0;
            } else {
                self.col += token.len();
            }
            Some(token)
        } else {
            None
        }
    }

    /// Try to get the next nesting element.
    /// If the next non-whitespace character is a nesting element, it is returned and the state
    /// is advanced past the element, otherwise the state is not modified and [None] is returned.
    fn next_nesting_elem(&mut self) -> Option<NestingElement> {
        let nesting = self.text[self.idx..].trim_start().chars().next().and_then(|c| {
            as_nesting_start(&String::from(c))
        });
        if let Some(elem) = nesting {
            // consume the input until non-whitespace is found, which is the nesting elem
            loop {
                if let Some(token) = self.next() {
                    if !token.trim().is_empty() {
                        break;
                    }
                }
            }
            self.nesting.push(NestingToken { pos: self.pos(), elem });
        }
        nesting
    }
}

fn as_nesting_start(token: &str) -> Option<NestingElement> {
    let elem = match token {
        "(" => NestingElement::Parens,
        "[" => NestingElement::Square,
        "{" => NestingElement::Curly,
        _ => return None
    };
    Some(elem)
}

fn as_nesting_end(token: &str) -> Option<NestingElement> {
    let elem = match token {
        ")" => NestingElement::Parens,
        "]" => NestingElement::Square,
        "}" => NestingElement::Curly,
        _ => return None
    };
    Some(elem)
}

fn is_terminated(nodes: &Vec<ASTNode>) -> bool {
    match &nodes[..] {
        // EOF?
        &[] => true,
        // single group is always self-terminating
        &[ASTNode::Group(..)] => true,
        // anything that's not a group, and ending with END, is terminated
        &[.., ASTNode::End] => true,
        _ => false,
    }
}

/// Perform the lexing of a Wasmin program source code.
pub fn lex(source: &str) -> Result<Vec<ASTNode>, WasminError> {
    let mut state = LexerState::new(source);
    lex_to_end(&mut state)
}

fn lex_to_end<'s>(state: &mut LexerState<'s>)
                  -> Result<Vec<ASTNode<'s>>, WasminError> {
    let mut nodes = vec![];
    loop {
        match lex_expr(state)? {
            Some(n) => nodes.push(n),
            None => break
        };
    }
    if !state.nesting.is_empty() {
        let last_n = state.nesting.last().unwrap();
        return Err(werr_syntax!(format!("unmatched '{}', which started at {}",
                   last_n.elem, last_n.pos_str()), last_n.pos, state.pos()));
    }
    Ok(nodes)
}

#[inline]
fn lex_expr<'s>(state: &mut LexerState<'s>)
                -> Result<Option<ASTNode<'s>>, WasminError> {
    let nesting = state.next_nesting_elem();
    lex_expr_with_nesting(state, nesting)
}

fn lex_expr_with_nesting<'s>(state: &mut LexerState<'s>, nesting: Option<NestingElement>)
                             -> Result<Option<ASTNode<'s>>, WasminError> {
    let mut nodes = Vec::<ASTNode>::new();
    let mut comment_start_pos: Option<usize> = None;

    loop {
        let token = if let Some(t) = state.next() { t } else { break; };
        if comment_start_pos.is_some() {
            if token.contains("\n") {
                let comment_start = comment_start_pos.take().unwrap();
                let comment_end = state.idx - 1;
                nodes.push(ASTNode::Comment(&state.text[comment_start..comment_end]));
            }
            continue;
        }

        let token = token.trim();
        if token.is_empty() { continue; }

        if let Some(elem) = as_nesting_start(token) {
            state.nesting.push(NestingToken { pos: state.pos(), elem });
            match lex_expr_with_nesting(state, Some(elem))? {
                Some(n) => nodes.push(n),
                None => break // EOF
            };
            if nodes.is_empty() { break; } else { continue; }
        }
        if let Some(elem) = as_nesting_end(token) {
            let opener = state.nesting.pop();
            if let Some(opener) = opener {
                if opener.elem == elem { break; }
                return Err(werr_syntax!(format!("misplaced '{}', expecting '{}', which started at {}",
                    token, opener.closing_char(), opener.pos_str()), state.pos()));
            }
            return Err(werr_syntax!(format!("mismatched '{}', closes nothing", token), state.pos()));
        }
        // '.' does not split words so we need special treatment for it
        if token.contains(".") {
            split_dots_or_num(&mut nodes, token);
            continue;
        }
        nodes.push(match token {
            "=" => ASTNode::Eq(Box::new(match lex_expr(state)? {
                Some(n) => n,
                None => break,
            })),
            "#" => {
                comment_start_pos.insert(state.idx);
                continue; // nothing to push
            }
            ";" if nesting.is_none() => {
                nodes.push(ASTNode::End);
                break;
            }
            ";" => ASTNode::End,
            "," => ASTNode::Split,
            "let" => ASTNode::Let,
            "mut" => ASTNode::Mut,
            "set" => ASTNode::Set,
            "fun" => ASTNode::Fun,
            "pub" => ASTNode::Pub,
            "use" => ASTNode::Use,
            "if" => ASTNode::If,
            "then" => ASTNode::Then,
            "else" => ASTNode::Else,
            "def" => ASTNode::Def,
            "ext" => ASTNode::Ext,
            "@" => ASTNode::At,
            "." => ASTNode::Dot,
            "-" => ASTNode::Dash,
            "\"" => handle_str(state, "\"")?,
            "'" => handle_str(state, "'")?,
            _ if is_num(token) => ASTNode::Num(token),
            _ => ASTNode::Id(token),
        });
    }
    if let Some(comment_start) = comment_start_pos {
        nodes.push(ASTNode::Comment(&state.text[comment_start..state.idx]));
    }
    if nodes.is_empty() && nesting.is_none() { return Ok(None); }
    Ok(Some(join_nodes(nodes, nesting)))
}

fn is_num(token: &str) -> bool {
    NUM_REGEX.is_match(token)
}

fn split_dots_or_num<'s>(nodes: &mut Vec<ASTNode<'s>>, token: &'s str) {
    if token == "." {
        nodes.push(ASTNode::Dot);
        return;
    }
    if is_num(token) {
        nodes.push(ASTNode::Num(token));
        return;
    }
    for part in token.split(".") {
        nodes.push(if part.is_empty() { ASTNode::Dot } else { ASTNode::Id(part) });
        nodes.push(ASTNode::Dot);
    }
    nodes.pop();
}

fn handle_str<'s>(state: &mut LexerState<'s>, end: &str)
                  -> Result<ASTNode<'s>, WasminError> {
    let start = state.pos();
    Ok(ASTNode::Str(parse_str(state, end)
        .map_err(|e| werr_syntax!(e, start, state.pos()))?))
}

fn join_nodes(mut nodes: Vec<ASTNode>, nesting: Option<NestingElement>) -> ASTNode {
    if nodes.is_empty() {
        Group(vec![], nesting)
    } else if nodes.len() == 1 {
        let node = nodes.remove(0);
        if nesting.is_some() {
            Group(vec![node], nesting)
        } else {
            node
        }
    } else {
        Group(nodes, nesting)
    }
}

/*
 MACROS
*/

#[macro_export]
macro_rules! wnest {
    (p) => {$crate::lexer::model::NestingElement::Parens};
    (s) => {$crate::lexer::model::NestingElement::Square};
    (c) => {$crate::lexer::model::NestingElement::Curly};
}

#[macro_export]
macro_rules! wgroup {
    () => {$crate::lexer::model::ASTNode::Group(vec![], None)};
    (p) => {$crate::lexer::model::ASTNode::Group(vec![], Some($crate::wnest!(p)))};
    (s) => {$crate::lexer::model::ASTNode::Group(vec![], Some($crate::wnest!(s)))};
    (c) => {$crate::lexer::model::ASTNode::Group(vec![], Some($crate::wnest!(c)))};
    ($($a:expr), +) => {$crate::lexer::model::ASTNode::Group(vec![ $( $a ), * ], None)};
    (p $($a:expr), +) => {$crate::lexer::model::ASTNode::Group(vec![ $( $a ), * ], Some($crate::wnest!(p)))};
    (s $($a:expr), +) => {$crate::lexer::model::ASTNode::Group(vec![ $( $a ), * ], Some($crate::wnest!(s)))};
    (c $($a:expr), +) => {$crate::lexer::model::ASTNode::Group(vec![ $( $a ), * ], Some($crate::wnest!(c)))};
}

#[macro_export]
macro_rules! wid { ($e:literal) => {$crate::lexer::model::ASTNode::Id($e)} }
#[macro_export]
macro_rules! wcomment { ($e:literal) => {$crate::lexer::model::ASTNode::Comment($e)} }
#[macro_export]
macro_rules! wnum { ($e:literal) => {$crate::lexer::model::ASTNode::Num($e)} }
#[macro_export]
macro_rules! wstr { ($e:literal) => {$crate::lexer::model::ASTNode::Str($e)} }
#[macro_export]
macro_rules! wsplit { () => {$crate::lexer::model::ASTNode::Split} }
#[macro_export]
macro_rules! wend { () => {$crate::lexer::model::ASTNode::End} }
#[macro_export]
macro_rules! wlet { () => {$crate::lexer::model::ASTNode::Let} }
#[macro_export]
macro_rules! wmut { () => {$crate::lexer::model::ASTNode::Mut} }
#[macro_export]
macro_rules! wset { () => {$crate::lexer::model::ASTNode::Set} }
#[macro_export]
macro_rules! wfun { () => {$crate::lexer::model::ASTNode::Fun} }
#[macro_export]
macro_rules! wpub { () => {$crate::lexer::model::ASTNode::Pub} }
#[macro_export]
macro_rules! wuse { () => {$crate::lexer::model::ASTNode::Use} }
#[macro_export]
macro_rules! wif { () => {$crate::lexer::model::ASTNode::If} }
#[macro_export]
macro_rules! wthen { () => {$crate::lexer::model::ASTNode::Then} }
#[macro_export]
macro_rules! welse { () => {$crate::lexer::model::ASTNode::Else} }
#[macro_export]
macro_rules! wdef { () => {$crate::lexer::model::ASTNode::Def} }
#[macro_export]
macro_rules! wext { () => {$crate::lexer::model::ASTNode::Ext} }
#[macro_export]
macro_rules! wat { () => {$crate::lexer::model::ASTNode::At} }
#[macro_export]
macro_rules! wdot { () => {$crate::lexer::model::ASTNode::Dot} }
#[macro_export]
macro_rules! wdash { () => {$crate::lexer::model::ASTNode::Dash} }
#[macro_export]
macro_rules! weq { ($g:expr) => {$crate::lexer::model::ASTNode::Eq(Box::new($g))} }

/// TESTS
#[cfg(test)]
mod is_terminated_test {
    use crate::lexer::model::{ASTNode::*};
    use crate::lexer::model::NestingElement::Parens;

    use super::is_terminated;

    #[test]
    fn is_terminated_test() {
        assert_eq!(is_terminated(&vec![]), true);
        assert_eq!(is_terminated(&vec![Group(vec![], None)]), true);
        assert_eq!(is_terminated(&vec![Group(vec![], Some(Parens))]), true);
        assert_eq!(is_terminated(&vec![Id(""), End]), true);
        assert_eq!(is_terminated(&vec![Id(""), Id(""), End]), true);
        assert_eq!(is_terminated(&vec![Let, Id(""), End]), true);
        assert_eq!(is_terminated(&vec![End]), true);

        assert_eq!(is_terminated(&vec![Id("")]), false);
        assert_eq!(is_terminated(&vec![Split]), false);
        assert_eq!(is_terminated(&vec![Split, Id("")]), false);
        assert_eq!(is_terminated(&vec![Id(""), Group(vec![], None)]), false);
        assert_eq!(is_terminated(&vec![Id(""), Group(vec![], Some(Parens))]), false);
        assert_eq!(is_terminated(&vec![Let, Id(""), Eq(Box::new(Group(vec![], Some(Parens))))]), false);
    }
}

#[cfg(test)]
mod is_num {
    use super::is_num;

    #[test]
    fn is_num_test() {
        assert!(is_num("0"));
        assert!(is_num("+0"));
        assert!(is_num("-0"));
        assert!(is_num("1"));
        assert!(is_num("2"));
        assert!(is_num("1e0"));
        assert!(is_num("1E2"));
        assert!(is_num("100e-1"));
        assert!(is_num("100.0123e-1"));
        assert!(is_num("100.0123e+245"));
        assert!(is_num("-100.0123e-245"));
        assert!(is_num("100"));
        assert!(is_num("11234556778990223"));
        assert!(is_num("+1"));
        assert!(is_num("-1"));
        assert!(is_num("1_0"));
        assert!(is_num("1_123_456"));
        assert!(is_num("1_123_456_i64"));
        assert!(is_num("1_123_456i64"));
        assert!(is_num("1_123_456E1i64"));
        assert!(is_num("1_123_456E10_00_00_i64"));
    }

    #[test]
    fn is_not_num_test() {
        assert!(!is_num("f"));
        assert!(!is_num("."));
        assert!(!is_num("-"));
        assert!(!is_num("+"));
        assert!(!is_num("11A234556778990223"));
        assert!(!is_num("+1A"));
        assert!(!is_num("+A1"));
        assert!(!is_num("-1e"));
        assert!(!is_num("123."));
        assert!(!is_num("123.4.5"));
        assert!(!is_num(".1"));
        assert!(!is_num(".A"));
        assert!(!is_num("-A"));
        assert!(!is_num("-A1"));
        assert!(!is_num("_1"));
        assert!(!is_num("_10_"));
        assert!(!is_num("1f32f64"));
        assert!(!is_num("10f32_i64"));
    }
}

#[cfg(test)]
mod tests {
    use crate::errors::ErrorPosition;

    use super::*;

    macro_rules! lex {
        ($input:literal) => {{
            let mut state = LexerState::new($input);
            lex_expr(&mut state)
        }};
    }

    macro_rules! assert_ok {
        ($left:expr, $right:expr) => { assert_eq!($left, Ok(Some($right))) };
    }

    macro_rules! assert_syntax_err {
        ($e:expr, $msg:literal, $start:expr, $end:expr) => {{
            assert_eq!($e, Err(WasminError::SyntaxError {
                cause: $msg.into(),
                pos: ErrorPosition { start: $start, end: $end },
            }));
        }};
    }

    #[test]
    fn test_empty_expr() {
        assert_eq!(lex!(""), Ok(None));
        assert_ok!(lex!("()"), wgroup!(p));
        assert_ok!(lex!("( )"), wgroup!(p));
        assert_ok!(lex!("  [    ]   "), wgroup!(s));
        assert_ok!(lex!("  \n{ \n  }\n   "), wgroup!(c));
    }

    #[test]
    fn test_simple_expr() {
        assert_ok!(lex!("hello"), wid!("hello"));
        assert_ok!(lex!("hello_world"), wid!("hello_world"));
    }

    #[test]
    fn test_word_boundaries() {
        assert_ok!(lex!("hi ho"), wgroup!(wid!("hi"), wid!("ho")));
        assert_ok!(lex!("hi\nho"), wgroup!(wid!("hi"), wid!("ho")));
        assert_ok!(lex!("hi\tho"), wgroup!(wid!("hi"), wid!("ho")));
        assert_ok!(lex!("hi\r\nho"), wgroup!(wid!("hi"), wid!("ho")));
        assert_ok!(lex!("hi_ho"), wid!("hi_ho"));
        // most symbols are not treated specially
        assert_ok!(lex!("hi?ho"), wgroup!(wid!("hi"), wid!("?"), wid!("ho")));
        assert_ok!(lex!("hi ? ho"), wgroup!(wid!("hi"), wid!("?"), wid!("ho")));
        assert_ok!(lex!("hi!ho"), wgroup!(wid!("hi"), wid!("!"), wid!("ho")));
        assert_ok!(lex!("hi^ho"), wgroup!(wid!("hi"), wid!("^"), wid!("ho")));
        assert_ok!(lex!("hi&ho"), wgroup!(wid!("hi"), wid!("&"), wid!("ho")));
        // but some are
        assert_ok!(lex!("hi@ho"), wgroup!(wid!("hi"), wat!(), wid!("ho")));
        assert_ok!(lex!("hi=ho"), wgroup!(wid!("hi"), weq!(wid!("ho"))));
        assert_ok!(lex!("hi-ho"), wgroup!(wid!("hi"), wdash!(), wid!("ho")));
        assert_ok!(lex!("hi.ho"), wgroup!(wid!("hi"), wdot!(), wid!("ho")));
        assert_ok!(lex!("hi,ho"), wgroup!(wid!("hi"), wsplit!(), wid!("ho")));
        assert_ok!(lex!("hi;ho"), wgroup!(wid!("hi"), wend!()));
    }

    #[test]
    fn test_group_expr() {
        assert_ok!(lex!("(hello_world)"), wgroup!(p wid!("hello_world")));
        assert_ok!(lex!("foo bar"), wgroup!(wid!("foo"), wid!("bar")));
        assert_ok!(lex!("(foo bar)"), wgroup!(p wid!("foo"), wid!("bar")));
        assert_ok!(lex!("[foo]"), wgroup!(s wid!("foo")));
        assert_ok!(lex!("(+ 1 2 3)"),
                   wgroup!(p wid!("+"), wnum!("1"), wnum!("2"), wnum!("3")));
    }

    #[test]
    fn test_group() {
        assert_ok!(lex!("foo, bar"), wgroup!(wid!("foo"), wsplit!(), wid!("bar")));
        assert_ok!(lex!("(foo, bar)"), wgroup!(p wid!("foo"), wsplit!(), wid!("bar")));
        assert_ok!(lex!("foo, bar 1, zort 2"),
            wgroup!(wid!("foo"), wsplit!(),
                wid!("bar"), wnum!("1"), wsplit!(),
                wid!("zort"), wnum!("2")));
        assert_ok!(lex!("[foo,]"), wgroup!(s wid!("foo"), wsplit!()));
        assert_ok!(lex!("{,}"), wgroup!(c wsplit!()));
        assert_ok!(lex!("1, 2 ,3; ignore"),
            wgroup!(wnum!("1"), wsplit!(), wnum!("2"), wsplit!(), wnum!("3"), wend!()));
        assert_ok!(lex!("1 2 ,3; ignore"),
            wgroup!(wnum!("1"), wnum!("2"), wsplit!(), wnum!("3"), wend!()));
    }

    #[test]
    fn test_group_expr_square_brackets() {
        assert_ok!(lex!("foo[bar]"), wgroup!(wid!("foo"), wgroup!(s wid!("bar"))));
        assert_ok!(lex!("[+ 1 2 3]"),
                   wgroup!(s wid!("+"), wnum!("1"), wnum!("2"), wnum!("3")));
    }

    #[test]
    fn test_nested_empty_expr() {
        assert_ok!(lex!("(())"), wgroup!(p wgroup!(p)));
        assert_ok!(lex!("([])"), wgroup!(p wgroup!(s)));
        assert_ok!(lex!("[({})]"), wgroup!(s wgroup!(p wgroup!(c))));
        assert_ok!(lex!("{{([])}}"), wgroup!(c wgroup!(c wgroup!(p wgroup!(s)))));
    }

    #[test]
    fn test_nested_group_expr() {
        assert_ok!(lex!("foo(bar)"), wgroup!(wid!("foo"), wgroup!(p wid!("bar"))));
        assert_ok!(lex!("((foo)(bar))"), wgroup!(p wgroup!(p wid!("foo")), wgroup!(p wid!("bar"))));
        assert_ok!(lex!("foo[(bar) zort]"), wgroup!(wid!("foo"),
            wgroup!(s wgroup!(p wid!("bar")), wid!("zort"))));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   wgroup!(p wid!("+"),
                       wgroup!(p wnum!("1"),
                           wgroup!(p wnum!("2"), wnum!("3")))));
    }

    #[test]
    fn test_nested_group() {
        assert_ok!(lex!("foo, (bar)"), wgroup!(wid!("foo"), wsplit!(), wgroup!(p wid!("bar"))));
        assert_ok!(lex!("(foo, (bar, zort))"),
            wgroup!(p wid!("foo"), wsplit!(), wgroup!(p wid!("bar"), wsplit!(), wid!("zort"))));
        assert_ok!(lex!("[1,foo(z,2)]"),
            wgroup!(s wnum!("1"), wsplit!(), wid!("foo"),
                wgroup!(p wid!("z"), wsplit!(), wnum!("2"))));
        assert_ok!(lex!("{[1,foo(z,2)], [(4,  89), 6]}"),
            wgroup!(c
                wgroup!(s wnum!("1"), wsplit!(), wid!("foo"),
                    wgroup!(p wid!("z"), wsplit!(), wnum!("2"))), wsplit!(),
                wgroup!(s wgroup!(p wnum!("4"), wsplit!(), wnum!("89")), wsplit!(), wnum!("6"))));
    }

    #[test]
    fn test_naked_expr() {
        assert_ok!(lex!(";"), wend!());
        assert_ok!(lex!("foo;"), wgroup!(wid!("foo"), wend!()));
        assert_ok!(lex!("foo ; ignored"), wgroup!(wid!("foo"), wend!()));
        assert_ok!(lex!("foo bar 1 2 3; ignored"),
            wgroup!(wid!("foo"), wid!("bar"),
                wnum!("1"), wnum!("2"), wnum!("3"), wend!()));
    }

    #[test]
    fn test_naked_expr_with_nested_exprs() {
        assert_ok!(lex!("foo(bar); ignored"),
            wgroup!(wid!("foo"), wgroup!(p wid!("bar")), wend!()));
        assert_ok!(lex!("foo(); ignored"),
            wgroup!(wid!("foo"), wgroup!(p), wend!()));
        assert_ok!(lex!("foo (bar 1) 2; ignored"),
            wgroup!(wid!("foo"),
                wgroup!(p wid!("bar"), wnum!("1")),
                wnum!("2"), wend!()));
        assert_ok!(lex!("foo (bar 1)[2]{ 3 } ; ignored"),
            wgroup!(wid!("foo"),
                wgroup!(p wid!("bar"), wnum!("1")),
                wgroup!(s wnum!("2")), wgroup!(c wnum!("3")), wend!()));
    }

    #[test]
    fn test_naked_exprs_within_nested_let_exprs() {
        assert_ok!(lex!("(foo;bar)"),
            wgroup!(p wid!("foo"), wend!(), wid!("bar")));
        assert_ok!(lex!("(let x = 1; let y=2; + x y)"),
            wgroup!(p
                wlet!(), wid!("x"), weq!(wgroup!(wnum!("1"), wend!())),
                wlet!(), wid!("y"), weq!(wgroup!(wnum!("2"), wend!())),
                wid!("+"), wid!("x"), wid!("y")));
    }

    #[test]
    fn test_nested_exprs_within_nested_exprs() {
        assert_ok!(lex!("mul [add 2 (3; sub 2)] 5"),
            wgroup!(
                wid!("mul"),
                wgroup!(s wid!("add"), wnum!("2"),
                    wgroup!(p wnum!("3"), wend!(), wid!("sub"), wnum!("2"))),
                wnum!("5")));
    }

    #[test]
    fn test_keywords() {
        assert_ok!(lex!("let x = 1, mut y = 2, set y=add x y;"),
            wgroup!(wlet!(), wid!("x"), weq!(wgroup!(wnum!("1"), wsplit!(),
                wmut!(), wid!("y"), weq!(wgroup!(wnum!("2"), wsplit!(),
                wset!(), wid!("y"), weq!(wgroup!(wid!("add"), wid!("x"), wid!("y"), wend!()))))))));
        assert_ok!(lex!("ext mod {
            add [u32] u32;
            mul [f32] f32;
            }"), wgroup!(wext!(), wid!("mod"), wgroup!(c
                wid!("add"), wgroup!(s wid!("u32")), wid!("u32"), wend!(),
                wid!("mul"), wgroup!(s wid!("f32")), wid!("f32"), wend!())));
        assert_ok!(lex!("if cond then x else y"),
            wgroup!(wif!(), wid!("cond"), wthen!(), wid!("x"), welse!(), wid!("y")));
        assert_ok!(lex!("pub fun f x = (sqrt x)"),
            wgroup!(wpub!(), wfun!(), wid!("f"), wid!("x"), weq!(
                wgroup!(p wid!("sqrt"), wid!("x")))));
        assert_ok!(lex!("@macro x"), wgroup!(wat!(), wid!("macro"), wid!("x")));
        assert_ok!(lex!("use foo, bar;"), wgroup!(wuse!(), wid!("foo"), wsplit!(), wid!("bar"), wend!()));
    }

    #[test]
    fn test_dot() {
        assert_ok!(lex!("."), wdot!());
        assert_ok!(lex!(".a"), wgroup!(wdot!(), wid!("a")));
        assert_ok!(lex!("a."), wgroup!(wid!("a"), wdot!()));
        assert_ok!(lex!("foo.bar"), wgroup!(wid!("foo"), wdot!(), wid!("bar")));
        assert_ok!(lex!("foo\n  .bar"), wgroup!(wid!("foo"), wdot!(), wid!("bar")));
        assert_ok!(lex!("foo.\n  bar"), wgroup!(wid!("foo"), wdot!(), wid!("bar")));
        assert_ok!(lex!("(foo . bar x)"),
            wgroup!(p wid!("foo"), wdot!(), wid!("bar"), wid!("x")));
        assert_ok!(lex!("(( foo ).bar )"),
            wgroup!(p wgroup!(p wid!("foo")), wdot!(), wid!("bar")));
        assert_ok!(lex!("[( foo ). (bar )]"),
            wgroup!(s wgroup!(p wid!("foo")), wdot!(), wgroup!(p wid!("bar"))));
    }

    #[test]
    fn test_pos_numbers() {
        assert_ok!(lex!("0"), wnum!("0"));
        assert_ok!(lex!("1"), wnum!("1"));
        assert_ok!(lex!("1.0"), wnum!("1.0"));
        assert_ok!(lex!("0.1"), wnum!("0.1"));
        assert_ok!(lex!("1i32"), wnum!("1i32"));
        assert_ok!(lex!("10_000f32"), wnum!("10_000f32"));
        assert_ok!(lex!("10_000.62f64"), wnum!("10_000.62f64"));
        assert_ok!(lex!("10_000e32"), wnum!("10_000e32"));
        assert_ok!(lex!("10_000e100_100_100"), wnum!("10_000e100_100_100"));
        assert_ok!(lex!("1.625428E100"), wnum!("1.625428E100"));
    }

    #[test]
    fn test_neg_numbers() {
        assert_ok!(lex!("-0"), wgroup!(wdash!(), wnum!("0")));
        assert_ok!(lex!("-1"), wgroup!(wdash!(), wnum!("1")));
        assert_ok!(lex!("-1.0"), wgroup!(wdash!(), wnum!("1.0")));
        assert_ok!(lex!("-0.1"), wgroup!(wdash!(), wnum!("0.1")));
        assert_ok!(lex!("-1i32"), wgroup!(wdash!(), wnum!("1i32")));
        assert_ok!(lex!("-10_000f32"), wgroup!(wdash!(), wnum!("10_000f32")));
        assert_ok!(lex!("-10_000.62f64"), wgroup!(wdash!(), wnum!("10_000.62f64")));
        assert_ok!(lex!("-10_000e32"), wgroup!(wdash!(),wnum!("10_000e32")));
        assert_ok!(lex!("-10_000e100_100_100"), wgroup!(wdash!(),wnum!("10_000e100_100_100")));
        assert_ok!(lex!("-1.625428E100"), wgroup!(wdash!(),wnum!("1.625428E100")));
    }

    #[test]
    fn test_not_numbers() {
        assert_ok!(lex!("a0"), wid!("a0"));
        assert_ok!(lex!("_1"), wid!("_1"));
        assert_ok!(lex!("1z"), wid!("1z"));
        assert_ok!(lex!("@b4"), wgroup!(wat!(), wid!("b4")));
    }

    #[test]
    fn test_single_line_comments() {
        assert_ok!(lex!("# foo bar\na0"), wgroup!(
            wcomment!(" foo bar"), wid!("a0")));
        assert_ok!(lex!("#foo\na1#done"), wgroup!(wcomment!("foo"), wid!("a1"), wcomment!("done")));
        assert_ok!(lex!("# foo bar
            let x
            # another comment # ignore it
            # done
            = y; # end"), wgroup!(wcomment!(" foo bar"),
                wlet!(), wid!("x"),
                wcomment!(" another comment # ignore it"), wcomment!(" done"),
                weq!(wgroup!(wid!("y"), wend!())), wcomment!(" end")));
    }

    #[test]
    fn test_str() {
        assert_ok!(lex!("\"\""), wstr!(""));
        assert_ok!(lex!("\"hello\""), wstr!("hello"));
        assert_ok!(lex!("\"A full sentence...\""), wstr!("A full sentence..."));
        assert_ok!(lex!("\"pub fun let 'keywords' = 2, 4)\""),
            wstr!("pub fun let 'keywords' = 2, 4)"));
        assert_ok!(lex!("let s = \"hello\";"),
            wgroup!(wlet!(), wid!("s"), weq!(wgroup!(wstr!("hello"), wend!()))));
        assert_ok!(lex!("(concat \"hello\" [fst \"bar\"])"),
            wgroup!(p wid!("concat"), wstr!("hello"), wgroup!(s wid!("fst"), wstr!("bar"))));
    }

    #[test]
    fn test_str_single_quote() {
        assert_ok!(lex!("'hello'"), wstr!("hello"));
        assert_ok!(lex!("'A full sentence...'"), wstr!("A full sentence..."));
        assert_ok!(lex!("'pub fun let \"keywords\" = 2, 4)'"),
            wstr!("pub fun let \"keywords\" = 2, 4)"));
    }

    #[test]
    fn test_str_escapes() {
        assert_ok!(lex!("'hello\\n'"), wstr!("hello\\n"));
        assert_ok!(lex!("'hello\\n\\r'"), wstr!("hello\\n\\r"));
        assert_ok!(lex!("'hello\\t'"), wstr!("hello\\t"));
        assert_ok!(lex!("'hello\\''"), wstr!("hello\\'"));
        assert_ok!(lex!("\"hello\\\"\""), wstr!("hello\\\""));
    }

    #[test]
    fn test_str_unfinished() {
        assert_syntax_err!(lex!("\"foo bar"),
            "unclosed string", (1, 1), (1, 8));
    }

    #[test]
    fn test_unclosed_group_error() {
        assert_syntax_err!(lex("(x"),
            "unmatched '(', which started at 1:1", (1, 1), (1, 2));
        assert_syntax_err!(lex("foo[x;"),
            "unmatched '[', which started at 1:4", (1, 4), (1, 6));
        assert_syntax_err!(lex("fun x y = {\n  (\n abc ; \n )"),
            "unmatched '{', which started at 1:11", (1, 11), (4, 2));
    }

    #[test]
    fn test_unmatched_closing_group_error() {
        assert_syntax_err!(lex("x)"),
            "mismatched ')', closes nothing", (1, 2), (1, 2));
        assert_syntax_err!(lex("ext {
            foo [] u64;
            bar ];
        }\n  "),
            "misplaced ']', expecting '}', which started at 1:5", (3, 17), (3, 17));
    }

    #[test]
    fn test_iterator_can_be_reused_and_pos() {
        let words = "(a)\n(b)";
        let mut state = LexerState::new(words);
        assert_eq!(lex_expr(&mut state),
                   Ok(Some(wgroup!(p wid!("a")))));
        assert_eq!(lex_expr(&mut state),
                   Ok(Some(wgroup!(p wid!("b")))));
        assert_eq!(state.pos(), (2, 3));
    }

    #[test]
    fn test_new_lines_pos() {
        let words = "\n \r\n foo\nb a r";
        let mut state = LexerState::new(words);
        assert_eq!(lex_expr(&mut state),
                   Ok(Some(wgroup!(wid!("foo"), wid!("b"), wid!("a"), wid!("r")))));
        assert_eq!(state.pos(), (4, 5));
    }
}
