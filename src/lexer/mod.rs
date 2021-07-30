use std::fmt::{Display, Formatter};

use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use crate::errors::WasminError;

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Expression<'s> {
    Empty(Option<NestingElement>),
    Wrap(Box<Expression<'s>>, NestingElement),
    Simple(&'s str, Option<NestingElement>),
    Group(Vec<Expression<'s>>, Option<NestingElement>),
    Multi(Vec<Expression<'s>>, Option<NestingElement>),
}

impl<'s> Expression<'s> {
    fn nest(self, elem: NestingElement) -> Self {
        match self {
            Expression::Empty(_) => Expression::Empty(Some(elem)),
            Expression::Wrap(e, _) => Expression::Wrap(e, elem),
            Expression::Simple(e, _) => Expression::Simple(e, Some(elem)),
            Expression::Group(e, _) => Expression::Group(e, Some(elem)),
            Expression::Multi(e, _) => Expression::Multi(e, Some(elem)),
        }
    }

    fn has_nesting(&self) -> bool {
        match self {
            Expression::Empty(n) => n.is_some(),
            Expression::Wrap(..) => true,
            Expression::Simple(_, n) => n.is_some(),
            Expression::Group(_, n) => n.is_some(),
            Expression::Multi(_, n) => n.is_some(),
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

/// ExprOrComma allows the lexer to keep track of where each expression is split up
/// into multi-values.
#[derive(Debug)]
enum ExpOrComma<'s> {
    Exp(Expression<'s>),
    Comma,
}

struct Tokens<'s> {
    words: UWordBounds<'s>,
    returned: Option<&'s str>,
}

impl<'s> Tokens<'s> {
    fn from(s: &str) -> Tokens {
        Tokens { words: s.split_word_bounds(), returned: None }
    }

    fn next(&mut self) -> Option<&'s str> {
        if let Some(r) = self.returned.take() {
            return Some(r);
        }
        self.words.next()
    }

    fn put_back(&mut self, token: &'s str, state: &mut LexerState) {
        if let Some(t) = self.returned {
            panic!("Putting back more than one token: '{}', '{}'", t, token)
        }
        // state.col -= token.len();
        self.returned.insert(token);
    }
}

/// Start a nesting expression from a naked expression.
/// In other words, this macro does not terminate parsing of current expression.
macro_rules! nested_exp {
    ($tokens:expr, $token:expr, $state:expr) => {{
        $tokens.put_back($token, $state);
        ExpOrComma::Exp(lexer($tokens, $state)?)
    }};
}

/// End a nesting expression.
/// Pops a nesting level and results in a Result<Expression>.
macro_rules! end_nesting {
    ($elem:expr, $token:expr, $nesting:expr, $state:expr) => {{
        if $nesting.elem != $elem {
            return Err(werr_syntax!(format!("misplaced '{}', expecting '{}', which started at {}",
                $token, $nesting.closing_char(), $nesting.pos_str()), $state.pos()))
        }
        break;
    }};
}

macro_rules! update_pos {
    ($token:expr, $state:expr) => {{
        if $token == "\n" || $token == "\r\n" {
            $state.line += 1;
            $state.col = 0;
        } else {
            $state.col += $token.len()
        }
    }};
}

fn get_nesting_elem(token: &str) -> Option<NestingElement> {
    let elem = match token {
        "(" => NestingElement::Parens,
        "[" => NestingElement::Square,
        "{" => NestingElement::Curly,
        _ => return None
    };
    Some(elem)
}

fn first_non_empty_token<'s>(tokens: &mut Tokens<'s>, state: &mut LexerState)
                             -> Option<&'s str> {
    loop {
        let mut token = if let Some(t) = tokens.next() { t } else {
            return None;
        };
        update_pos!(token, state);
        token = token.trim();
        if !token.is_empty() { return Some(token); }
    }
}

fn lexer<'s>(tokens: &mut Tokens<'s>, state: &mut LexerState)
             -> Result<Expression<'s>, WasminError> {
    // let file = fs::read_to_string("ex.wasmin").unwrap();
    // let tokens = UnicodeSegmentation::split_word_bounds(&file);
    let mut exprs = Vec::<ExpOrComma>::new();

    let token = if let Some(t) = first_non_empty_token(tokens, state) { t } else {
        return Ok(Expression::Empty(None));
    };

    // If '(', '[' or '{' start the expression, then we terminate it as soon as we handle the
    // nesting, otherwise we go all the way down to either ';' or EOF.
    let nesting = get_nesting_elem(token)
        .map(|elem| NestingToken { pos: state.pos(), elem });
    if nesting.is_none() {
        tokens.put_back(token, state);
    }

    loop {
        let token = if let Some(t) = first_non_empty_token(tokens, state) { t } else {
            break;
        };
        match (&nesting, token) {
            // nesting cases
            (_, "(" | "[" | "{") => exprs.push(nested_exp!(tokens, token, state)),
            // ending nesting within naked expression
            (None, ")" | "]" | "}") => {
                tokens.put_back(token, state); // it's not ours to take
                break;
            }
            // ending naked expression
            (None, ";") => break,
            // terminating nested expression cases
            (Some(n), ")") => end_nesting!(NestingElement::Parens, token, n, state),
            (Some(n), "]") => end_nesting!(NestingElement::Square, token, n, state),
            (Some(n), "}") => end_nesting!(NestingElement::Curly, token, n, state),
            // TODO
            (Some(_), ";") => panic!("need to group current contents and then continue?"),
            // multi expression
            (_, ",") => exprs.push(ExpOrComma::Comma),
            // simple expression
            _ => exprs.push(ExpOrComma::Exp(Expression::Simple(token, None))),
        };
    }
    Ok(join_exprs(exprs, nesting.map(|n| n.elem)))
    // FIXME caller needs to do this in the top-level scope
    // } else {
    //     let last_n = state.nesting.last().unwrap();
    //     let err = if eof { "EOF" } else { "';'" };
    //     Err(werr_syntax!(format!("unexpected {}, unmatched '{}', which started at {}",
    //             err , last_n.elem, last_n.pos_str()), state.pos()))
    // }
}

fn join_exprs(exprs: Vec<ExpOrComma>, nesting: Option<NestingElement>) -> Expression {
    let mut multi = vec![Vec::new()];
    for exp in exprs {
        match exp {
            ExpOrComma::Exp(e) => {
                multi.last_mut().unwrap().push(e);
            }
            ExpOrComma::Comma => {
                multi.push(Vec::new());
            }
        }
    }
    if multi.len() > 1 {
        Expression::Multi(
            multi.drain(..)
                .map(|e| join_single_exprs(e, None))
                .collect(), nesting)
    } else {
        join_single_exprs(multi.remove(0), nesting)
    }
}

fn join_single_exprs(mut exprs: Vec<Expression>, nesting: Option<NestingElement>) -> Expression {
    match (exprs.len(), nesting) {
        (0, n) => Expression::Empty(n),
        (1, Some(n)) => {
            let exp = exprs.remove(0);
            if exp.has_nesting() {
                Expression::Wrap(Box::new(exp), n)
            } else {
                exp.nest(n)
            }
        }
        (1, None) => exprs.remove(0),
        (_, n) => Expression::Group(exprs, n)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Expression;

    use super::*;

    macro_rules! nest {
        (p) => {NestingElement::Parens};
        (s) => {NestingElement::Square};
        (c) => {NestingElement::Curly};
    }

    macro_rules! group_expr {
        ($($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], None)};
        (p $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(nest!(p)))};
        (s $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(nest!(s)))};
        (c $($a:expr), +) => {Expression::Group(vec![ $( $a ), + ], Some(nest!(c)))};
    }

    macro_rules! multi_expr {
        ($($a:expr), +) => {Expression::Multi(vec![ $( $a ), + ], None)};
        (p $($a:expr), +) => {Expression::Multi(vec![ $( $a ), + ], Some(nest!(p)))};
        (s $($a:expr), +) => {Expression::Multi(vec![ $( $a ), + ], Some(nest!(s)))};
        (c $($a:expr), +) => {Expression::Multi(vec![ $( $a ), + ], Some(nest!(c)))};
    }

    macro_rules! empty_expr {
        () => {Expression::Empty(None)};
        (p) => {Expression::Empty(Some(nest!(p)))};
        (s) => {Expression::Empty(Some(nest!(s)))};
        (c) => {Expression::Empty(Some(nest!(c)))};
    }

    macro_rules! str_expr {
        ($e:literal) => {Expression::Simple($e, None)};
        (p $e:literal) => {Expression::Simple($e, Some(nest!(p)))};
        (s $e:literal) => {Expression::Simple($e, Some(nest!(s)))};
        (c $e:literal) => {Expression::Simple($e, Some(nest!(c)))};
    }

    macro_rules! wrap_expr {
        (p $e:expr) => { Expression::Wrap(Box::new($e), nest!(p)) };
        (s $e:expr) => { Expression::Wrap(Box::new($e), nest!(s)) };
        (c $e:expr) => { Expression::Wrap(Box::new($e), nest!(c)) };
    }

    macro_rules! lex {
        ($input:literal) => {{
            let mut state = LexerState::default();
            let mut tokens = Tokens::from($input);
            lexer(&mut tokens, &mut state)
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
    fn test_multi_expr() {
        assert_ok!(lex!("foo, bar"), multi_expr!(str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("(foo, bar)"), multi_expr!(p str_expr!("foo"), str_expr!("bar")));
        assert_ok!(lex!("foo, bar 1, zort 2"),
            multi_expr!(str_expr!("foo"),
                group_expr!(str_expr!("bar"), str_expr!("1")),
                group_expr!(str_expr!("zort"), str_expr!("2"))));
        assert_ok!(lex!("[foo,]"), multi_expr!(s str_expr!("foo"), empty_expr!()));
        assert_ok!(lex!("{,}"), multi_expr!(c empty_expr!(), empty_expr!()));
        assert_ok!(lex!("1, 2 ,3; ignore"),
            multi_expr!(str_expr!("1"), str_expr!("2"), str_expr!("3")));
        assert_ok!(lex!("1 2 ,3; ignore"),
            multi_expr!(group_expr!(str_expr!("1"), str_expr!("2")), str_expr!("3")));
    }

    #[test]
    fn test_group_expr_square_brackets() {
        assert_ok!(lex!("foo[bar]"), group_expr!(str_expr!("foo"), str_expr!(s "bar")));
        assert_ok!(lex!("[+ 1 2 3]"),
                   group_expr!(s str_expr!("+"), str_expr!("1"), str_expr!("2"), str_expr!("3")));
    }

    #[test]
    fn test_nested_empty_expr() {
        assert_ok!(lex!("(())"), wrap_expr!(p empty_expr!(p)));
        assert_ok!(lex!("([])"), wrap_expr!(p empty_expr!(s)));
        assert_ok!(lex!("[({})]"), wrap_expr!(s wrap_expr!(p empty_expr!(c))));
        assert_ok!(lex!("{{([])}}"), wrap_expr!(c wrap_expr!(c wrap_expr!(p empty_expr!(s)))));
    }

    #[test]
    fn test_nested_group_expr() {
        assert_ok!(lex!("foo(bar)"), group_expr!(str_expr!("foo"), str_expr!(p "bar")));
        assert_ok!(lex!("((foo)(bar))"), group_expr!(p str_expr!(p "foo"), str_expr!(p "bar")));
        assert_ok!(lex!("foo[(bar) zort]"), group_expr!(str_expr!("foo"),
            group_expr!(s str_expr!(p "bar"), str_expr!("zort"))));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   group_expr!(p str_expr!("+"),
                       group_expr!(p str_expr!("1"),
                           group_expr!(p str_expr!("2"), str_expr!("3")))));
    }

    #[test]
    fn test_nested_multi_expr() {
        assert_ok!(lex!("foo, (bar)"), multi_expr!(str_expr!("foo"), str_expr!(p "bar")));
        assert_ok!(lex!("(foo, (bar, zort))"),
            multi_expr!(p str_expr!("foo"), multi_expr!(p str_expr!("bar"), str_expr!("zort"))));
        assert_ok!(lex!("[1,foo(z,2)]"),
            multi_expr!(s str_expr!("1"),
                group_expr!(str_expr!("foo"), multi_expr!(p str_expr!("z"), str_expr!("2")))));
        println!(r"Staring {{[1,foo(z,2)], [(4,  89)]}}");
        assert_ok!(lex!("{[1,foo(z,2)], [(4,  89), 6]}"),
            multi_expr!(c
                multi_expr!(s str_expr!("1"), group_expr!(str_expr!("foo"),
                    multi_expr!(p str_expr!("z"), str_expr!("2")))),
                multi_expr!(s multi_expr!(p str_expr!("4"), str_expr!("89")), str_expr!("6"))));
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
        assert_ok!(lex!("foo(); ignored"),
            group_expr!(str_expr!("foo"), empty_expr!(p)));
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
        let mut iter = Tokens::from("(a)\n(b)");
        assert_ok!(lexer(&mut iter, &mut state), str_expr!(p "a"));
        assert_ok!(lexer(&mut iter, &mut state), str_expr!(p "b"));
        assert_eq!(state.pos(), (2, 3));
    }

    #[test]
    fn test_new_lines_pos() {
        let mut state = LexerState::default();
        let mut iter = Tokens::from("\n \r\n foo\nb a r");
        assert_ok!(lexer(&mut iter, &mut state),
            group_expr!(str_expr!("foo"), str_expr!("b"), str_expr!("a"), str_expr!("r")));
        assert_eq!(state.pos(), (4, 5));
    }
}