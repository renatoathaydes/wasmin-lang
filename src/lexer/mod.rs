use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use model::*;

use crate::errors::WasminError;
use crate::lexer::model::ASTNode::Group;

mod model;

struct LexerState<'s> {
    nesting: Vec<NestingToken>,
    line: usize,
    col: usize,
    words: UWordBounds<'s>,
}

impl<'s> LexerState<'s> {
    fn pos(&'s self) -> (usize, usize) {
        (self.line, self.col)
    }

    fn new(words: UWordBounds<'s>) -> LexerState<'s> {
        LexerState {
            nesting: vec![],
            line: 1,
            col: 0,
            words,
        }
    }

    fn next(&mut self) -> Option<&'s str> {
        let token = self.words.next()?;
        if token == "\n" || token == "\r\n" {
            self.line += 1;
            self.col = 0;
        } else {
            self.col += token.len();
        }
        Some(token.trim())
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
        // anything that's not a group ending with END is terminated
        &[.., ASTNode::End] => true,
        _ => false,
    }
}

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
        assert_eq!(is_terminated(&vec![Str(""), End]), true);
        assert_eq!(is_terminated(&vec![Str(""), Str(""), End]), true);
        assert_eq!(is_terminated(&vec![Let, Str(""), End]), true);
        assert_eq!(is_terminated(&vec![End]), true);

        assert_eq!(is_terminated(&vec![Str("")]), false);
        assert_eq!(is_terminated(&vec![Split]), false);
        assert_eq!(is_terminated(&vec![Split, Str("")]), false);
        assert_eq!(is_terminated(&vec![Str(""), Group(vec![], None)]), false);
        assert_eq!(is_terminated(&vec![Str(""), Group(vec![], Some(Parens))]), false);
        assert_eq!(is_terminated(&vec![Let, Str(""), Eq, Group(vec![], Some(Parens))]), false);
    }
}

fn lexer<'s>(state: &mut LexerState<'s>)
             -> Result<ASTNode<'s>, WasminError> {
    let mut nodes = vec![];
    loop {
        let mut next = lexer_rec(state)?;
        if next.is_empty() { break; }
        nodes.append(&mut next);
        if is_terminated(&nodes) { break; }
    }
    Ok(join_nodes(nodes, None))
}

fn lexer_rec<'s>(state: &mut LexerState<'s>)
                 -> Result<Vec<ASTNode<'s>>, WasminError> {
    // let file = fs::read_to_string("ex.wasmin").unwrap();
    // let tokens = UnicodeSegmentation::split_word_bounds(&file);
    let mut nodes = Vec::<ASTNode>::new();

    loop {
        let token = if let Some(t) = state.next() { t } else { break; };
        if token.is_empty() { continue; }

        if let Some(elem) = as_nesting_start(token) {
            state.nesting.push(NestingToken { pos: state.pos(), elem });
            nodes.push(join_nodes(lexer_rec(state)?, Some(elem)));
            if state.nesting.is_empty() { break; } else { continue; }
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
        if token == ";" && state.nesting.is_empty() {
            nodes.push(ASTNode::End);
            break;
        }
        nodes.push(match token {
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
            "=" => ASTNode::Eq,
            "@" => ASTNode::At,
            _ => ASTNode::Str(token),
        });
    }
    Ok(nodes)
    // FIXME caller needs to do this in the top-level scope
    // } else {
    //     let last_n = state.nesting.last().unwrap();
    //     let err = if eof { "EOF" } else { "';'" };
    //     Err(werr_syntax!(format!("unexpected {}, unmatched '{}', which started at {}",
    //             err , last_n.elem, last_n.pos_str()), state.pos()))
    // }
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

#[cfg(test)]
mod tests {
    use unicode_segmentation::UnicodeWords;

    use super::*;
    use super::model::*;

    macro_rules! nest {
        (p) => {NestingElement::Parens};
        (s) => {NestingElement::Square};
        (c) => {NestingElement::Curly};
    }

    macro_rules! group {
        () => {ASTNode::Group(vec![], None)};
        (p) => {ASTNode::Group(vec![], Some(nest!(p)))};
        (s) => {ASTNode::Group(vec![], Some(nest!(s)))};
        (c) => {ASTNode::Group(vec![], Some(nest!(c)))};
        ($($a:expr), +) => {ASTNode::Group(vec![ $( $a ), * ], None)};
        (p $($a:expr), +) => {ASTNode::Group(vec![ $( $a ), * ], Some(nest!(p)))};
        (s $($a:expr), +) => {ASTNode::Group(vec![ $( $a ), * ], Some(nest!(s)))};
        (c $($a:expr), +) => {ASTNode::Group(vec![ $( $a ), * ], Some(nest!(c)))};
    }

    macro_rules! str { ($e:literal) => {ASTNode::Str($e)} }
    macro_rules! split { () => {ASTNode::Split} }
    macro_rules! end { () => {ASTNode::End} }
    macro_rules! _let { () => {ASTNode::Let} }
    macro_rules! _mut { () => {ASTNode::Mut} }
    macro_rules! _set { () => {ASTNode::Set} }
    macro_rules! _fun { () => {ASTNode::Fun} }
    macro_rules! _pub { () => {ASTNode::Pub} }
    macro_rules! _use { () => {ASTNode::Use} }
    macro_rules! _if { () => {ASTNode::If} }
    macro_rules! _then { () => {ASTNode::Then} }
    macro_rules! _else { () => {ASTNode::Else} }
    macro_rules! _def { () => {ASTNode::Def} }
    macro_rules! _ext { () => {ASTNode::Ext} }
    macro_rules! _at { () => {ASTNode::At} }
    macro_rules! _eq { () => {ASTNode::Eq} }

    macro_rules! lex {
        ($input:literal) => {{
            let mut state = LexerState::new(UnicodeSegmentation::split_word_bounds($input));
            lexer(&mut state)
        }};
    }

    macro_rules! assert_ok {
        ($left:expr, $right:expr) => { assert_eq!($left, Ok($right)) };
    }

    #[test]
    fn test_empty_expr() {
        assert_ok!(lex!(""), group!());
        assert_ok!(lex!("()"), group!(p));
        assert_ok!(lex!("( )"), group!(p));
        assert_ok!(lex!("  [    ]   "), group!(s));
        assert_ok!(lex!("  \n{ \n  }\n   "), group!(c));
    }

    #[test]
    fn test_simple_expr() {
        assert_ok!(lex!("hello"), str!("hello"));
        assert_ok!(lex!("hello_world"), str!("hello_world"));
    }

    #[test]
    fn test_group_expr() {
        assert_ok!(lex!("(hello_world)"), group!(p str!("hello_world")));
        assert_ok!(lex!("foo bar"), group!(str!("foo"), str!("bar")));
        assert_ok!(lex!("(foo bar)"), group!(p str!("foo"), str!("bar")));
        assert_ok!(lex!("[foo]"), group!(s str!("foo")));
        assert_ok!(lex!("(+ 1 2 3)"),
                   group!(p str!("+"), str!("1"), str!("2"), str!("3")));
    }

    #[test]
    fn test_group() {
        assert_ok!(lex!("foo, bar"), group!(str!("foo"), split!(), str!("bar")));
        assert_ok!(lex!("(foo, bar)"), group!(p str!("foo"), split!(), str!("bar")));
        assert_ok!(lex!("foo, bar 1, zort 2"),
            group!(str!("foo"), split!(),
                str!("bar"), str!("1"), split!(),
                str!("zort"), str!("2")));
        assert_ok!(lex!("[foo,]"), group!(s str!("foo"), split!()));
        assert_ok!(lex!("{,}"), group!(c split!()));
        assert_ok!(lex!("1, 2 ,3; ignore"),
            group!(str!("1"), split!(), str!("2"), split!(), str!("3"), end!()));
        assert_ok!(lex!("1 2 ,3; ignore"),
            group!(str!("1"), str!("2"), split!(), str!("3"), end!()));
    }

    #[test]
    fn test_group_expr_square_brackets() {
        assert_ok!(lex!("foo[bar]"), group!(str!("foo"), group!(s str!("bar"))));
        assert_ok!(lex!("[+ 1 2 3]"),
                   group!(s str!("+"), str!("1"), str!("2"), str!("3")));
    }

    #[test]
    fn test_nested_empty_expr() {
        assert_ok!(lex!("(())"), group!(p group!(p)));
        assert_ok!(lex!("([])"), group!(p group!(s)));
        assert_ok!(lex!("[({})]"), group!(s group!(p group!(c))));
        assert_ok!(lex!("{{([])}}"), group!(c group!(c group!(p group!(s)))));
    }

    #[test]
    fn test_nested_group_expr() {
        assert_ok!(lex!("foo(bar)"), group!(str!("foo"), group!(p str!("bar"))));
        assert_ok!(lex!("((foo)(bar))"), group!(p group!(p str!("foo")), group!(p str!("bar"))));
        assert_ok!(lex!("foo[(bar) zort]"), group!(str!("foo"),
            group!(s group!(p str!("bar")), str!("zort"))));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   group!(p str!("+"),
                       group!(p str!("1"),
                           group!(p str!("2"), str!("3")))));
    }

    #[test]
    fn test_nested_group() {
        assert_ok!(lex!("foo, (bar)"), group!(str!("foo"), split!(), group!(p str!("bar"))));
        assert_ok!(lex!("(foo, (bar, zort))"),
            group!(p str!("foo"), split!(), group!(p str!("bar"), split!(), str!("zort"))));
        assert_ok!(lex!("[1,foo(z,2)]"),
            group!(s str!("1"), split!(), str!("foo"),
                group!(p str!("z"), split!(), str!("2"))));
        assert_ok!(lex!("{[1,foo(z,2)], [(4,  89), 6]}"),
            group!(c
                group!(s str!("1"), split!(), str!("foo"),
                    group!(p str!("z"), split!(), str!("2"))), split!(),
                group!(s group!(p str!("4"), split!(), str!("89")), split!(), str!("6"))));
    }

    #[test]
    fn test_naked_expr() {
        assert_ok!(lex!(";"), end!());
        assert_ok!(lex!("foo;"), group!(str!("foo"), end!()));
        assert_ok!(lex!("foo ; ignored"), group!(str!("foo"), end!()));
        assert_ok!(lex!("foo bar 1 2 3; ignored"),
            group!(str!("foo"), str!("bar"),
                str!("1"), str!("2"), str!("3"), end!()));
    }

    #[test]
    fn test_naked_expr_with_nested_exprs() {
        assert_ok!(lex!("foo(bar); ignored"),
            group!(str!("foo"), group!(p str!("bar")), end!()));
        assert_ok!(lex!("foo(); ignored"),
            group!(str!("foo"), group!(p), end!()));
        assert_ok!(lex!("foo (bar 1) 2; ignored"),
            group!(str!("foo"),
                group!(p str!("bar"), str!("1")),
                str!("2"), end!()));
        assert_ok!(lex!("foo (bar 1)[2]{ 3 } ; ignored"),
            group!(str!("foo"),
                group!(p str!("bar"), str!("1")),
                group!(s str!("2")), group!(c str!("3")), end!()));
    }

    #[test]
    fn test_naked_exprs_within_nested_let_exprs() {
        assert_ok!(lex!("(foo;bar)"),
            group!(p str!("foo"), end!(), str!("bar")));
        assert_ok!(lex!("(let x = 1; let y=2; + x y)"),
            group!(p
                _let!(), str!("x"), _eq!(), str!("1"), end!(),
                _let!(), str!("y"), _eq!(), str!("2"), end!(),
                str!("+"), str!("x"), str!("y")));
    }

    #[test]
    fn test_nested_exprs_within_nested_exprs() {
        assert_ok!(lex!("mul [add 2 (3; sub 2)] 5"),
            group!(
                str!("mul"),
                group!(s str!("add"), str!("2"),
                    group!(p str!("3"), end!(), str!("sub"), str!("2"))),
                str!("5")));
    }

    #[test]
    fn test_keywords() {
        assert_ok!(lex!("let x = 1, mut y = 2, set y=add x y;"),
            group!(_let!(), str!("x"), _eq!(), str!("1"), split!(),
                _mut!(), str!("y"), _eq!(), str!("2"), split!(),
                _set!(), str!("y"), _eq!(), str!("add"), str!("x"), str!("y"), end!()));
        assert_ok!(lex!("ext mod {
            add [u32] u32;
            mul [f32] f32;
            }"), group!(_ext!(), str!("mod"), group!(c
                str!("add"), group!(s str!("u32")), str!("u32"), end!(),
                str!("mul"), group!(s str!("f32")), str!("f32"), end!())));
        assert_ok!(lex!("if cond then x else y"),
            group!(_if!(), str!("cond"), _then!(), str!("x"), _else!(), str!("y")));
        assert_ok!(lex!("pub fun f x = (sqrt x)"),
            group!(_pub!(), _fun!(), str!("f"), str!("x"), _eq!(),
                group!(p str!("sqrt"), str!("x"))));
        assert_ok!(lex!("@macro x"), group!(_at!(), str!("macro"), str!("x")));
        assert_ok!(lex!("use foo, bar;"), group!(_use!(), str!("foo"), split!(), str!("bar"), end!()));
    }

    #[test]
    fn test_iterator_can_be_reused_and_pos() {
        let words = "(a)\n(b)";
        let mut state = LexerState::new(words.split_word_bounds());
        assert_ok!(lexer(&mut state), group!(p str!("a")));
        assert_ok!(lexer(&mut state), group!(p str!("b")));
        assert_eq!(state.pos(), (2, 3));
    }

    #[test]
    fn test_new_lines_pos() {
        let words = "\n \r\n foo\nb a r";
        let mut state = LexerState::new(words.split_word_bounds());
        assert_ok!(lexer(&mut state),
            group!(str!("foo"), str!("b"), str!("a"), str!("r")));
        assert_eq!(state.pos(), (4, 5));
    }
}