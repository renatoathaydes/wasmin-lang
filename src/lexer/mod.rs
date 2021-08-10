use unicode_segmentation::{UnicodeSegmentation, UWordBounds};

use model::*;

use crate::errors::WasminError;
use crate::lexer::model::ASTNode::Group;
use crate::lexer::str::parse_str;

mod model;
mod str;

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
        let token = self.words.next()?;
        self.idx += token.len();
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
        assert_eq!(is_terminated(&vec![Id(""), End]), true);
        assert_eq!(is_terminated(&vec![Id(""), Id(""), End]), true);
        assert_eq!(is_terminated(&vec![Let, Id(""), End]), true);
        assert_eq!(is_terminated(&vec![End]), true);

        assert_eq!(is_terminated(&vec![Id("")]), false);
        assert_eq!(is_terminated(&vec![Split]), false);
        assert_eq!(is_terminated(&vec![Split, Id("")]), false);
        assert_eq!(is_terminated(&vec![Id(""), Group(vec![], None)]), false);
        assert_eq!(is_terminated(&vec![Id(""), Group(vec![], Some(Parens))]), false);
        assert_eq!(is_terminated(&vec![Let, Id(""), Eq, Group(vec![], Some(Parens))]), false);
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
    if !state.nesting.is_empty() {
        let last_n = state.nesting.last().unwrap();
        return Err(werr_syntax!(format!("unmatched '{}', which started at {}",
                   last_n.elem, last_n.pos_str()), last_n.pos, state.pos()));
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
        // '.' does not split words so we need special treatment for it
        if token.contains(".") {
            split_dots_or_num(&mut nodes, token);
            continue;
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
            "." => ASTNode::Dot,
            "-" => ASTNode::Dash,
            "\"" => handle_str(state, "\"")?,
            "'" => handle_str(state, "'")?,
            _ if is_num(token) => ASTNode::Num(token),
            _ => ASTNode::Id(token),
        });
    }
    Ok(nodes)
}

fn is_num(token: &str) -> bool {
    token.chars().nth(0).map_or(false, |c| c.is_digit(10))
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

#[cfg(test)]
mod tests {
    use crate::errors::ErrorPosition;

    use super::*;

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

    macro_rules! id { ($e:literal) => {ASTNode::Id($e)} }
    macro_rules! num { ($e:literal) => {ASTNode::Num($e)} }
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
    macro_rules! _dot { () => {ASTNode::Dot} }
    macro_rules! _dash { () => {ASTNode::Dash} }
    macro_rules! _eq { () => {ASTNode::Eq} }

    macro_rules! lex {
        ($input:literal) => {{
            let mut state = LexerState::new($input);
            lexer(&mut state)
        }};
    }

    macro_rules! assert_ok {
        ($left:expr, $right:expr) => { assert_eq!($left, Ok($right)) };
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
        assert_ok!(lex!(""), group!());
        assert_ok!(lex!("()"), group!(p));
        assert_ok!(lex!("( )"), group!(p));
        assert_ok!(lex!("  [    ]   "), group!(s));
        assert_ok!(lex!("  \n{ \n  }\n   "), group!(c));
    }

    #[test]
    fn test_simple_expr() {
        assert_ok!(lex!("hello"), id!("hello"));
        assert_ok!(lex!("hello_world"), id!("hello_world"));
    }

    #[test]
    fn test_group_expr() {
        assert_ok!(lex!("(hello_world)"), group!(p id!("hello_world")));
        assert_ok!(lex!("foo bar"), group!(id!("foo"), id!("bar")));
        assert_ok!(lex!("(foo bar)"), group!(p id!("foo"), id!("bar")));
        assert_ok!(lex!("[foo]"), group!(s id!("foo")));
        assert_ok!(lex!("(+ 1 2 3)"),
                   group!(p id!("+"), num!("1"), num!("2"), num!("3")));
    }

    #[test]
    fn test_group() {
        assert_ok!(lex!("foo, bar"), group!(id!("foo"), split!(), id!("bar")));
        assert_ok!(lex!("(foo, bar)"), group!(p id!("foo"), split!(), id!("bar")));
        assert_ok!(lex!("foo, bar 1, zort 2"),
            group!(id!("foo"), split!(),
                id!("bar"), num!("1"), split!(),
                id!("zort"), num!("2")));
        assert_ok!(lex!("[foo,]"), group!(s id!("foo"), split!()));
        assert_ok!(lex!("{,}"), group!(c split!()));
        assert_ok!(lex!("1, 2 ,3; ignore"),
            group!(num!("1"), split!(), num!("2"), split!(), num!("3"), end!()));
        assert_ok!(lex!("1 2 ,3; ignore"),
            group!(num!("1"), num!("2"), split!(), num!("3"), end!()));
    }

    #[test]
    fn test_group_expr_square_brackets() {
        assert_ok!(lex!("foo[bar]"), group!(id!("foo"), group!(s id!("bar"))));
        assert_ok!(lex!("[+ 1 2 3]"),
                   group!(s id!("+"), num!("1"), num!("2"), num!("3")));
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
        assert_ok!(lex!("foo(bar)"), group!(id!("foo"), group!(p id!("bar"))));
        assert_ok!(lex!("((foo)(bar))"), group!(p group!(p id!("foo")), group!(p id!("bar"))));
        assert_ok!(lex!("foo[(bar) zort]"), group!(id!("foo"),
            group!(s group!(p id!("bar")), id!("zort"))));
        assert_ok!(lex!("(+ (1 (2 3)))"),
                   group!(p id!("+"),
                       group!(p num!("1"),
                           group!(p num!("2"), num!("3")))));
    }

    #[test]
    fn test_nested_group() {
        assert_ok!(lex!("foo, (bar)"), group!(id!("foo"), split!(), group!(p id!("bar"))));
        assert_ok!(lex!("(foo, (bar, zort))"),
            group!(p id!("foo"), split!(), group!(p id!("bar"), split!(), id!("zort"))));
        assert_ok!(lex!("[1,foo(z,2)]"),
            group!(s num!("1"), split!(), id!("foo"),
                group!(p id!("z"), split!(), num!("2"))));
        assert_ok!(lex!("{[1,foo(z,2)], [(4,  89), 6]}"),
            group!(c
                group!(s num!("1"), split!(), id!("foo"),
                    group!(p id!("z"), split!(), num!("2"))), split!(),
                group!(s group!(p num!("4"), split!(), num!("89")), split!(), num!("6"))));
    }

    #[test]
    fn test_naked_expr() {
        assert_ok!(lex!(";"), end!());
        assert_ok!(lex!("foo;"), group!(id!("foo"), end!()));
        assert_ok!(lex!("foo ; ignored"), group!(id!("foo"), end!()));
        assert_ok!(lex!("foo bar 1 2 3; ignored"),
            group!(id!("foo"), id!("bar"),
                num!("1"), num!("2"), num!("3"), end!()));
    }

    #[test]
    fn test_naked_expr_with_nested_exprs() {
        assert_ok!(lex!("foo(bar); ignored"),
            group!(id!("foo"), group!(p id!("bar")), end!()));
        assert_ok!(lex!("foo(); ignored"),
            group!(id!("foo"), group!(p), end!()));
        assert_ok!(lex!("foo (bar 1) 2; ignored"),
            group!(id!("foo"),
                group!(p id!("bar"), num!("1")),
                num!("2"), end!()));
        assert_ok!(lex!("foo (bar 1)[2]{ 3 } ; ignored"),
            group!(id!("foo"),
                group!(p id!("bar"), num!("1")),
                group!(s num!("2")), group!(c num!("3")), end!()));
    }

    #[test]
    fn test_naked_exprs_within_nested_let_exprs() {
        assert_ok!(lex!("(foo;bar)"),
            group!(p id!("foo"), end!(), id!("bar")));
        assert_ok!(lex!("(let x = 1; let y=2; + x y)"),
            group!(p
                _let!(), id!("x"), _eq!(), num!("1"), end!(),
                _let!(), id!("y"), _eq!(), num!("2"), end!(),
                id!("+"), id!("x"), id!("y")));
    }

    #[test]
    fn test_nested_exprs_within_nested_exprs() {
        assert_ok!(lex!("mul [add 2 (3; sub 2)] 5"),
            group!(
                id!("mul"),
                group!(s id!("add"), num!("2"),
                    group!(p num!("3"), end!(), id!("sub"), num!("2"))),
                num!("5")));
    }

    #[test]
    fn test_keywords() {
        assert_ok!(lex!("let x = 1, mut y = 2, set y=add x y;"),
            group!(_let!(), id!("x"), _eq!(), num!("1"), split!(),
                _mut!(), id!("y"), _eq!(), num!("2"), split!(),
                _set!(), id!("y"), _eq!(), id!("add"), id!("x"), id!("y"), end!()));
        assert_ok!(lex!("ext mod {
            add [u32] u32;
            mul [f32] f32;
            }"), group!(_ext!(), id!("mod"), group!(c
                id!("add"), group!(s id!("u32")), id!("u32"), end!(),
                id!("mul"), group!(s id!("f32")), id!("f32"), end!())));
        assert_ok!(lex!("if cond then x else y"),
            group!(_if!(), id!("cond"), _then!(), id!("x"), _else!(), id!("y")));
        assert_ok!(lex!("pub fun f x = (sqrt x)"),
            group!(_pub!(), _fun!(), id!("f"), id!("x"), _eq!(),
                group!(p id!("sqrt"), id!("x"))));
        assert_ok!(lex!("@macro x"), group!(_at!(), id!("macro"), id!("x")));
        assert_ok!(lex!("use foo, bar;"), group!(_use!(), id!("foo"), split!(), id!("bar"), end!()));
    }

    #[test]
    fn test_dot() {
        assert_ok!(lex!("."), _dot!());
        assert_ok!(lex!(".a"), group!(_dot!(), id!("a")));
        assert_ok!(lex!("a."), group!(id!("a"), _dot!()));
        assert_ok!(lex!("foo.bar"), group!(id!("foo"), _dot!(), id!("bar")));
        assert_ok!(lex!("foo\n  .bar"), group!(id!("foo"), _dot!(), id!("bar")));
        assert_ok!(lex!("foo.\n  bar"), group!(id!("foo"), _dot!(), id!("bar")));
        assert_ok!(lex!("(foo . bar x)"),
            group!(p id!("foo"), _dot!(), id!("bar"), id!("x")));
        assert_ok!(lex!("(( foo ).bar )"),
            group!(p group!(p id!("foo")), _dot!(), id!("bar")));
        assert_ok!(lex!("[( foo ). (bar )]"),
            group!(s group!(p id!("foo")), _dot!(), group!(p id!("bar"))));
    }

    #[test]
    fn test_pos_numbers() {
        assert_ok!(lex!("0"), num!("0"));
        assert_ok!(lex!("1"), num!("1"));
        assert_ok!(lex!("1.0"), num!("1.0"));
        assert_ok!(lex!("0.1"), num!("0.1"));
        assert_ok!(lex!("1i32"), num!("1i32"));
        assert_ok!(lex!("10_000f32"), num!("10_000f32"));
        assert_ok!(lex!("10_000.62f64"), num!("10_000.62f64"));
        // the lexer does not validate numbers fully
        assert_ok!(lex!("2Z"), num!("2Z"));
    }

    #[test]
    fn test_neg_numbers() {
        assert_ok!(lex!("-0"), group!(_dash!(), num!("0")));
        assert_ok!(lex!("-1"), group!(_dash!(), num!("1")));
        assert_ok!(lex!("-1.0"), group!(_dash!(), num!("1.0")));
        assert_ok!(lex!("-0.1"), group!(_dash!(), num!("0.1")));
        assert_ok!(lex!("-1i32"), group!(_dash!(), num!("1i32")));
        assert_ok!(lex!("-10_000f32"), group!(_dash!(), num!("10_000f32")));
        assert_ok!(lex!("-10_000.62f64"), group!(_dash!(), num!("10_000.62f64")));
        // the lexer does not validate numbers fully
        assert_ok!(lex!("-2Z"), group!(_dash!(), num!("2Z")));
    }

    #[test]
    fn test_not_numbers() {
        assert_ok!(lex!("a0"), id!("a0"));
        assert_ok!(lex!("_1"), id!("_1"));
        assert_ok!(lex!("@b4"), group!(_at!(), id!("b4")));
    }

    #[test]
    fn test_str() {
        assert_ok!(lex!("\"\""), str!(""));
        assert_ok!(lex!("\"hello\""), str!("hello"));
        assert_ok!(lex!("\"A full sentence...\""), str!("A full sentence..."));
        assert_ok!(lex!("\"pub fun let 'keywords' = 2, 4)\""),
            str!("pub fun let 'keywords' = 2, 4)"));
        assert_ok!(lex!("let s = \"hello\";"),
            group!(_let!(), id!("s"), _eq!(), str!("hello"), end!()));
        assert_ok!(lex!("(concat \"hello\" [fst \"bar\"])"),
            group!(p id!("concat"), str!("hello"), group!(s id!("fst"), str!("bar"))));
    }

    #[test]
    fn test_str_single_quote() {
        assert_ok!(lex!("'hello'"), str!("hello"));
        assert_ok!(lex!("'A full sentence...'"), str!("A full sentence..."));
        assert_ok!(lex!("'pub fun let \"keywords\" = 2, 4)'"),
            str!("pub fun let \"keywords\" = 2, 4)"));
    }

    #[test]
    fn test_str_escapes() {
        assert_ok!(lex!("'hello\\n'"), str!("hello\\n"));
        assert_ok!(lex!("'hello\\n\\r'"), str!("hello\\n\\r"));
        assert_ok!(lex!("'hello\\t'"), str!("hello\\t"));
        assert_ok!(lex!("'hello\\''"), str!("hello\\'"));
        assert_ok!(lex!("\"hello\\\"\""), str!("hello\\\""));
    }

    #[test]
    fn test_str_unfinished() {
        assert_syntax_err!(lex!("\"foo bar"),
            "unclosed string", (1, 1), (1, 8));
    }

    #[test]
    fn test_unclosed_group_error() {
        assert_syntax_err!(lex!("(x"),
            "unmatched '(', which started at 1:1", (1, 1), (1, 2));
        assert_syntax_err!(lex!("foo[x;"),
            "unmatched '[', which started at 1:4", (1, 4), (1, 6));
        assert_syntax_err!(lex!("fun x y = {\n  (\n abc ; \n )"),
            "unmatched '{', which started at 1:11", (1, 11), (4, 2));
    }

    #[test]
    fn test_unmatched_closing_group_error() {
        assert_syntax_err!(lex!("x)"),
            "mismatched ')', closes nothing", (1, 2), (1, 2));
        assert_syntax_err!(lex!("ext {
            foo [] u64;
            bar ];
        }\n  "),
            "misplaced ']', expecting '}', which started at 1:5", (3, 17), (3, 17));
    }

    #[test]
    fn test_iterator_can_be_reused_and_pos() {
        let words = "(a)\n(b)";
        let mut state = LexerState::new(words);
        assert_ok!(lexer(&mut state), group!(p id!("a")));
        assert_ok!(lexer(&mut state), group!(p id!("b")));
        assert_eq!(state.pos(), (2, 3));
    }

    #[test]
    fn test_new_lines_pos() {
        let words = "\n \r\n foo\nb a r";
        let mut state = LexerState::new(words);
        assert_ok!(lexer(&mut state),
            group!(id!("foo"), id!("b"), id!("a"), id!("r")));
        assert_eq!(state.pos(), (4, 5));
    }
}