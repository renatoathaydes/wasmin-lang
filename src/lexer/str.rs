use crate::lexer::LexerState;

pub(super) fn parse_str<'s>(state: &mut LexerState<'s>) -> &'s str {
    let idx = state.idx;
    loop {
        match state.next() {
            Some("\"") | None => break,
            _ => {}
        }
    }
    if idx == state.idx { return ""; }
    &state.text[idx..state.idx - 1]
}