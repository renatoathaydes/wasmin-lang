use crate::lexer::LexerState;

pub(super) fn parse_str<'s>(state: &mut LexerState<'s>) -> Result<&'s str, String> {
    let idx = state.idx;
    loop {
        match state.next() {
            Some("\"") => break,
            None => return Err("unclosed string".into()),
            _ => {}
        }
    }
    Ok(if idx == state.idx { "" } else {
        &state.text[idx..state.idx - 1]
    })
}