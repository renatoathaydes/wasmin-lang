use crate::lexer::LexerState;

pub(super) fn parse_str<'s>(state: &mut LexerState<'s>, end: &str)
                            -> Result<&'s str, String> {
    let idx = state.idx;
    loop {
        match state.next() {
            Some("\\") => {
                // don't even look at next char, anything goes
                // TODO but later we should check it's a valid escape seq
                if state.next().is_none() {
                    return Err("unclosed string".into());
                }
            }
            Some(s) if s == end => break,
            None => return Err("unclosed string".into()),
            _ => {}
        }
    }
    Ok(if idx == state.idx { "" } else {
        &state.text[idx..state.idx - 1]
    })
}
