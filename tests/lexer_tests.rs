use wasmin::{wdot, wend, weq, wext, wfun, wgroup, wid, wlet, wnum};
use wasmin::errors::WasminError;
use wasmin::lexer::lex;
use wasmin::lexer::model::ASTNode;

/// Basic lexer crate tests only here...
/// The most functionality is checked in the lexer unit tests already.
///
#[test]
fn test_lexer_external() -> Result<(), WasminError> {
    let ast = lex("let x = 1;")?;
    assert_eq!(ast, wgroup!(wlet!(), wid!("x"), weq!(), wnum!("1"), wend!()));

    let ast = lex("ext console {
      log [i32] i32;
      foo [f32];
    }")?;
    assert_eq!(ast, wgroup!(wext!(), wid!("console"),
        wgroup!(c wid!("log"), wgroup!(s wid!("i32")), wid!("i32"), wend!(),
            wid!("foo"), wgroup!(s wid!("f32")), wend!())));

    Ok(())
}
