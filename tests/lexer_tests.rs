use wasmin::errors::WasminError;
use wasmin::lexer::*;
use wasmin::lexer::model::ASTNode;

/// Basic lexer crate tests only here...
/// The most functionality is checked in the lexer unit tests already.
///
#[test]
fn test_lexer_external() -> Result<(), WasminError> {
    let ast = lex("let x = 1;")?;
    assert_eq!(ast, wgroup!(wlet!(), wid!("x"), weq!(), wnum!("1"), wend!()));
    Ok(())
}
