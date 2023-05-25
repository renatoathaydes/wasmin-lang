use wasmin::{wcomment, wdef, wdot, wend, weq, wext, wfun, wgroup, wid, wlet, wmut, wnum, wpub, wset, wsplit};
use wasmin::errors::WasminError;
use wasmin::lexer::lex;

/// Basic lexer crate tests-old only here...
/// The most functionality is checked in the lexer unit tests-old already.
///
#[test]
fn test_lexer_basic_expression() -> Result<(), WasminError> {
    let ast = lex("let x = 1;")?;
    assert_eq!(ast, vec![wgroup!(wlet!(), wid!("x"), weq!(wgroup!(wnum!("1"), wend!())))]);
    Ok(())
}

#[test]
fn test_lexer_ext() -> Result<(), WasminError> {
    let ast = lex("ext console {
      log [i32] i32;
      foo [f32];
    }")?;
    assert_eq!(ast, vec![wgroup!(wext!(), wid!("console"),
        wgroup!(c wid!("log"), wgroup!(s wid!("i32")), wid!("i32"), wend!(),
            wid!("foo"), wgroup!(s wid!("f32")), wend!()))]);
    Ok(())
}

#[test]
fn test_lexer_commented_fun() -> Result<(), WasminError> {
    let ast = lex("
        # given the above definition for console
        fun _start = console.log 10;")?;
    assert_eq!(ast, vec![
        wgroup!(wcomment!(" given the above definition for console"),
            wfun!(), wid!("_start"), weq!(wgroup!(
                wid!("console"), wdot!(), wid!("log"), wnum!("10"), wend!())))]);
    Ok(())
}

#[test]
fn test_lexer_multiple_defs() -> Result<(), WasminError> {
    let ast = lex("
        def hello i32;
        def bye [];
        def ho [i32 f64](f32 i32);")?;
    assert_eq!(ast, vec![
        wgroup!(wdef!(), wid!("hello"), wid!("i32"), wend!()),
        wgroup!(wdef!(), wid!("bye"), wgroup!(s), wend!()),
        wgroup!(wdef!(), wid!("ho"),
            wgroup!(s wid!("i32"), wid!("f64")),
            wgroup!(p wid!("f32"), wid!("i32")),
            wend!()),
    ]);
    Ok(())
}

#[test]
fn test_lexer_example_counter() -> Result<(), WasminError> {
    let ast = lex("
        mut count = 0;

        def increment [] i32;
        pub fun increment = (set count = count, add 1; count)

        def decrement [] i32;
        pub fun decrement = (set count = count, sub 1; count)")?;

    assert_eq!(ast, vec![
        wgroup!(wmut!(), wid!("count"), weq!(wgroup!(wnum!("0"), wend!())),
            wdef!(), wid!("increment"), wgroup!(s), wid!("i32"), wend!()),
        wgroup!(wpub!(), wfun!(), wid!("increment"), weq!(
            wgroup!(p wset!(), wid!("count"), weq!(wgroup!(
                wid!("count"), wsplit!(),
                wid!("add"), wnum!("1"), wend!())), wid!("count"))),
            wdef!(), wid!("decrement"), wgroup!(s), wid!("i32"), wend!()),
        wgroup!(wpub!(), wfun!(), wid!("decrement"), weq!(
            wgroup!(p wset!(), wid!("count"), weq!(wgroup!(
                wid!("count"), wsplit!(),
                wid!("sub"), wnum!("1"), wend!())), wid!("count")))),
    ]);
    Ok(())
}
