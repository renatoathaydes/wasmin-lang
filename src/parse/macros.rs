/// space_or takes a char and two expressions.
/// The first expression is evaluated if the char is whitespace.
/// The second expression is evaluated otherwise.
macro_rules! space_or {
    ( $c:expr, $space:expr, $other:expr ) => {
        match $c {
            ' ' | '\n' | '\t' | '\r' => $space,
            _ => $other
        }
    };
}

/// sep_or takes a char and two expressions.
/// The first expression is evaluated if the char is a separator (other than spaces).
/// The second expression is evaluated otherwise.
macro_rules! sep_or {
    ( $c:expr, $sep:expr, $other:expr ) => {
        match $c {
            '[' | ']' | '(' | ')' | '{' | '}' | '=' | '"' |
            ',' | ';' | ':' | '<' | '>' | '#' | '|' => $sep,
            _ => $other
        }
    };
}

/// space_sep_or takes a char and two expressions.
/// The first expression is evaluated if the char is a space or separator.
/// The second expression is evaluated otherwise.
macro_rules! space_sep_or {
    ( $c:expr, $yes:expr, $no:expr ) => {
        space_or!($c, $yes, sep_or!($c, $yes, $no))
    };
}
