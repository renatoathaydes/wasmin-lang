use std::fmt::{Display, Formatter};

/// Expression is the basic unit of Wasmin code.
#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum ASTNode<'s> {
    /// Basic identifier.
    Id(&'s str),
    /// Number.
    Num(&'s str),
    /// Constant String
    Str(&'s str),
    /// Grouped expression that introduces nesting.
    Group(Vec<ASTNode<'s>>, Option<NestingElement>),
    /// A comma splitting an expression into a multi-value expression.
    Split,
    /// A semi-colon ending the current expression.
    End,
    /// let keyword
    Let,
    /// mut keyword
    Mut,
    /// set keyword
    Set,
    /// fun keyword
    Fun,
    /// pub keyword
    Pub,
    /// use keyword
    Use,
    /// if keyword
    If,
    /// then keyword
    Then,
    /// else keyword
    Else,
    /// def keyword
    Def,
    /// ext keyword
    Ext,
    /// '=' symbol
    Eq,
    /// '@' symbol
    At,
    /// '.' symbol
    Dot,
    /// '-' symbol
    Dash,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq, Copy)]
pub enum NestingElement {
    Parens,
    Square,
    Curly,
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub struct NestingToken {
    pub elem: NestingElement,
    pub pos: (usize, usize),
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

impl NestingToken {
    pub fn closing_char(&self) -> char {
        match self.elem {
            NestingElement::Parens => ')',
            NestingElement::Square => ']',
            NestingElement::Curly => '}',
        }
    }

    pub fn pos_str(&self) -> String {
        format!("{}:{}", self.pos.0, self.pos.1)
    }
}