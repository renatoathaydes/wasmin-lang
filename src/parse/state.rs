use crate::ast::Expression;
use crate::parse::parser::{GroupingState, GroupingSymbol};
use crate::types::Type;
use crate::vec_utils::{get_last, get_last_mut, remove_last, remove_last_n};

pub struct ParsingState<'s> {
    symbols: GroupingState,
    pub stack: &'s mut Vec<Type>,
    expr_parts: Vec<Vec<ExprPart>>,
    exprs: Vec<Vec<Expression>>,
    block_info: Vec<Option<BlockInfo>>,
    is_root: bool,
}

#[derive(Debug, Clone)]
struct BlockInfo {
    type_count_at_start: usize
}

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub(crate) enum ExprPart {
    Arg(String, (usize, usize)),
    Fun(String, (usize, usize)),
    Expr(Expression, (usize, usize)),
}

impl ExprPart {
    pub(crate) fn is_expression(&self) -> bool {
        matches!(self, ExprPart::Expr(_))
    }
}

impl<'s> ParsingState<'s> {
    pub fn new(stack: &'s mut Vec<Type>) -> ParsingState<'s> {
        ParsingState {
            symbols: GroupingState::new(),
            stack,
            exprs: vec![vec![]],
            expr_parts: vec![vec![]],
            block_info: vec![None],
            is_root: true,
        }
    }

    pub fn new_nested(state: &'s mut ParsingState) -> ParsingState<'s> {
        let block_info = state.block_info.clone();
        ParsingState {
            symbols: GroupingState::new(),
            stack: &mut state.stack,
            exprs: vec![vec![]],
            expr_parts: vec![vec![]],
            block_info,
            is_root: false,
        }
    }

    pub fn symbols(&self) -> &GroupingState {
        &self.symbols
    }

    pub fn has_non_expr_part(&self) -> bool {
        get_last(&self.expr_parts).iter().any(|e| !e.is_expression())
    }

    pub fn enter_level(&mut self, symbol: GroupingSymbol) {
        self.symbols.enter(symbol);
        self.expr_parts.push(vec![]);
        self.exprs.push(vec![]);
        self.block_info.push(None);
    }

    pub fn exit_level(&mut self,
                      symbol: GroupingSymbol,
    ) -> Result<bool, String> {
        if self.symbols.is_inside(&symbol) {
            self.symbols.exit_symbol();
            let parts = self.end_expr();
            if !parts.is_empty() {
                panic!("Exiting level without consuming expression parts first: {:?}", parts)
            }
            remove_last_n(&mut self.expr_parts, 1);
            remove_last_n(&mut self.block_info, 1);
            let level = remove_last(&mut self.exprs);
            self.push_exprs(level);
            Ok(true)
        } else if self.is_root {
            Err(format!("Closing {} does not close anything", symbol))
        } else {
            Ok(false)
        }
    }

    fn curr_level(&mut self) -> &mut Vec<Expression> {
        get_last_mut(&mut self.exprs)
    }

    fn curr_parts(&mut self) -> &mut Vec<ExprPart> {
        get_last_mut(&mut self.expr_parts)
    }

    pub(crate) fn start_block(&mut self) {
        get_last_mut(&mut self.block_info)
            .replace(BlockInfo { type_count_at_start: self.stack.len() });
    }

    pub(crate) fn get_stack_count_at_block_start(&self) -> Option<usize> {
        self.block_info.iter().rfind(|info| info.is_some())
            .map(|info| info.as_ref().unwrap().type_count_at_start)
    }

    pub(crate) fn push_expr_part(&mut self, part: ExprPart) {
        get_last_mut(&mut self.expr_parts).push(part);
    }

    pub(crate) fn end_expr(&mut self) -> Vec<ExprPart> {
        self.curr_parts().drain(..).collect()
    }

    pub(crate) fn push_exprs(&mut self, mut exprs: Vec<Expression>) {
        self.curr_level().append(&mut exprs);
    }

    pub fn finish_off(&mut self) -> Expression {
        let level: Vec<_> = self.curr_level().drain(..).collect();
        group_exprs(level)
    }

    pub fn verify_end_state(&mut self) {
        if self.expr_parts.len() != 1 {
            panic!("Expected one level of expression parts left, but got {:?}", self.expr_parts);
        }
        if self.exprs.len() != 1 {
            panic!("Expected one level of expressions left, but got {:?}", self.exprs);
        }
        let parts = self.expr_parts.remove(0);
        if !parts.is_empty() {
            panic!("Expected all expression parts to have been consumed, but still have parts \
                remaining: {:?}", parts)
        }

        let level = self.exprs.remove(0);
        if !level.is_empty() {
            panic!("Expected all expressions to have been consumed, but still have expressions \
                remaining: {:?}", level)
        }
    }
}

fn group_exprs(mut exprs: Vec<Expression>) -> Expression {
    match exprs.len() {
        0 => Expression::Empty,
        1 => remove_last(&mut exprs),
        _ => Expression::Group(exprs),
    }
}

#[cfg(test)]
mod state_tests {
    use crate::types::{Type::{*}};

    use super::*;

    macro_rules! arg {
        ($id:literal) => { ExprPart::Arg($id.to_owned()) };
    }

    macro_rules! fun {
        ($id:literal) => { ExprPart::Fun($id.to_owned()) };
    }

    macro_rules! expr {
        ($e:expr) => { ExprPart::Expr($e) };
    }

    #[test]
    fn basic_usage() {
        let mut stack = Vec::new();
        let mut state = ParsingState::new(&mut stack);

        state.push_expr_part(fun!("hi"));
        state.push_expr_part(expr!(expr_const!("3" I32)));
        assert_eq!(state.end_expr(), vec![fun!("hi"), expr!(expr_const!("3" I32))]);

        // shouldn't panic
        state.verify_end_state();
    }

    #[test]
    fn can_merge_levels_when_exit_level() {
        let mut stack = Vec::new();
        let mut state = ParsingState::new(&mut stack);

        state.push_exprs(vec![expr_const!("2" I32)]);
        state.enter_level(GroupingSymbol::Parens);
        state.push_expr_part(arg!("3"));
        state.push_expr_part(expr!(expr_const!("4" I32)));

        assert_eq!(state.end_expr(), vec![arg!("3"), expr!(expr_const!("4" I32))]);

        state.push_exprs(vec![expr_const!("3" I32), expr_const!("4" I32)]);

        assert!(state.exit_level(GroupingSymbol::Parens).unwrap());

        assert_eq!(state.curr_level(), &vec![
            expr_const!("2" I32), expr_const!("3" I32), expr_const!("4" I32)
        ]);

        assert_eq!(state.finish_off(), Expression::Group(vec![
            expr_const!("2" I32), expr_const!("3" I32), expr_const!("4" I32)
        ]));

        // shouldn't panic
        state.verify_end_state();
    }

    #[test]
    fn can_remember_block_start() {
        let mut stack = vec![Type::I32];
        let mut state = ParsingState::new(&mut stack);

        state.start_block();
        assert_eq!(state.get_stack_count_at_block_start(), Some(1));

        state.enter_level(GroupingSymbol::Parens);
        state.stack.push(Type::F32);
        state.enter_level(GroupingSymbol::Parens);
        state.stack.push(Type::F64);

        assert_eq!(state.get_stack_count_at_block_start(), Some(1));
        state.start_block();
        assert_eq!(state.get_stack_count_at_block_start(), Some(3));
        state.exit_level(GroupingSymbol::Parens).unwrap();
        assert_eq!(state.get_stack_count_at_block_start(), Some(1));
        state.start_block();
        assert_eq!(state.get_stack_count_at_block_start(), Some(3));
        state.exit_level(GroupingSymbol::Parens).unwrap();
        assert_eq!(state.get_stack_count_at_block_start(), Some(1));
    }
}
