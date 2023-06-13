use wasmin::ast::{Assignment, AST, Comment, ExprType, FunKind, TopLevelElement, Type};
use wasmin::ast::Type::I32;
use wasmin::ast::Visibility::Private;
use wasmin::parse::model::Numeric;
use wasmin::parse::parser::Parser;

#[test]
fn test_simple_let_and_fun() {
    let mut ast = AST::new();
    let ten_assign = ast.new_assignment("ten",
                                        None,
                                        AST::new_number(Numeric::I32(10), vec![]));
    let add = ast.intern("add");
    let add_10 = ast.intern("add-10");
    let local_ten = ast.new_local("ten", I32, vec![]);
    let local_x = ast.new_local("x", I32, vec![]);
    let add_call = ast.new_fun_call(add, ExprType::new(vec![I32, I32], vec![I32]), FunKind::Wasm, vec![]);
    let function = ast.new_fun_interned_with_index(
        add_10, vec!["x".to_owned()],
        AST::new_group(vec![AST::new_group(vec![local_ten, local_x], vec![]),
                            add_call], vec![]),
        ExprType::new(vec![I32], vec![I32]),
        0);

    let mut parser = Parser::new_with_ast("
    let ten = 10;
    # this function adds 10 to x.
    fun add-10 x: [i32](i32) = add ten x;
    ", ast);

    assert_eq!(parser.parse_next(), Some(TopLevelElement::Let(ten_assign, Private, None, vec![])));
    assert_eq!(parser.parse_next(), Some(TopLevelElement::Fun(
        function, Private,
        Some(" this function adds 10 to x.".to_owned()),
        vec![])));
}
