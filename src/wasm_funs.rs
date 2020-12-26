use crate::parse::stack::Stack;
use crate::types::Type;
use crate::types::Type::{F32, F64, I32, I64};

pub fn wasm_std_funs() -> Stack {
    let mut stack = Stack::new();

    // ibinop && fbinop
    [I32, I64, F32, F64].iter().for_each(|t| {
        ["add", "mul", "sub"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone() t.clone()](t.clone()))]), true).unwrap();
        });
    });

    stack
}
