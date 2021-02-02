use crate::parse::stack::Stack;
use crate::types::Type;
use crate::types::Type::{F32, F64, I32, I64};

pub fn wasm_std_funs() -> Stack {
    let mut stack = Stack::new();

    // ibinop && fbinop
    [I32, I64, F32, F64].iter().for_each(|t| {
        ["add", "mul", "sub", "eq", "ne"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone() t.clone()](t.clone()))]), true, false).unwrap();
        });
    });
    [I32, I64].iter().for_each(|t| {
        ["gt_s", "gt_u", "ge_s", "ge_u", "lt_s", "lt_u", "le_s", "le_u",
            "div_u", "div_s", "rem_u", "rem_s", "and", "or", "xor",
            "shl", "shr_u", "shr_s", "rotl", "rotr"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone() t.clone()](t.clone()))]), true, false).unwrap();
        });
    });
    [F32, F64].iter().for_each(|t| {
        ["gt", "ge", "lt", "le", "div", "min", "max", "copysign"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone() t.clone()](t.clone()))]), true, false).unwrap();
        });
    });

    [I32, I64].iter().for_each(|t| {
        ["clz", "ctz", "popcnt", "eqz"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone()](t.clone()))]), true, false).unwrap();
        });
    });

    [F32, F64].iter().for_each(|t| {
        ["abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone()](t.clone()))]), true, false).unwrap();
        });
    });

    stack
}
