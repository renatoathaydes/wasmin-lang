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
        ["clz", "ctz", "popcnt", "eqz", "extend_8s", "extend_16s"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone()](t.clone()))]), true, false).unwrap();
        });
    });

    [F32, F64].iter().for_each(|t| {
        ["abs", "neg", "sqrt", "ceil", "floor", "trunc", "nearest"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([t.clone()](t.clone()))]), true, false).unwrap();
        });
    });

    // conversion functions

    [(F32, I32), (F64, I32)].for_each(|(from, to)| {
        ["convert_i32_u", "convert_i32_s"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([from.clone()](to.clone()))]), true, false).unwrap();
        });
    });

    [(F32, I64), (F64, I64)].for_each(|(from, to)| {
        ["convert_i64_u", "convert_i64_s"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([from.clone()](to.clone()))]), true, false).unwrap();
        });
    });

    [(F64, I32), (F64, I64)].for_each(|(from, to)| {
        ["trunc_f64_s", "trunc_f64_u", "trunc_sat_f64_s", "trunc_sat_f64_u"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([from.clone()](to.clone()))]), true, false).unwrap();
        });
    });

    [(F32, I32), (F32, I64)].for_each(|(from, to)| {
        ["trunc_sat_f32_s", "trunc_sat_f32_u"].iter().for_each(|name| {
            stack.push(name.to_owned().to_owned(), Type::WasmFn(vec![fun_type!([from.clone()](to.clone()))]), true, false).unwrap();
        });
    });

    stack.push("wrap_i64".to_owned(), Type::WasmFn(vec![fun_type!([I64](I32))]), true, false).unwrap();
    stack.push("extend_32s".to_owned(), Type::WasmFn(vec![fun_type!([I64](I64))]), true, false).unwrap();
    stack.push("demote_f64".to_owned(), Type::WasmFn(vec![fun_type!([F64](F32))]), true, false).unwrap();
    stack.push("promote_f32".to_owned(), Type::WasmFn(vec![fun_type!([F32](F64))]), true, false).unwrap();
    stack.push("reinterpret_i32".to_owned(), Type::WasmFn(vec![fun_type!([I32](F32))]), true, false).unwrap();
    stack.push("reinterpret_f32".to_owned(), Type::WasmFn(vec![fun_type!([F32](I32))]), true, false).unwrap();
    stack.push("reinterpret_i64".to_owned(), Type::WasmFn(vec![fun_type!([I64](F64))]), true, false).unwrap();
    stack.push("reinterpret_f64".to_owned(), Type::WasmFn(vec![fun_type!([F64](I64))]), true, false).unwrap();

    stack
}
