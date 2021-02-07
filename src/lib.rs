#[macro_use]
pub mod ast;
pub mod types;
pub mod parse;
pub mod sink;
pub mod wasm_parse;
mod wasm_funs;
mod vec_utils;

#[cfg(test)]
mod tests;
