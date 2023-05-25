#[macro_use]
pub mod ast;
pub mod lexer;
pub mod parse;
pub mod sink;
pub mod types;
mod vec_utils;
mod wasm_funs;
pub mod wasm_parse;

#[cfg(test)]
mod tests;
