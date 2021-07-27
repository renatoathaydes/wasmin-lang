#[macro_use]
pub mod errors;
#[macro_use]
pub mod ast;
mod lexer;
pub mod parse;
pub mod sink;
pub mod types;
mod vec_utils;
mod wasm_funs;
pub mod wasm_parse;

#[cfg(test)]
mod tests;
