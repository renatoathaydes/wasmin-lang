use crate::ast::{Assignment, Expression, TopLevelExpression, Visibility};
use crate::types::Type;

pub mod wat;
pub mod wasm;
pub mod debug;

pub type Wat = wat::Wat;
pub type Wasm = wasm::Wasm;
pub type DebugSink = debug::DebugSink;

pub type ErrorCode = i32;

/// A WasminSink receives [`TopLevelExpression`] items as a Wasmin program is being parsed.
///
/// It has the purpose of generating output from a Wasmin program (bytecode in a compilation target,
/// output from an interpreted program, debug information etc.).
pub trait WasminSink {
    /// Start writing a module with the given name.
    fn start(&mut self, module_name: String) -> Vec<u8>;

    /// Receive a [`TopLevelExpression`] from the Wasmin parser.
    ///
    /// If the sink implementation is a compiler, the returned bytes may represent the compilation
    /// target's format (e.g. WAT text or WASM binary).
    /// If the sink is an interpreter, the bytes may be the output of the program.
    ///
    /// If an [`ErrorCode`] is returned, the Wasmin CLI exits immediately with the provided code.
    fn receive(&mut self, expr: TopLevelExpression) -> Result<Vec<u8>, ErrorCode>;

    /// Flush any state that may be pending after receiving a full Wasmin program.
    ///
    /// This method may be used to return the final output of a Wasmin program.
    ///
    /// The default implementation returns an empty [`Vec`].
    fn flush(&mut self) -> Result<Vec<u8>, ErrorCode> {
        // nothing by default
        Ok(Vec::new())
    }
}

fn for_each_assignment<F, V, E>(a: Assignment, mut action: F) -> Result<Vec<V>, E>
    where F: FnMut(&String, &Expression) -> Result<V, E> {
    let (mut ids, mut exprs, mut rep) = a;

    ids.drain(..)
        .zip(exprs.drain(..))
        .zip(rep.drain(..))
        .map(|((id, expr), fix)| {
            // TODO add fix conversion to expression if needed

            action(&id, &expr)
        }).collect()
}
