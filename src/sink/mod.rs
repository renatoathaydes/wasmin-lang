use std::io::{Result, Write};

use crate::ast::{Assignment, Expression, TopLevelElement};

pub mod wat;
pub mod wasm;
pub mod debug;

pub type Wat = wat::Wat;
pub type Wasm = wasm::Wasm;
pub type DebugSink = debug::DebugSink;

/// A WasminSink receives [`TopLevelElement`] items as a Wasmin program is being parsed.
///
/// It has the purpose of generating output from a Wasmin program (bytecode in a compilation target,
/// output from an interpreted program, debug information etc.).
pub trait WasminSink {
    /// Start writing a module with the given name.
    fn start(&mut self, module_name: String, w: &mut Box<dyn Write>) -> Result<()>;

    /// Receive a [`TopLevelElement`] from the Wasmin parser.
    ///
    /// If the sink implementation is a compiler, the returned bytes may represent the compilation
    /// target's format (e.g. WAT text or WASM binary).
    /// If the sink is an interpreter, the bytes may be the output of the program.
    ///
    /// If an [`ErrorCode`] is returned, the Wasmin CLI exits immediately with the provided code.
    fn receive(&mut self, element: TopLevelElement, w: &mut Box<dyn Write>) -> Result<()>;

    /// Flush any state that may be pending after receiving a full Wasmin program.
    ///
    /// This method may be used to return the final output of a Wasmin program.
    ///
    /// The default implementation returns an empty [`Vec`].
    fn flush(&mut self, _w: &mut Box<dyn Write>) -> Result<()> {
        // nothing by default
        Ok(())
    }
}

fn for_each_assignment<F>(
    mut w: &mut Box<dyn Write>,
    a: &Assignment,
    mut action: F,
) -> Result<()>
    where F: FnMut(&mut Box<dyn Write>, &String, &Expression, bool) -> Result<()> {
    let (mut ids, mut exprs, mut rep) = a.clone();

    let mut err: Vec<_> = ids.drain(..)
        .zip(exprs.drain(..))
        .zip(rep.drain(..)
            .zip(std::iter::once(true).chain(std::iter::repeat(false))))
        .map(|((id, expr), (_fix, is_first))| {
            // TODO add fix conversion to expression if needed

            action(&mut w, &id, &expr, is_first)
        }).filter(|r| r.is_err())
        .collect();
    if err.is_empty() {
        Ok(())
    } else {
        err.remove(0)
    }
}

pub(crate) fn sanitize_number(text: &String) -> String {
    let without_type = if text.ends_with("i32") || text.ends_with("i64") ||
        text.ends_with("f32") || text.ends_with("f64") {
        &text[0..text.len() - 3]
    } else {
        text
    };
    without_type.replace("_", "")
}