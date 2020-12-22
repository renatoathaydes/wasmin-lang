A minimal, concatenative, statically typed programming language that builds on the primitives provided by
[WebAssembly (WASM)](https://developer.mozilla.org/en-US/docs/WebAssembly).

## Wasmin Goals

- stay close to WASM for fast compilation and zero runtime dependencies.
- no memory management nor garbage collection (GC) by using a linear type system.
- mix the functional and concatenative (exposing the WASM stack) programming paradigms.
- simplest possible syntax that preserves readability.

## This is work in progress

Feature Checklist:

- [x] primitive values.
- [x] parenthesis-grouped expressions.
- [x] ungrouped expressions.
- [x] multi-expressions.
- [x] type declarations.
- [ ] generic type declarations.
- [x] let assignments.
- [x] mut assignments.
- [ ] math operators.
- [x] function calls.
- [x] function implementations.
- [ ] generic functions.
- [ ] single-line comments.
- [ ] multi-line comments.
- [ ] global constants.
- [ ] import from other Wasmin files.
- [ ] import external functions.
- [ ] export functions and constants.
- [ ] if/else blocks.
- [ ] loops.
- [ ] stack operator `>`.
- [ ] string values.
- [ ] function pointers.
- [ ] arrays.
- [ ] records.
- [ ] generic records.
- [ ] special functions (`get`, `set`, `remove`, `size`, `copy`).
- [ ] `typeof` special function.

Not yet designed features (may never be added):

- pattern matching.
- type checks.
- threads.
- [SIMD](https://medium.com/wasmer/webassembly-and-simd-13badb9bf1a8).
- [WASI interface types](https://hacks.mozilla.org/2019/08/webassembly-interface-types/).
- embed WAT code inside Wasmin.

## The language

Wasmin is designed to be simple, built from very few generic syntactic forms,
and therefore fast to parse and compile, like WASM itself!

It attempts to minimize punctuation to be as syntactically light as possible without losing readability.

Because it only contains primitives that can be mapped easily to WASM, it should run as fast 
as hand-written WASM programs on any platform.

Wasmin is statically typed, non-garbage-collected
(but requires no memory management thanks to [linear types](http://home.pipeline.com/~hbaker1/ForthStack.html),
which do not generate garbage) and supports the procedural and concatenative programming paradigms.

### Basics

The basic constructs of a Wasmin program are **expressions**.

Expressions are simply arrangements of symbols, constants and other expressions which evaluate to a value or perform some
side-effect.

An expression may consist of several sub-expressions that are evaluated in sequence within a parenthesis-demarked
group. Its value is that of the last sub-expression. To separate each sub-expression, either group each of them within
parenthesis, or add a semi-colon `;` between them.

For example, these are all expressions:

- `0` (the constant `0`, of type `i64`, or 64-bit integer).
- `(0)` (same as previous).
- `add 1 2` (calls function<sup><a href="#footnote-1">[1]</a></sup> `add` with arguments `1` and `2`).
- `(let n = 1; add n 3)` (one expression grouping two others<sup><a href="#footnote-2">[2]</a></sup> - evaluates to the result of the last one).
- `1 > 2 > add` (same as `add 1 2`, using concatenative style).

> Wasmin source code must always be encoded using UTF-8.

### Let expressions

In order to bind the value of an expression to an identifier, a `let` expression can be used.

Let expressions always evaluate to `empty`, or `()` (which cannot be assigned or returned) and have the form:

```
let <identifiers>,* = <expressions>,*
```

For example:

```rust
let constant-ten = 10;

let one, two = 1, 2;

let three, four = (
    let t = 3;
    let f = 4;
    t, f
)
```

Optionally, the type of an identifier can be defined with the `def` keyword before it's assigned:

```rust
def ten i32;
let ten = 10;
```

This is mostly useful when exporting an identifier, as we'll see later.

### Mut expressions

Mut expressions are almost exactly like `let` expressions, but allow the declared variable
to be both re-assigned and mutated (in the case of arrays and record types, as we'll see later).

For example:

```rust
mut counter = 0;

# increment the counter
set counter = counter > add 1;
```
