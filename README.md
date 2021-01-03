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
- [x] set assignments.
- [ ] math operators.
- [x] function calls.
- [x] function implementations.
- [ ] generic functions.
- [ ] single-line comments.
- [ ] multi-line comments.
- [x] global constants.
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
- [ ] `typeof` special function.
- [ ] traits.

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
(but requires no memory management thanks to [linear types](http://home.pipeline.com/~hbaker1/ForthStack.html), which do
not generate garbage) and supports the procedural and concatenative programming paradigms.

## Tour of Wasmin

The basic constructs of a Wasmin program are **expressions**.

Expressions are arrangements of symbols, constants and other expressions which evaluate to zero or more values.

Expressions have the following forms:

- `()` empty expression.
- `1`, `0.1`, `1i64`, `0.1f64` (constants).
- `let name = expr;` (single let expression, used for immutable assignments).
- `let name1, name2 = expr1, expr2` (multi-value let expression).
- `mut name = expr;` (`mut` is like let, but for mutable assignments).
- `set name = expr;` (`set` is used to re-assign a `mut` variable).
- `function arg1 arg2;` (function call).
- `(function arg1 arg2)` (alternative function call syntax).
- `function(arg1, arg2);` (same as previous two examples, uses multi-values to mimic C-like function call).
- `(expr1; expr2)` (group of expressions, evaluates to the value of the last one).
- `expr1 > expr2 > function;` (concatenative style, each expression is pushed to the stack)

For example, these are all expressions:

- `0` (the constant `0`, of type `i64`, or 64-bit integer).
- `(0)` (same as previous).
- `add 1 2` (calls the native WASM `i32.add` function with arguments `1` and `2`, which have type `i32`).
- `(let n = 1; add n 3)` (assigns `1` to the variable `n`, then calls `add` with `n` and `3` as arguments).
- `1 > 2 > add` (same as `add 1 2`, using concatenative style).
- `1 >add 2` (same as previous example).

The last example shows that the concatenative style allows certain expressions to be written in a more natural way, in
this case using what looks like the familiar "infix operator" syntax.

Notice that expressions within parenthesis are self-contained. You cannot do this for example:

```rust
# wrong!
let x = (add 2) 3;
```

The above example would try to assign the result of `add 2` to `x` (which won't work because `add` takes two arguments),
then the expression `3` appears outside of the previous `let` expression, in an illegal location
(only `let`, `mut` and as we'll see below, `def` and `fun` can appear as top-level elements).

These, however, would be fine:

```rust
# ok!
let x = add (2) (3);
let y = (add 2 3)
let z = add (2, 3);
```

Basically, if an expression does not start with `(`, it must end with `;`.

Multi-value expressions are separated by `,`.

In the example above, `add (2, 3)` actually creates an expression whose second term evalutes to two values, `2` and `3`,
which then are passed as arguments to the first term, the `add`
function, and hence is equivalent to just `(add 2 3)` (but _accidentally_ looks like a C-like function call).

> Wasmin source code must always be encoded using UTF-8.

To complete the description of the Wasmin syntax, there are just a couple more concepts to learn:

- `def` defines the type of a term.
- `fun` is used to implement a function.

`def`s are optional for `let` and `mut` bindings (it is always possible to infer their types), but mandatory for `fun`.

To make a `fun` or `let` visible outside the module (i.e. add them to the WASM module's exports), declare them
with `pub`. For example, `pub let PI = 3.1415;`.

> Notice that `mut` bindings cannot be exported. This is a WASM restriction.

Types have a simple syntax:

- `i32`, `i64`, `f32`, `f64` - native WASM types.
- `[i32]i32` - function from one `i32` to another `i32` value.
- `[i32 i32] f32 f32` - function from two `i32` values, to two `f32` values.

In this example, we define and implement a simple identity function:

```rust
def identity [i32] i32;
fun identity n = n;
```

The return value(s) can be declared within parenthesis in order to declare more complicated types:

```rust
# function from two functions, both taking one `i32` and returning one `i32`, to a single `i32` value
def complex-function [ [i32](i32) [i32](i32) ] (i32)
```

This completes the Wasmin syntax! I hope you will agree it really is a minimalistic syntax for WASM
(get it? Was-min!).

## Examples

### Counter

```rust
mut count = 0;

def increment [] i32;
pub fun increment = (set count = add count 1; count)

def decrement [] i32;
pub fun decrement = (set count = sub count 1; count)
```
