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
- [x] multi-value expressions.
- [x] type declarations.
- [ ] generic type declarations.
- [x] comments.
- [x] let assignments.
- [x] mut assignments.
- [x] set assignments.
- [ ] math operators.
- [x] function calls.
- [x] function implementations.
- [ ] generic functions.
- [ ] function pointers.
- [x] if/else blocks.
- [x] loops.
- [ ] lambdas.
- [x] global constants.
- [ ] import from other Wasmin files with `use`.
- [x] import external declarations with `ext`.
- [x] export functions and constants.
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

### Expressions

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
- `(function arg1 arg2)` (function call - does not require `;` is within parens).
- `function(arg1, arg2);` (same as previous examples, multi-value arguments style).
- `arg1, arg2, function;` (same as previous examples, concatenative style).
- `(expr1; expr2; expr3)` (group of expressions, each one is evaluated in order).
- `if cond_expr; expr1; expr2;` (if `cond_expr` evaluates to non-zero, evaluate `expr1`, else evaluate `expr2`).
- `if (cond_exp) (expr1) (expr2)` (same as previous but using parenthesis instead of `;` to separate expressions).
- `if cond_exp; then expr1; else expr2;` (same as previous but using `then` and `else` optional keywords for
  readability).
- `(if cond_expr; expr1)` (the `else` expression is optional if `expr1` evaluates to `()`).

For example, these are all expressions:

- `0` (the constant `0`, of type `i64`, or 64-bit integer).
- `(0)` (same as previous).
- `add 1 2` (calls the native WASM `i32.add` function with arguments `1` and `2`, which have type `i32`).
- `(let n = 1; add n 3)` (assigns `1` to the variable `n`, then calls `add` with `n` and `3` as arguments).
- `1, add 2` (same as `add 1 2`, using concatenative style - read as `take 1, add 2 to it`).
- `1, 2, add` (same as previous example).

The last examples show that the concatenative style allows certain expressions to be written in a more natural way. For
example, it allows using a familiar "infix operator" syntax.

> Wasmin source code must always be encoded using UTF-8.

### Comments

Comments start with the `#` character. Multi-line comments require the `#{ ... multi-line comment ... }` form:

```bash
# single line comment

#{
Multi-line
Comment
}
```

### Demarking expressions

Top-level expressions within parenthesis end when the closing parens is reached, so no `;` is required.

```rust
# ok
let x = (add 2 3)

# wrong!
let x = (add 2) 3;
```

The last example above would try to assign the result of `add 2` to `x` (which won't work because `add` takes two
arguments), then the expression `3` appears outside of the previous `let` expression, in an illegal location
(only `let`, `mut`, `set`, and as we'll see below, `def` and `fun` can appear as top-level elements).

These, however, would be fine:

```rust
# ok!
let x = add 2 3;
let y = add (2) (3);
let z = (add 2 3)
let w = add(2, 3);
```

Basically, if an expression does not start with `(`, it ends when a `;` is found.

### Multi-value expressions

Multi-value expressions are separated by `,`.

In the example above, `add (2, 3)` actually creates an expression whose second term evalutes to two values, `2` and `3`,
which then are passed as arguments to the first term, the `add`
function, and hence is equivalent to just `(add 2 3)` (but _coincidentally_ looks like a C-like function call).

> `let` expressions can be multi-valued as in `let x, y = 2, 3;`.
> A `let` will consume one value per identifier.
> Wasmin does not allow a single identifier to have more than one value, hence the number of elements must be the
> same on both sides.

### Types

The type of a variable or function can be declared explicitly with a `def` statement. To implement a function, the `fun`
keyword is used.

`def`s are optional for `let` and `mut` bindings (it is always possible to infer their types), but mandatory for `fun`
unless the function has type `[]()` (no args, no return value).

To make a `fun` or `let` visible outside the module (i.e. add them to the WASM module's exports), declare them
with `pub`. For example, `pub let PI = 3.1415;`.

> Notice that `mut` bindings cannot be exported. This is a WASM restriction.

Types have a simple syntax:

- `i32`, `i64`, `f32`, `f64` - native WASM types.
- `[i32]i32` - function from one `i32` to another `i32` value.
- `[i32 i32] f32 f32` - function from two `i32` values, to two `f32` values.
- `[ [i32]f32 ]( [i32](f32) )` - function that takes a function from `i32` to `f32` and returns
  another function from `i32` to `f32` (notice how the return type can appear between parens). 

### Functions

In this example, we define and implement a simple identity function:

```rust
def identity [i32] i32;
fun identity n = n;
```

To be able to use functions provided by the host environment (so you can actually print something, for example)
, you can use `ext` (external module):

```rust
ext console {
    log [i32];
    log [f32];
    warn [i32];
    warn [f32];
}
```

> Notice that, for obvious reasons, you cannot implement values or functions in `ext`, only define their types,
> hence the only allowed elements inside an `ext` block are `def`s, and as the `def` keyword would be redundant, it is
> omitted entirely.

To call functions defined in an `ext` module, employ the familiar `module_name.fun_name` syntax:

```rust
ext console { # { omitted definitions } }

fun _start = console.log 10;
```

### Strings

String literals can be declared as in most other languages:

```rust
# Explicit type declaration shown for illustration purposes
def name str;
let name = "John";
```

### Organizing code

You can split up Wasmin programs in several files. To do that, just import other files as shown below.

> TODO implement `use`.

`factorial.wasmin`:

```rust
def factorial [i32]i64;
pub fun factorial n = if n, le_s 2;
    then n;
    else n, mul (n, sub 1, factorial);
```

`main.wasmin`:

```rust
# the file extension is always implied to be .wasmin,
# so this will import all `pub` definitions from "./factorial.wasmin".
use * from "./factorial";

def _start []i64;
fun _start = factorial 5;
```

`use` statements may also list which definitions to import:

```rust
use { factorial } from "./factorial";

def _start []i64;
fun _start = factorial 5;
```

If another file defines an `ext` module, then the `ext` module can be used as any other definition in that file:

```rust
# assume that "ext console" was defined inside "./factorial.wasmin"
use {
    factorial
    console
} from "./factorial";

fun _start = console.log (factorial 5);
```

### Macros

Macros allow Wasmin programs to manipulate the AST (Abstract syntax tree) of a given expressions at compile-time.

To invoke a macro named `foo`, prefix it with `@`, as in `@foo 1 2 3`.

For example, suppose we really like infix notation for maths, so we want to add that to Wasmin, which would allow us
to do this:

```rust
let x = @infix 2 add 3;
```

Here's a very simple implementation that would work for this example:

```rust
macro infix expr = (
	if len expr, eq 3 then
		get expr 0, get expr 2, get expr 1, group
	else
		raise "infix expects exactly 3 expressions"
)
```

`expr` is an array of symbols or other expressions. Hence, `get expr 0` returns the first item, which in the
case of the invocation `@infix 2 add 3` would be `2`. When expanded, this invocation would thus become:

```rust
let x = 2, 3, add;
```

This is a valid Wasmin expression, so it would compile as expected.

This completes the description of the Wasmin syntax (a minimalistic syntax for WASM, hence the name ;))!

## Memory

To write most interesting programs, you'll need to allocate memory to represent structures more complex than just numbers and literal Strings!

Wasmin exposes the WASM `memory` instruction, but leaves it up to libraries to use that for the management of `struct`s and other more complex objects.

As an example, one could create a vector of `i32` as follows:

```rust
let min_size = 10   # 10 pages or ~64KiB
let max_size = 100  # 100 pages or ~64MiB

# Memory must be declared in the top-level.
# Current version of WASM only allows one memory per module!
# This is our heap!
let mem = memory min_size max_size;

def create_vec [i32 i32] i32;
fun create_vec offset len = (
	# push multiples of 2 into the vec
	# initial value of i is the offset in memory
	mut i = offset;
	mut v = 0;
	let limit = add offset len;
	loop (
		# the store instruction is used to store a value in mem,
		# it takes the memory and an offset followed by a value
		if i, gt limit then break
		else store mem i (let a = v; set v = add 2; a); # ==> 0, 2, 4 ...
		# we add 4 because an i32 has 4 bytes
		set i = i, add 4
	)
	# return a pointer to the vec
	offset
)

ext console { log [i32]; }

# Using the vector
fun _start = (
	let len, offset, limit = 10, 16, mul len 4;
	mut my_vec = create_vec offset len;
    loop (
		if my_vec, gt limit then break
		else console.log (load mem my_vec);
		# my_vec is a pointer, so we increment it to go to the next elemnt
		set my_vec = my_vec, add 4
	)
)
```

## Examples

### Counter

```rust
mut count = 0;

def increment [] i32;
pub fun increment = (set count = count, add 1; count)

def decrement [] i32;
pub fun decrement = (set count = count, sub 1; count)
```

## Grammar

```
expr          = naked_expr | group_expr
simple_expr   = identifier | number
naked_expr    = simple_expr expr* ';'
group_expr    = '(' expr* ')' | '[' expr* ']' | '{' expr* '}'
multi_expr    = expr ',' expr
```
