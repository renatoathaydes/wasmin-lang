# Wasmin Programming Language

The Wasmin programming language was designed to have semantics very close to those of
[WebAssembly (WASM)](https://developer.mozilla.org/en-US/docs/WebAssembly), making it a perfect language for
compiling to WASM.

Wasmin is statically typed, non-garbage-collected
(but requires no memory management thanks to [linear types](http://home.pipeline.com/~hbaker1/ForthStack.html), which do
not generate garbage) and supports concatenative programming, s-expressions and procedural programming through a simple,
yet very flexible syntax.

## Wasmin Goals

- stay close to WASM for fast compilation and zero runtime dependencies.
- no memory management nor garbage collection (GC) by using a linear type system.
- mix the functional and concatenative (exposing the WASM stack) programming paradigms.
- simplest possible syntax that preserves readability.

## This is work in progress

Not yet designed features (may never be added):

- pattern matching.
- type checks.
- threads.
- [SIMD](https://medium.com/wasmer/webassembly-and-simd-13badb9bf1a8).
- [WASI interface types](https://hacks.mozilla.org/2019/08/webassembly-interface-types/).
- embed WAT code inside Wasmin.

## The language

Wasmin was designed to stay as close as possible to WASM bytecode while offering the readability
of a modern programming language.

The syntax is extremely simple, allowing the parser to be blazing fast - which is important for use cases where WASM
is used, like on the web and for short-lived processes running in the cloud.

> Wasmin source code must always be encoded using UTF-8.

### Immutable variable binding

An immutable value can be declared with `let`:

```rust
let x = 1;
```

An explicit type may be provided, but it's not needed as long as it can be inferred:

```rust
let x: i32 = 1;
```

The value of a `let` can be any expression.

### Mutable bindings

Mutable bindings can be introduced with `mut`.

```rust
mut x = 10;
```

To modify a `mut` binding, use `set`:

```rust
set x = 42;
```

### Functions

A function is declared similarly, but using `fun`, and can take arguments:

```rust
fun square a = mul a a;
```

As shown above, a function call has the form `function arg1 arg2...`, but it's also possible to use
concatenative style, as we'll see later.

Function types can be explicitly declared and have the form
`fun name arg...: [argType...](returnType...)`.

For example:

```rust
fun square a: [i32](i32) = mul a a;
```

As seen above, expressions end with a semicolon `;`.

### Multiple values and the WASM stack

As WASM, Wasmin is a stack-based language, and hence allows values to be pushed onto an implicit stack.

To explicitly push values onto the stack, use commas `,` to separate the values or expressions:

```rust
# assign 1 to a, 2 to b and 3 to c
let a, b, c = 1, 2, 3;
```

> Notice that a variable can only hold a single value. It would be a mistake to do something like
> `let a = 1, 2;` as `a` would take the value `1`, but the `2` would be a _stray_ value which is not allowed.

Anything between the commas can be any expression. If it's a function call, it can use elements previously pushed
onto the stack, which means that it's possible to use the concatenative style of programming:

```rust
fun square a = a, a, mul;
```

The body of the function above is identical, semantically, to the previous implementation, `mul a a`.

> Concatenative expressions may not take values from the stack unless they're pushed within the same expression. This
> allows Wasmin to remain type-safe.

A function call may take explicit arguments, but it can also take values from the stack if it expects more arguments
than provided. In the latter case, the necessary stack values are used as the first arguments to the function, and
any explicit arguments are used as the last arguments.

The `square` function seen previously can also be written as:

```rust
fun square a = a, mul a;
```

> The above notation looks like infix notation, so is typically used where infix notation is desired, like in
> mathematical operations.

Instead of using `,` to separate values, Wasmin also allows using parenthesis, which means that yet another way to write
the above expression is this:

```rust
fun square a = a (mul a);
```

Notice that by using parenthesis, Wasmin code can be written as s-expressions:

```rust
# s-expressions
fun square a = (mul a a);

fun cube a = (mul a (square a));

# concatenative style:
fun cube2 a = a, a, square, mul;
```

> The semicolon at the end is necessary in all cases because without it, Wasmin would expect more values until it found
> a terminating ';'.

A Wasmin expression ends with a `;` unless it is wrapped into curly braces.
Hence, the previous examples could also be written like this:

```rust
fun square a = { mul a a }

fun cube a = { mul a { square a } }

fun cube2 a = { a, a, square, mul }
```

### Types

Wasmin types are the same as WASM types (as of the MVP first WASM release):

* `i32` - 32-bit integer
* `i64` - 64-bit integer
* `f32` - 32-bit float
* `f64` - 64-bit float

> WASM has a few more types now, but Wasmin does not support them yet.

Function types are combinations of these types... for example, the type of a function taking two `i32` arguments and
returning one `i64` value is `[i32 i32](i64)`.

Functions can return multiple values. A function that just returns 2 `f32` values has type `[](f32 f32)`, for example.

> Notice that WASM does not have signed/unsigned numbers: weather a number is interpreted as being signed or not depends
> on the operations they're used with.

### If expressions

Wasmin if expressions have the form:

* `if condition then positive else negative` (both branches must have the same type).

Booleans, as with WASM, are of type `i32` and only `0` is considered `false`.

The `condition` expression must be of type `i32` (but used as boolean).

The `else` branch is mandatory, but can be an empty expression like `{}` or `()`.

Examples:

```rust
fun greater_than_10 n: [i32](i32) = if n, gt_s 10 then 1 else 0;

fun print_if_gt_10 n: [i32]() = if n, greater_than_10 then print n else {}
```

### Loops

The loop expression has the following form:

```rust
loop {
  <expressions>
}
```

For a loop to terminate, it must have a `break` expression within. A `break` expression may terminate with one or more values.
Every `break` within a `loop` must break with the same type, which is the type the `loop` expression itself inherits.

The following example loops `n` times, adding `n` to itself at each iteration and accumulating the result in `sq`,
then returning the values `n` and `sq`.

```rust
fun n_and_sq n: [i32](i32 i32) =
    mut sq = n,
    mut count = 0,
    loop {
        if count, gte n then break n, sq;
        set count = (count, add 1)
        set sq = (sq, add sq)
    };
```

### Comments

Comments start with the `#` character. Multi-line comments require the `#{ ... multi-line comment ... }` form, which
ends with `}` even on the same line:

```bash
# single line comment

#{
Multi-line
Comment
}

#{ comment } let this_is_code = #{ another comment } 1;
```

The number of braces used to start the comment block must be matched to end it, allowing nested comments:

```bash
#{{
  A comment.
  #{ A nested comment. }
  This is all one big comment which ends in the next line.
}}
```

### External imports

WASM allows importing definitions from the environment.

In Wasmin, you can declare such imports using `ext` (extension blocks):

```rust
ext console {
    log [i32]()
    warn [i32]()
}
```

> Notice that, for obvious reasons, you cannot implement values or functions in `ext`, only define their types.

To call functions defined in an `ext` module, such as `console` defined above, use the syntax `<module>.<function>`.

```rust
fun _start = console.log 10, console.warn 20;
```

### Importing other files

To import `pub` definitions from another file, use the `use` keyword.

> TODO implement `use`.

`factorial.wasmin`:

```rust
pub fun factorial n: [i32](i64) =
    if n, le_s 2 then n else n, mul factorial (n, sub 1);
```

`main.wasmin`:

```rust
# the file extension is always implied to be .wasmin,
# so this will import all `pub` definitions from "./factorial.wasmin".
use "./factorial";

def _start []i64;
fun _start = factorial 5;
```

`use` statements may also limit which definitions to import with `show`:

```rust
# only factorial is imported
use "./factorial" show factorial;
```

Or exclude definitions to import with `hide`:

```rust
# everything except factorial is imported
use "./factorial" hide factorial;
```

If another file defines an `ext` module, then the `ext` module can be used as any other definition in that file:

```rust
# assume that "ext console" was defined inside "./factorial.wasmin"
use "./factorial"
    show factorial console;

fun _start = console.log (factorial 5);
```

This completes the description of the Wasmin syntax (a minimalistic syntax for WASM, hence the name ;))!

## Memory

> TODO: this section is not well thought out yet, it's all speculation, currently.

To write most interesting programs, you'll need to allocate memory to represent structures more complex than just
numbers!

Wasmin exposes the WASM `memory` instruction, but leaves it up to libraries to interpret those bytes as complex objects.

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
mut value = 0;
# each element has 4 bytes, so the limit is len * 4
let limit = add offset (len, mul 4);
loop (
# the store instruction is used to store a value in mem,
# it takes the memory and an offset followed by a value
if i, gt limit then break; ;
store mem i ( let a = value; set value = add 2; a); # == > 0, 2, 4...
# we add 4 to the index because an i32 has 4 bytes
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
        if my_vec, gt limit then break;;
        console.log (load mem my_vec);
        # my_vec is a pointer, so we increment it to go to the next elemnt
        set my_vec = my_vec, add 4
    )
)
```

## Examples

### Counter

```rust
mod Counter {
  mut count = 0;

  pub fun increment: [](i32) =
    set count = (count, add 1)
    count;

  pub fun decrement: [](i32) = 
    set count = (count, sub 1)
    count;  
}
```
