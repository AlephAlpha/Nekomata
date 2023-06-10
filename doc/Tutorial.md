# Tutorial

Nekomata is still in an early stage of development. The syntax and semantics of the language may change in the future. This tutorial is also incomplete.

For installing and using the interpreter, please see [Getting Started](GettingStarted.md).

## The Stack

Nekomata is a concatenative language. "Concatenative" is just a fancy word for "stack-based". In a concatenative language, the program is a sequence of instructions that operate on a stack.

For example, here is the "Hello, World!" program in Nekomata:

```
"Hello, World!"
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FuuVPFJzcvJ1FMLzi3JSFJWWFCclF0PlYGoA)

This program pushes the string `"Hello, World!"` onto the stack. After that, the top of the stack is printed.

Now let's try to add two numbers:

```
1 2+
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FksMFYy0lxQnJRdDBWASAA)

There are three instructions in this program:

- The integer literal `1` pushes the integer `1` onto the stack.
- The integer literal `2` pushes the integer `2` onto the stack.
- The built-in `+` pops the top two elements from the stack, adds them, and pushes the result back onto the stack. Now the top of the stack is the integer `3`.

*(TODO: I'm still not sure what I should call the things on the stack. I'm using "elements", "items", and "values" interchangeably. But "values" may also refer to the different results of a non-deterministic computation. This is confusing.)*

After that, the top of the stack is printed.

Some programs may require input from the user. Inputs are also passed as command-line arguments using the `-i` option, and separated by spaces. For example:

```bash
Nekomata -c '+' -i '1 2'
```

The program here is simply the built-in `+` function. The inputs are `1` and `2`, which are passed as command-line arguments. Before the program is executed, the inputs are cycled and pushed onto the stack. In this case, the stack will become the infinite sequence `1 2 1 2 1 2 ...`. After execution, the stack becomes `3 1 2 1 2 1 2 ...`. The top of the stack is printed, which is `3`.

On ATO, you can input the code in the "Code" box, and the inputs in the "Input" box.

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6Fgu1lxQnJRdDeQsWGyoYQZgA)

## Data Types

Nekomata has three data types: numbers, strings, and lists.

### Numbers

Numbers in Nekomata are arbitrary-precision rational numbers.

Since the symbol `-` is already used for subtraction, when you want to push a negative number, you need to use the built-in function for negation, `_`. For example, when you want to push the number `-1` onto the stack, you need to write `1_`.

Similarly, when you want to push a fraction, the symbol `/` is already used for division. But you can write `\` instead of `/` to push a fraction. For example, when you want to push the number `-3/4`, you need to write `3\4_`. Of course, you can also write `3_4/` to push the same number.

Numbers in lists are written as normal, e.g., `[-1, -2, -3/4]`. When taking input, numbers are also written as normal, e.g., `-1 -2 -3/4`.

When you want to push two or more integers onto the stack, you can separate them with spaces. For example, `1 2 3` pushes the integers `1`, `2`, and `3` onto the stack. `123` pushes the integer `123`.

### Strings

Strings are delimited by double quotes. For example, `"Hello, World!"` is a string.

When the output is a string, the quotes are not printed. For example, `"Hello, World!"` is printed as `Hello, World!`. However, if the output is a list, the quotes are printed. For example, `["Hello, World!"]` is printed as `["Hello, World!"]`.

### Lists

Lists may contain any data types. For example, `[1, "Hello, World!", [2, 3]]` is a list that contains an integer, a string, and another list.

Strings and lists in Nekomata are lazy, so you can create infinite lists or strings. However, the interpreter does not support outputting infinite lists or strings, because it is impossible to find all possible values of a non-deterministic infinite list. Infinite lists or strings can only be used as intermediate results.

## Built-in Functions

A built-in function has a full name that starts with `\`, and a short name that is a single character. For example, the full name of the built-in function `+` is `\add`, and the short name is `+`. You can use either in a program. For example, `1 2+` and `1 2\add` are equivalent.

A function has an in arity and an out arity. The in arity is the number of elements that the function pops from the stack. The out arity is the number of elements that the function pushes onto the stack. For example, the arity of the built-in function `+` is `2 -> 1`, which means that it pops two elements and pushes one.

You can see the list of all built-in functions in [Builtins.md](Builtins.md).

## Blocks

A block is a sequence of instructions that are enclosed in curly braces. For example, `{1 2+}` is a block that pushes the integer `3` onto the stack.

Blocks also have an in arity and an out arity. For example, the arity of the block `{1 2+}` is `0 -> 1`, which means that it pops nothing and pushes one.

The right brace `}` at the end of a program can be omitted.

## Particles

A particle is a higher-order function that modifies a function. The function it modifies can be a built-in function, a block, or a function modified by another particle.

Like built-in functions, a particle has a full name that starts with `\`, and a short name that is a single character. The short name is always a superscript letter.

Let's look at this example:

```
[1,2,3]ᵐ{1+}
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FuuiDXWMdIxjH26dUG2oXbukOCm5GCoFUwIA)

Here the superscript letter `ᵐ` is the particle `\map`. It maps a function over a list. The function it maps is the block `{1+}`. The list it maps over is `[1,2,3]`. So the result is `[2,3,4]`.

The behavior of a particle may depend on the arity of the function it modifies.

You can see the list of all particles in [Builtins.md](Builtins.md).

## Vectorization

Many built-in functions in Nekomata are automatically vectorized. So the above example can be written as:

```
[1,2,3]1+
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FiujDXWMdIxjDbWXFCclF0NFYbIA)

For unary functions, vectorization simply means that the function is applied to each element of the list.

For binary functions, if one of the arguments is a list, and the other is not, the function is vectorized over the list. For example, both `[1,2,3]4+` and `4[1,2,3]+` return `[5,6,7]`.

When both arguments are lists, the situation is more complicated.

Some functions like `*` (`\mul`) will check if the two lists have the same length. If they do, the function is vectorized over the two lists. For example, `[1,2,3][4,5,6]*` returns `[4,10,18]`. If they don't, the function will fail. For example, `[1,2,3][4,5]*` fails.

Some functions like `+` (`\add`) do not check the lengths. They will pad the shorter list with zeros. For example, `[1,2,3][4,5]+` returns `[5,7,3]`.

Please refer to [Builtins.md](Builtins.md) for the vectorization behavior of each built-in function.

## Non-deterministic Computation

This is the hardest part of Nekomata. I'm still not sure how to explain it. In fact, I'm not even sure if I implemented it correctly.

Non-determinism means that a computation may have multiple results at the same time. It does not mean that the computation is random. In fact, all functions in Nekomata is pure, and the set of all possible results of a non-deterministic computation and their order is completely determined by the program.

Let's look at this built-in function, `?` (`\choice`). It pops two elements from the stack, and Non-deterministically pushes one of them back onto the stack. For example, `1 2?` may push `1` or `2` onto the stack. Now the top of the stack has two possible values: `1` and `2`.

By default, the interpreter will print all the possible results, separated by newlines.

Now look at a more complicated example:

```
1 2?:+
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FssMFYzsrbSXFCclF0OFYFIA)

There is a new built-in function, `:` (`\dup`). It pops an element from the stack, and pushes two copies of it onto the stack.

After executing `1 2?:`, the stack is either `1 1` or `2 2`. Now adding them gives `2` or `4`. So the result of the computation is either `2` or `4`.

But the following code is different:

```
1 2?1 2?+
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FisNFYzsQVh7SXFScjFUFCYLAA)

This looks similar to the previous code. It also pushes two elements onto the stack, both of which are either `1` or `2`. But the two elements are not related this time. So the result might be `1+1=2`, `1+2=3`, `2+1=3`, or `2+2=4`. When printing the results, `3` will appear twice, because there are two ways to get `3`.

## Modes

Nekomata is a non-deterministic programming language. A program can have multiple possible outputs. Nekomata supports four modes for outputting the results of a program:

### `all`

This is the default mode. It outputs all possible results of a program, separated by newlines.

In the REPL, you can switch to this mode by typing `\Mode all`.

### `first`

This mode outputs the first result of a program. If the program has no results, it outputs nothing.

When running a program from the command line, you can switch to this mode by passing the `-1` flag.

In the REPL, you can switch to this mode by typing `\Mode first`.s

### `count`

This mode outputs the number of possible results of a program.

When running a program from the command line, you can switch to this mode by passing the `-n` flag.

In the REPL, you can switch to this mode by typing `\Mode count`.

### `exists`

This mode outputs `True` if the program has at least one result, and `False` otherwise.

Since Nekomata does not have the concept of booleans or truthiness, this mode is required for [decision-problem](https://codegolf.stackexchange.com/questions/tagged/decision-problem) challenges.

When running a program from the command line, you can switch to this mode by passing the `-e` flag.

In the REPL, you can switch to this mode by typing `\Mode exists`.

## Example: Fibonacci Numbers (1)

```
1:ᶦ{$ᵉ+
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FqsNrR5uW1at8nBrp_aS4qTkYqg4TB4A)

Or written in a more readable way:

```
1 \dup \iterate { \swap \dupDip \add }
```

This program takes no input. The result is non-deterministic, and all the possible results are the Fibonacci numbers.

```
1:ᶦ{$ᵉ+
1       Push 1 onto the stack.
 :      Duplicate the top of the stack.
  ᶦ     Iterate the following function zero or more times non-deterministically.
   {    Start a block.
    $   Swap the top two elements of the stack.
     ᵉ  Apply the following function, and then push the original top of the stack onto the stack.
      + Add the top two elements of the stack.
```

After executing `1:`, the stack is `1 1`. Then the block `{$ᵉ+}` is applied non-deterministic many times. If it is applied once, the stack is `1 2`. If it is applied twice, the stack is `2 3`. If it is applied three times, the stack is `3 5`. And so on. So all possible results of the top of the stack are the Fibonacci numbers.

## Failure

A computation may also have no result at all. In this case, we say that the computation fails.

Nekomata does not have a Boolean type. Functions that return a Boolean value in other languages are represented by a function that possibly fails. For example, the built-in function `=` (`\eq`) returns the value itself if the top two elements of the stack are equal, and fails otherwise.

This is useful for choosing the correct paths in a non-deterministic computation. Let's look at this example:

```
1 2?2 3?=
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FisNFYzsjRSM7W2XFCclF0NFYbIA)

After executing `1 2?2 3?`, the top of the stack is either `2` or `3`, and the element below it is either `1` or `2`. Now the only way for them to be equal is choosing `2` for both. All the other paths fail. So the result `2`.

## Example: Fibonacci Numbers (2)

```
ʷ{←Pᶜ←
```

[*Attempt This Online!*](https://ato.pxeger.com/run?1=m70iLzU7PzexJHFZtJJunlLsgqWlJWm6FmtPba9-1DYh4OG2OUBqSXFScjFUZsFCYwgDAA)

Or written in a more readable way:

```
\while { \decrement \positive \orApply \decrement }
```

This program should be run in the `count` mode (`-n` flag in command line or `\Mode count` in the REPL), which prints the number of possible results of a computation.

It takes an integer as input, and prints the Fibonacci number at that position.

```
ʷ{←Pᶜ←
ʷ       Repeat the following function until it fails.
 {      Start a block.
  ←     Decrement the top of the stack.
   P    Check if the top of the stack is positive. If it is, keep the element unchanged. If it is not, fail.
    ᶜ   Optionally apply the following function.
     ←  Decrement the top of the stack.
```

Let's take the input `3` as an example.

- The stack is initialized to `3 3 3 ...`. Since only the top is used, we will simply write it as `3`.
- Let's try to execute the block.
  - Decrement the top of the stack. The stack is now `2`.
  - `2` is positive, so the stack is unchanged.
  - Now we optionally decrement the top of the stack. The stack is now either `1` or `2`. In either case, the computation does not fail.
    - If we choose `1`. Let's try to execute the block again.
      - Decrement the top of the stack. The stack is now `0`.
      - `0` is not positive, so the computation fails.
      - Since the block failed, the result of is the value before executing the block, which is `1`.
    - If we choose `2`. Let's try to execute the block again.
      - Decrement the top of the stack. The stack is now `1`.
      - `1` is positive, so the stack is unchanged.
      - Now we optionally decrement the top of the stack. The stack is now either `0` or `1`. In either case, the computation does not fail.
        - If we choose `0`. Let's try to execute the block again.
          - Decrement the top of the stack. The stack is now `-1`.
          - `-1` is not positive, so the computation fails.
          - Since the block failed, the result of is the value before executing the block, which is `0`.
        - If we choose `1`. Let's try to execute the block again.
          - Decrement the top of the stack. The stack is now `0`.
          - `0` is not positive, so the computation fails.
          - Since the block failed, the result of is the value before executing the block, which is `1`.
      - There are two possible results in this path: `0` and `1`.
    - There are three possible results altogether: `1`, `0`, and `1`. In `count` mode, the interpreter will print `3`.
