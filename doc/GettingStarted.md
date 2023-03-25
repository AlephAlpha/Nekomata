# Getting Started

## Installation

Nekomata is written in [Haskell](https://www.haskell.org/). You need [Cabal](https://www.haskell.org/cabal/) (version 3.0 or newer) and [GHC](https://www.haskell.org/ghc/) (version 8.8.4 or newer) to build Nekomata.

The easiest way to install these tools is through [GHCup](https://www.haskell.org/ghcup/). The packages provided by your Linux distribution's package manager may be too old to build Nekomata.

Once you have Cabal and GHC installed, you can build Nekomata by running the following command in the project's root directory:

```bash
cabal build
```

The generated executable will be hiding somewhere in the `dist-newstyle` directory. You can install it to your system by running:

```bash
cabal install
```

You can also run Nekomata without installing it by running:

```bash
cabal run Nekomata
```

## Running Nekomata programs

You can run a Nekomata program by passing the code as a command-line argument:

```bash
Nekomata -c '"Hello, World!"'
```

Some programs may require input from the user. Inputs are also passed as command-line arguments, and separated by spaces. For example, the following program adds one and two:

```bash
Nekomata -c '+' -i '1 2'
```

Programs can also be read from a file. Nekomata supports reading codes either in UTF-8 or Nekomata's own encoding. You can pass the path to a UTF-8 file using the `-u` option, or a file with Nekomata's encoding using the `-f` option.

You can see a list of command-line options by running:

```bash
Nekomata -h
```

If you didn't install Nekomata, you can replace `Nekomata` with `cabal run Nekomata --` in the above commands.

## Using the REPL

Nekomata has a REPL (read-eval-print loop) that you can use to try out the language.

If you installed Nekomata, you can run the REPL by running:

```bash
Nekomata
```

If you didn't install Nekomata, you can run the REPL by running:

```bash
cabal run Nekomata
```

After entering the REPL, you will see a prompt that looks like this:

```
Nekomata REPL - type \H for help
>>>
```

Now you can type in some Nekomata code and press Enter to evaluate it. Let's try the "Hello, World!" program:

```
>>> "Hello, World!"
Hello, World!
```

You can type `\H` to see a list of commands that you can use in the REPL.

Builtin functions in Nekomata all have an ASCII-only full name and a single-character short name. For example, the function for adding two numbers is called `\add` and its short name is `+`. The REPL supports TAB completion that completes the full name to the short name. For example, if you type `\add ` and press TAB, it will be replaced with `+`. The space after the function name is required, because there may be other functions that start with the same characters.

Some programs may require input from the user. You can enter the input by typing `\Input` followed by the input. After taking the input, the stack will be cleared and initialized with the input.

For example, the following program adds one and two:

```
>>> \Input 1 2
>>> +
3
```

If you want to clear the stack without taking input, you can simply type `\Input` without any arguments.

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

When running a program from the command line, you can switch to this mode by passing the `-e` flag.

In the REPL, you can switch to this mode by typing `\Mode exists`.

## Online interpreter

Nekomata is now on [Attempt This Online!](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FguWFCclF8M4UBoA)
