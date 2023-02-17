# Nekomata

Trying to make a small [non-deterministic](https://en.wikipedia.org/wiki/Nondeterministic_programming) [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) [golfing](https://en.wikipedia.org/wiki/Code_golf) language.

The implementation of non-determinism is based on the paper [*KiCS2: A New Compiler from Curry to Haskell*](https://www.informatik.uni-kiel.de/~mh/papers/WFLP11_KiCS2.pdf).

The language is currently in a very early stage of development. It has very few built-ins, and it doesn't support any numeric types other than integers. The syntax is also likely to change in the future.

I'll start writing a documentation once it has enough features to write a useful program. For now, you can read the code for [built-in functions](src/Nekomata/Builtin.hs) and [built-in particles](src/Nekomata/Particle.hs), which includes help messages for each function.

## Name

[***Nekomata***](https://en.wikipedia.org/wiki/Nekomata) (猫又) are a kind of cat monster in Japanese folklore that have forked tails. I think it's a good name for a con**cat**enative language that chooses between forking paths of computation.

## Building and running

Nekomata is written in [Haskell](https://www.haskell.org/). You need [Cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) to build it. The easiest way to install these tools is through [GHCup](https://www.haskell.org/ghcup/).

Once you have Cabal and GHC installed, you can build Nekomata by running the following command in the project's root directory:

```bash
cabal build
```

To run a Nekomata program, you can use the following command:

```bash
cabal run Nekomata -- -f <path to program> -i <input string>
```

You can also take input from stdin by using the `-s` flag instead of `-i`, or read the program as command line arguments by using the `-c` flag instead of `-f`. You can also use the `-h` flag to see a list of all available options.

## Examples

### Hello world

```
"Hello, World!"
```

Currently, Nekomata doesn't support string compression, so you have to write the full string.

