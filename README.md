# Nekomata

Trying to make a small [non-deterministic](https://en.wikipedia.org/wiki/Nondeterministic_programming) [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) [golfing](https://en.wikipedia.org/wiki/Code_golf) language.

The implementation of non-determinism is based on the paper [*KiCS2: A New Compiler from Curry to Haskell*](https://www.informatik.uni-kiel.de/~mh/papers/WFLP11_KiCS2.pdf).

The language is currently in an early stage of development. It has very few built-ins but lots of bugs. The custom code page is incomplete. The only supported numeric type is integer. I'm not even sure if it's Turing-complete yet (likely not).

I'll start writing a documentation once it has enough features to write a useful program. For now, you can read the code for [built-in functions](src/Nekomata/Builtin.hs) and [built-in particles](src/Nekomata/Particle.hs), which includes help messages for each built-in. There are also some [examples in tests](test/Eval.hs).

## Name

[***Nekomata***](https://en.wikipedia.org/wiki/Nekomata) (猫又) are a kind of cat monster in Japanese folklore that have forked tails. I think it's a good name for a con**cat**enative language that chooses between forking paths of computation.

## Building and running

Nekomata is written in [Haskell](https://www.haskell.org/). You need [Cabal](https://www.haskell.org/cabal/) and [GHC](https://www.haskell.org/ghc/) to build it. The easiest way to install these tools is through [GHCup](https://www.haskell.org/ghcup/).

You need to use GHC 9.2.1 or later to build Nekomata, because older versions of GHC don't support the Unicode character [`𐞥` (U+107A5, MODIFIER LETTER SMALL Q)](https://util.unicode.org/UnicodeJsps/character.jsp?a=107A5). This character was added to the Unicode standard in version 14.0.0, which was released in 2021.

Once you have Cabal and GHC installed, you can build Nekomata by running the following command in the project's root directory:

```bash
cabal build
```

To run the Nekomata REPL, use:

```bash
cabal run Nekomata -- -r
```

To run a Nekomata program from a file with a given input string, use:

```bash
cabal run Nekomata -- -f <path to program> -i <input string>
```

To see a list of all available options, use:

```bash
cabal run Nekomata -- -h
```
