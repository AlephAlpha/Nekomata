# Nekomata

Trying to make a small [non-deterministic](https://en.wikipedia.org/wiki/Nondeterministic_programming) [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) [golfing](https://en.wikipedia.org/wiki/Code_golf) language.

The implementation of non-determinism is based on the paper [*KiCS2: A New Compiler from Curry to Haskell*](https://www.informatik.uni-kiel.de/~mh/papers/WFLP11_KiCS2.pdf).

The language is currently in an early stage of development. It has very few built-ins but lots of bugs. The custom code page is incomplete. I'm not even sure if it's Turing-complete yet (likely not).

## Name

[***Nekomata***](https://en.wikipedia.org/wiki/Nekomata) (猫又) are a kind of cat monster in Japanese folklore that have forked tails. I think it's a good name for a con**cat**enative language that chooses between forking paths of computation.

## Documentation

Like the language itself, the documentation is also incomplete. You can find it in the [doc](doc) folder.

There are also some example programs in [`test/Eval.hs`](test/Eval.hs). These examples are based on challenges from [Code Golf StackExchange](https://codegolf.stackexchange.com/). But there aren't any explanations.

## Influences

Nekomata is influenced by the following languages:

- [**Curry**](https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/). Curry is the first non-deterministic programming language I've heard of. Nekomata is lazy, purely functional, and non-deterministic like Curry. The implementation of non-determinism is also inspired by Curry's KiCS2 compiler.
- [**Brachylog**](https://github.com/JCumin/Brachylog). This is a declarative logic golfing language based on Prolog. I think it's the first golfing language that uses non-determinism. Nekomata's syntax and choice of built-ins are heavily influenced by Brachylog. For example, Nekomata uses superscript letters for particles (called "Metapredicates" in Brachylog), though particles in Nekomata come before the arguments instead of after them.
- [**Vyxal**](https://github.com/Vyxal/Vyxal), [**Jelly**](https://github.com/DennisMitchell/jellylanguage), [**05AB1E**](https://github.com/Adriandmen/05AB1E), [**Husk**](https://github.com/barbuz/Husk). These are all golfing languages that influence Nekomata's syntax and choice of built-ins. Vyxal and 05AB1E are both concatenative languages. When I don't know what built-in to add to Nekomata, I usually look at existing answers on [Code Golf StackExchange](https://codegolf.stackexchange.com/) in these languages.
