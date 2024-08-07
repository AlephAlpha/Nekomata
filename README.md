# Nekomata

**Nekomata** is an experimental [non-deterministic](https://en.wikipedia.org/wiki/Nondeterministic_programming) [concatenative](https://en.wikipedia.org/wiki/Concatenative_programming_language) [golfing](https://en.wikipedia.org/wiki/Code_golf) language.

The implementation of non-determinism is based on the paper [*KiCS2: A New Compiler from Curry to Haskell*](https://www.informatik.uni-kiel.de/~mh/papers/WFLP11_KiCS2.pdf).

The language is still in an early stage of development. The custom code page is incomplete. The semantics of some built-ins are not clear. The syntax is still subject to change.

## Name

[***Nekomata***](https://en.wikipedia.org/wiki/Nekomata) (猫又) are a kind of cat monster in Japanese folklore that have two tails. Just consider a non-deterministic [cons list](https://en.wikipedia.org/wiki/Cons) where the head is deterministic but the tail has two possible values. Doesn't it look like a nekomata?

This name is also inspired by [Cat](https://concatenative.org/wiki/view/Cat) and [Kitten](https://kittenlang.org/). Both are con**cat**enative languages.

## Documentation

Like the language itself, the documentation is also incomplete. You can find it in the [doc](doc) folder.

There are also some example programs in [`test/Eval.hs`](test/Eval.hs). These examples are based on challenges from [Code Golf StackExchange](https://codegolf.stackexchange.com/). But there aren't any explanations.

You can also [search for `Nekomata` on Code Golf StackExchange](https://codegolf.stackexchange.com/search?q=Nekomata) to find some examples. Most of them come with explanations.

## Influences

Nekomata is influenced by the following languages:

- [**Curry**](https://curry.pages.ps.informatik.uni-kiel.de/curry-lang.org/). Curry is the first non-deterministic programming language I've learned. Nekomata is lazy, purely functional, and non-deterministic like Curry. The implementation of non-determinism is also inspired by Curry's KiCS2 compiler.
- [**Brachylog**](https://github.com/JCumin/Brachylog). This is a declarative logic golfing language based on Prolog. I think it's the first golfing language that uses non-determinism. Nekomata's syntax and choice of built-ins are heavily influenced by Brachylog. For example, Nekomata uses superscript letters for particles (called "Metapredicates" in Brachylog), though particles in Nekomata come before the arguments instead of after them.
- [**Vyxal**](https://github.com/Vyxal/Vyxal), [**Jelly**](https://github.com/DennisMitchell/jellylanguage), [**05AB1E**](https://github.com/Adriandmen/05AB1E), [**Husk**](https://github.com/barbuz/Husk), [**Thunno 2**](https://github.com/Thunno/Thunno2) and other golfing languages. They influence Nekomata's choice of built-ins. When I don't know what built-in to add to Nekomata, I usually take inspiration from existing answers on [Code Golf StackExchange](https://codegolf.stackexchange.com/) in these languages.
- [**Joy**](https://hypercubed.github.io/joy/joy.html), [**Kitten**](https://kittenlang.org/), [**Factor**](https://factorcode.org/) and other concatenative languages. They influence Nekomata's syntax and semantics.
- [**Julia**](https://julialang.org/). In Julia's REPL, [some Unicode characters](https://docs.julialang.org/en/v1/manual/unicode-input/) can be entered via tab completion by a long name starting with `\`. Nekomata borrows this idea.

## Online interpreter

Nekomata is now on [Attempt This Online!](https://ato.pxeger.com/run?1=m70iLzU7PzexJHHBgqWlJWm6FguWFCclF8M4UBoA)

The version on ATO is not always up-to-date. [You can check the version by passing the `-h` flag.](https://ato.pxeger.com/run?1=m70iLzU7PzexJHFZtJJuhlLsgqWlJWm6FguWFCclF8M4UBoA)
