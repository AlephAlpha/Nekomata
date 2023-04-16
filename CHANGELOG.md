# Revision history for Nekomata

## 0.3.0.0 -- Unreleased

* The language is still incomplete. Everything, especially the code page, is subject to change.
* New built-in functions: `\factorial`, `\gcd`, `\inToChar`, `\lcm`, `\mean`, `\primePi`, `\rotate`, `\setPart`, `\transpose`, `\unpair`, `\unsingleton`.
* New built-in particle: `\mapFirst`.
* `\map` now supports functions with no argument.
* The interpreter can now print the version number using the `-v` flag.

### Breaking changes

* The code page is updated. New characters are added, and the values of some characters are changed.
* The argument order of `\pow` is reversed. Now the exponent is the first argument, and the base is the second.
* `\bytes` is renamed to `\charToInt`.
* `\length` and `\enumerate` now fail if the argument is not a list or a string.
* The arity of `\noPop` is changed to `(m -> n) -> (m -> m + n)`.

## 0.2.1.0 -- 2023-03-26

* Fixed some bugs.

## 0.2.0.0 -- 2023-03-18

* The language is still incomplete. Everything, especially the code page, is subject to change.
* The dependency on `base` becomes `base >=4.13 && <5`. Now you can build Nekomata with GHC 8.8.4 or newer.
* The interpreter can now take multiple inputs separated by newlines using the `-m` flag. This is useful for testing.
* New built-in functions: `\cons0`, `\delta`, `\divInt`, `\enumerate`, `\free`, `\split`, `\toBase`.
* Fixed some bugs in result printing and documentation generation.

### Breaking changes

* The code page is completely redesigned. Now it only contains characters supported by the [Liberation Mono font](https://en.wikipedia.org/wiki/Liberation_fonts).
* The short name for most of the built-ins are changed.
* The numeric type is changed from `Integer` to `Rational`, and the semantics of some built-ins are changed accordingly.
* `\div` is split into `\div` and `\divInt`.
* `\toBase` is renamed to `\toBaseRev`, and a new `\toBase` is added.
* `\positive` is renamed to `\isPositive`.

## 0.1.1.0 -- 2023-03-11

* Fixed some errors in the documentation.
* New built-in functions: `\allEqual`, `\init`, `\isPrime`, `\pow`, `\prime`, `\snoc`, `\unsnoc`.

## 0.1.0.0 -- 2023-02-26

* First version. Released on an unsuspecting world.
* The language is still incomplete. Everything is subject to change.
* I release it now so that I can start posting answers on Code Golf StackExchange.
