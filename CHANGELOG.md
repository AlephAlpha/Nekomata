# Revision history for Nekomata

## 0.2.1.0 -- Unreleased

* Fixed a bug in parsing empty inputs.
* Fixed a bug in `\toBase`.

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
