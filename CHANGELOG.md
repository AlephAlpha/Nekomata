# Revision history for Nekomata

## 0.8.0.0 -- 2024-09-01

* New builtin functions: `\lastValue`, `\shortestValue`, `\unmul`, `\unsnoc0`.
* `\orApply`, `\iterate`, `\nTimes`, `\while`, `\lengthWhile`, `\fixedPoint` now support functions with arity `m -> n` where `m >= n`. When `m > n`, the top `m - n` values on the stack are "quoted" as a new function that pushes these values to the stack, and this new function is composed with the original function before applying it.
* A new mode `last` is added. It outputs the last possible result. You can switch to this mode by the `-t` flag in the command line or the `\Mode last` command in the REPL.
* `\lengthIs` is now automatically vectorized.
* Now input can have leading and trailing spaces. The interpreter will trim them when parsing.
* Fixed a bug in the REPL. Now the REPL can correctly handle strings with whitespaces.
* Now Nekomata can be built with GHC 9.10.2 and 9.12.2, thanks to an update of the `haskeline` package.

### Breaking changes

* `\oneValue` is renamed to `\firstValue`.
* The built-in function `\normalForm` was removed in version 0.7.0.0. Its short name `¤` is now reused for the new function `\lastValue`.

## 0.7.0.0 -- 2024-08-10

* Added examples to the documentation of built-in functions and particles.
* Fixed some errors in the documentation.
* Fixed a bug in `\binomial`. Now it returns `0` when the second argument is negative.
* Fixed a bug in `\factor`. In earlier versions, it may return incorrect results.
* New particles: `\concatMap`, `\fixedPoint`, `\onAny`, `\unconcatMap`.

### Breaking changes

* Some modules in the library are no longer exposed.
* The built-in function `\normalForm` is removed, because it is never used, and its semantics are not well-defined.
* The result of `\divisors` is now sorted.
* `\intersect` and `\union` are now multiset intersection and union.
* `\bifurcate` now works on numbers. If the input is a number, it is converted to the range from 0 to the number - 1.

## 0.6.1.0 -- 2024-06-07

* Now the REPL saves the history to a file named `.history` in the current directory.
* New built-in functions: `\isLong`, `\sumEach`.

### Breaking changes

* `\charToInt` now leaves the argument unchanged if it is already an integer. It still fails if the argument is a non-integral number.
* `\intToChar` now leaves the argument unchanged if it is already a character.

## 0.6.0.0 -- 2024-02-25

* Updated some dependencies. Now Nekomata can be built with GHC 9.8.1.
* Fixed a bug in printing truncated results.
* Now characters are internally represented as `Word8`s instead of `Char`s.
* Added a REPL command `\Arity` to print the arity of a function.
* New built-in functions: `\elem`, `\filterBy`, `\minValue`, `\minMax`, `\maxValue`, `\ordering`, `\pad`, `\slices`.

### Breaking changes

* `\apply2` is renamed to `\onBoth`.
* `\interval` can now take two chars as arguments.
* `\intersect`, `\union` and `\setMinus` now convert numbers and characters to singleton lists.
* Fixed a bug in comparing characters. Now characters are compared by their code points in Nekomata's custom code page instead of Unicode.

## 0.5.1.0 -- 2023-09-27

* New built-in functions: `\bifurcate`, `\divMod`, `\flatten`, `\histogram`, `\isBig`, `\isSmall`, `\over`, `\popCount`, `\rle`, `\unrle`.
* New particle: `\zipWithTrunc`.

### Breaking changes

* `\nonzero` is renamed to `\isNonzero`.
* `\nonempty` is renamed to `\isNonempty`.

## 0.5.0.0 -- 2023-09-02

* The interpreter can now limit the number of results to show using the `-l` option.
* New built-in functions: `\bitAnd`, `\bitOr`, `\bitXor`, `\fromBinary`, `\gt`, `\lt`.

### Breaking changes

* The dependency on `base` becomes `base >=4.15 && <5`. Now GHC 9.0.1 or newer is required to build Nekomata.
* The string type is removed, and a new character type is added. Now strings are represented as lists of characters. Many built-in functions are changed accordingly.
* `\join` and `concat` no longer convert their arguments to strings. Now they convert numbers and characters to singleton lists.
* Many numeric functions now convert chars to their code points before operating on them.
* `\range0` now generates a range from 0 to `ceil(n)-1` instead of `floor(n)-1`.
* `\join`, `\concat`, `\cons` and `\snoc` are now lazier. Joining a non-failing list with a failing list no longer fails immediately. Instead, it only fails when the failing part is actually used. `\concat` can now concatenate an infinite list of lists.
* `\uninterleave` is now lazier. It can now uninterleave an infinite list.

## 0.4.1.0 -- 2023-07-22

* New built-in functions: `\fromDigits`, `\interval`, `\read`, `\show`, `\tuple`.
* New particle: `\fold1`.

## 0.4.0.0 -- 2023-07-10

* The language is still incomplete. The code page isn't filled yet. Everything is subject to change.
* New built-in functions: `\absDiff`, `\count`, `\digits`, `\div2`, `\divisors`, `\interleave`, `\isNonnegative`, `\isUnique`, `\mod2`, `\mul2`, `\powOf2`.
* New particle: `\firstInt`.

### Breaking changes

* Some list functions are now overloaded to work on numbers. When they are used on numbers, they are applied to the range from 0 to the number - 1. These functions are: `\anyOf`, `\uninterleave`, `\extract`, `\permutation`, `\prefix`, `\reverse`, `\rotate`, `\setPartition`, `\split`, `\subsequence`, `\subset`, `\suffix`, `\unconcat`.
* `\convolve` now supports arbitrary-dimensional nested lists. Numbers are treated as 0-dimensional nested lists.
* `\mapFirst` is renamed to `\mapWith`.
* `\deinterleave` is renamed to `\uninterleave`.

## 0.3.5.0 -- 2023-06-12

* New built-in functions: `\deinterleave`, `\longest`, `\maximumBy`, `\minimumBy`, `\orNeg`, `\shortest`, `\unitVec2D`.
* New particle: `\lengthWhile`.

### Breaking changes

* `\toBase2Rev` is renamed to `\binary`.

## 0.3.4.0 -- 2023-06-07

* New built-in functions: `\andThen`, `\chunks`, `\convolve`, `\extract`, `\half`, `\isZero`, `\toBase2Rev`.
* New particle: `\filter`.

### Breaking changes

* The code page is updated. The character `∞` is replaced with `½`, and the character `∂` is replaced with `¿`.

## 0.3.3.0 -- 2023-05-27

* New built-in functions: `\factor`, `\index`, `\intersect`, `\rot3`, `\tally`, `\union`.
* New particle: `\dupDip2`.

## 0.3.2.0 -- 2023-05-15

* New built-in functions: `\ceil`, `\denominator`, `\floor`, `\numerator`, `\setMinus`, `\sqrt`.
* Fixed a bug in `\pow`.

## 0.3.1.0 -- 2023-05-03

* New built-in functions: `\intPartition`, `\octet`, `\recip`, `\replicate`, `\uniqueValue`.

### Breaking changes

* To prevent the version number from growing too fast, small breaking changes like code page updates and function renames are no longer considered major version changes.
* The code page is updated. New characters are added, and the values of some characters are changed.
* `\setPart` is renamed to `\setPartition`.

## 0.3.0.0 -- 2023-04-16

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
