# Built-ins Functions and Particles

Nekomata is still in an early stage. The full names, short names, and meanings of built-in functions and particles are subject to change.

## Functions

### `choice` (`?`, `2 -> 1`)

Choose between two values.

This function is non-deterministic.

### `fail` (`!`, `0 -> 1`)

Push a non-deterministic object with no values.

### `allValues` (`a`, `1 -> 1`)

Get a list of all possible values for a non-deterministic object.

### `oneValue` (`¡`, `1 -> 1`)

Get the first possible value from a non-deterministic object.

Fails if the object has no values.

### `countValues` (`n`, `1 -> 1`)

Count the number of values in a non-deterministic object.

### `uniqueValue` (`ũ`, `1 -> 1`)

Remove duplicate values from a non-deterministic object.

### `normalForm` (`¤`, `1 -> 1`)

Convert a non-deterministic object to the normal form.

I haven't given a formal definition for the normal form. This function basically lifts all the non-determinism in lists to the top level.

### `if` (`I`, `2 -> 1`)

Choose the first value that doesn't fail between two values.

### `andThen` (`¿`, `2 -> 1`)

Take two values, 

and return the first one if the second one doesn't fail.

### `drop` (`^`, `1 -> 0`)

Drop the top value of the stack.

### `dup` (`:`, `1 -> 2`)

Duplicate the top value of the stack.

### `swap` (`$`, `2 -> 2`)

Swap the top two values of the stack.

### `rot3` (`§`, `3 -> 3`)

Rotate the top three values of the stack.

### `eq` (`=`, `2 -> 1`)

Check if two values are equal.

If they are, push the first value, otherwise fail.

### `ne` (`≠`, `2 -> 1`)

Check if two values are not equal.

If they are not, push the first value, otherwise fail.

### `nonempty` (`N`, `1 -> 1`)

Check if a list is non-empty.

If it is, push the list itself, otherwise fail.

### `nonzero` (`Z`, `1 -> 1`)

Check if a number is non-zero.

If it is, push the number itself, otherwise fail.

This function is automatically vectorized.

### `isPositive` (`P`, `1 -> 1`)

Check if a number is positive.

If it is, push the number itself, otherwise fail.

This function is automatically vectorized.

### `isNonnegative` (`ň`, `1 -> 1`)

Check if a number is non-negative.

If it is, push the number itself, otherwise fail.

This function is automatically vectorized.

### `isZero` (`ž`, `1 -> 1`)

Check if a number is zero.

If it is, push the number itself, otherwise fail.

This function is automatically vectorized.

### `less` (`<`, `2 -> 1`)

Check if the first number is less than the second.

If it is, push the first number, otherwise fail.

This function is automatically vectorized.

### `lessEq` (`≤`, `2 -> 1`)

Check if the first number is less than or equal to the second.

If it is, push the first number, otherwise fail.

This function is automatically vectorized.

### `greater` (`>`, `2 -> 1`)

Check if the first number is greater than the second.

If it is, push the first number, otherwise fail.

This function is automatically vectorized.

### `greaterEq` (`≥`, `2 -> 1`)

Check if the first number is greater than or equal to the second.

If it is, push the first number, otherwise fail.

This function is automatically vectorized.

### `neg1` (`£`, `0 -> 1`)

The constant -1.

### `ten` (`¢`, `0 -> 1`)

The constant 10.

### `octet` (`¥`, `0 -> 1`)

The constant 256.

### `neg` (`_`, `1 -> 1`)

Negate a number.

This function is automatically vectorized.

### `abs` (`A`, `1 -> 1`)

Absolute value of a number.

This function is automatically vectorized.

### `increment` (`→`, `1 -> 1`)

Increment a number.

This function is automatically vectorized.

### `decrement` (`←`, `1 -> 1`)

Decrement a number.

This function is automatically vectorized.

### `logicalNot` (`¬`, `1 -> 1`)

Takes a number and returns 1 if it is 0, and 0 otherwise.

This function is automatically vectorized.

### `sign` (`±`, `1 -> 1`)

Returns -1 if the argument is negative, 0 if it is zero, and 1 if it is positive.

This function is automatically vectorized.

### `add` (`+`, `2 -> 1`)

Add two numbers.

This function is automatically vectorized with padding zeros.

### `sub` (`-`, `2 -> 1`)

Subtract two numbers.

This function is automatically vectorized with padding zeros.

### `absDiff` (`≈`, `2 -> 1`)

Absolute difference of two numbers.

This function is automatically vectorized with padding zeros.

### `mul` (`*`, `2 -> 1`)

Multiply two numbers.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `div` (`/`, `2 -> 1`)

Division of two numbers.

Fails when the divisor is zero.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `divInt` (`÷`, `2 -> 1`)

Integer division of two numbers. Result is rounded towards negative infinity.

Fails when the divisor is zero.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `mod` (`%`, `2 -> 1`)

Modulo two numbers.

Fails when the divisor is zero.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `divExact` (`¦`, `2 -> 1`)

Divide two numbers.

Fails when the divisor is zero or the result is not an integer.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `half` (`½`, `1 -> 1`)

Check if an integer is even, and divide it by 2.

Fails when the integer is odd.

This function is automatically vectorized.

### `pow` (`E`, `2 -> 1`)

Raise a number to a power.

Fails when the exponent is not an integer.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `recip` (`ŗ`, `1 -> 1`)

Reciprocal of a number.

Fails when the number is zero.

This function is automatically vectorized.

### `mul2` (`Ä`, `1 -> 1`)

Multiply a number by 2.

This function is automatically vectorized.

### `div2` (`ä`, `1 -> 1`)

Divide a number by 2.

This is different from `half` in that it may return a non-integer.

This function is automatically vectorized.

### `mod2` (`Ö`, `1 -> 1`)

Modulo a number by 2.

This function is automatically vectorized.

### `powOf2` (`Ë`, `1 -> 1`)

Raise 2 to a power.

Fails when the exponent is not an integer.

This function is automatically vectorized.

### `denominator` (`ḍ`, `1 -> 1`)

Get the denominator of a number.

This function is automatically vectorized.

### `numerator` (`ṇ`, `1 -> 1`)

Get the numerator of a number.

This function is automatically vectorized.

### `min` (`m`, `2 -> 1`)

Get the minimum of two numbers or two chars.

This function is automatically vectorized with padding.

### `max` (`M`, `2 -> 1`)

Get the maximum of two numbers or two chars.

This function is automatically vectorized with padding.

### `ceil` (`K`, `1 -> 1`)

Round a number up to the nearest integer.

This function is automatically vectorized.

### `floor` (`k`, `1 -> 1`)

Round a number down to the nearest integer.

This function is automatically vectorized.

### `range0` (`r`, `1 -> 1`)

Create a list of integers from 0 to n-1.

This function is automatically vectorized.

### `range1` (`R`, `1 -> 1`)

Create a list of integers from 1 to n.

This function is automatically vectorized.

### `interval` (`ï`, `2 -> 1`)

Create a list of integers from ceil(x) to floor(y).

This function is automatically vectorized and fails when the two lists are of different lengths.

### `natural` (`Ň`, `0 -> 1`)

Non-deterministically choose a natural number.

This function is non-deterministic.

### `integer` (`Ž`, `0 -> 1`)

Non-deterministically choose an integer.

This function is non-deterministic.

### `sum` (`∑`, `1 -> 1`)

Take the sum of a list of numbers.

The addition is automatically vectorized with padding zeros.

### `product` (`∏`, `1 -> 1`)

Take the product of a list of numbers.

The multiplication is automatically vectorized and fails when the two lists are of different lengths.

### `dot` (`∙`, `2 -> 1`)

Take the dot product of two lists of numbers.

The current implementation is simply a composition of mul and sum.

### `convolve` (`×`, `2 -> 1`)

Take the convolution of two lists of numbers.

If one of the arguments is a number, it simply multiplies the other argument by that number.

If the arguments are nested lists, it takes the multi-dimensional convolution.

### `mean` (`µ`, `1 -> 1`)

Take the mean of a list of numbers.

### `fromBase` (`b`, `2 -> 1`)

Convert a list of digits to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

This function is automatically vectorized over the base.

### `fromBaseRev` (`d`, `2 -> 1`)

Convert a list of digits in reverse order to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

This function is automatically vectorized over the base.

### `toBase` (`D`, `2 -> 1`)

Convert an integer to a list of digits.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

### `toBaseRev` (`B`, `2 -> 1`)

Convert an integer to a list of digits in reverse order.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

### `binary` (`Ƃ`, `1 -> 1`)

Convert an integer to a list of binary digits in reverse order.

This function is automatically vectorized.

### `digits` (`Ɗ`, `1 -> 1`)

Convert an integer to a list of decimal digits.

This function is automatically vectorized.

### `fromDigits` (`ɗ`, `1 -> 1`)

Convert a list of decimal digits to an integer.

### `cumsum` (`∫`, `1 -> 1`)

Take the cumulative sum of a list of numbers.

The addition is automatically vectorized with padding zeros.

### `delta` (`∆`, `1 -> 1`)

Take the difference between adjacent elements of a list of numbers.

The subtraction is automatically vectorized with padding zeros.

### `binomial` (`Ç`, `2 -> 1`)

Compute the binomial coefficient.

The second argument must be an integer.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `factorial` (`F`, `1 -> 1`)

Compute the factorial of an integer.

This function is automatically vectorized.

### `isPrime` (`Q`, `1 -> 1`)

Check if an integer is prime.

This function is automatically vectorized.

### `prime` (`Ƥ`, `0 -> 1`)

Non-deterministically choose a prime number.

This function is non-deterministic.

### `primePi` (`ƥ`, `1 -> 1`)

Compute the number of primes less than or equal to an integer.

This function is automatically vectorized.

### `factor` (`ƒ`, `1 -> 2`)

Factorize a rational number, and return a list of prime factors and a list of exponents.

Fails when the input is zero.

This function is automatically vectorized.

### `gcd` (`G`, `2 -> 1`)

Compute the greatest common divisor of two numbers.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `lcm` (`g`, `2 -> 1`)

Compute the least common multiple of two numbers.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `divisors` (`Ď`, `1 -> 1`)

Compute the list of divisors of an integer.

Fail when the input is zero.

This function is automatically vectorized.

### `intPartition` (`Ṗ`, `1 -> 1`)

Partition an integer into a list of integers, whose sum is the original integer.

This function is non-deterministic and automatically vectorized.

### `sqrt` (`√`, `1 -> 1`)

Compute the square root of a rational number.

Fails when the argument is not a perfect square.

This function is automatically vectorized.

### `unitVec2` (`į`, `0 -> 1`)

Choose one of [0, 1] and [1, 0] non-deterministically.

This function is non-deterministic.

### `orNeg` (`ŋ`, `1 -> 1`)

Optionally negate a number.

This function is non-deterministic and automatically vectorized.

When the input is a list, each element is optionally negated independently.

### `charToInt` (`e`, `1 -> 1`)

Convert a char to an integer according to Nekomata's custom encoding.

This function is automatically vectorized.

### `intToChar` (`H`, `1 -> 1`)

Convert an integer to a char according to Nekomata's custom encoding.

Fail when the integer is not in the range 0 to 255.This function is automatically vectorized.

### `read` (`Ĝ`, `1 -> 1`)

Parse a string (a list of chars) or a single char as a Nekomata value.

Fail when the string is not a valid Nekomata value.

### `show` (`ĝ`, `1 -> 1`)

Convert a Nekomata value to a string (a list of chars).

### `anyOf` (`~`, `1 -> 1`)

Choose an element from a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `emptyList` (`Ø`, `0 -> 1`)

Push an empty list.

### `singleton` (`U`, `1 -> 1`)

Create a list with a single element.

### `unsingleton` (`z`, `1 -> 1`)

Get the only element of a list with a single element.

Fails when the list is empty or has more than one element.

### `pair` (`Ð`, `2 -> 1`)

Create a list with two elements.

### `unpair` (`đ`, `1 -> 2`)

Get the two elements of a list with two elements.

Fails when the length of the list is not 2.

### `removeFail` (`‼`, `1 -> 1`)

Remove failed items from a list.

### `length` (`#`, `1 -> 1`)

Get the length of a list.

### `lengthIs` (`L`, `2 -> 1`)

Check if the length of a list is equal to a given integer.

If it is, push the list itself, otherwise fail.

### `nth` (`@`, `2 -> 1`)

Get the nth element of a list.

This function is automatically vectorized on the second argument.

### `head` (`h`, `1 -> 1`)

Get the first element of a list.

### `tail` (`t`, `1 -> 1`)

Remove the first element of a list.

### `cons` (`c`, `2 -> 1`)

Prepend an element to a list.

### `uncons` (`C`, `1 -> 2`)

Get the first element and the rest of a list.

### `last` (`l`, `1 -> 1`)

Get the last element of a list.

### `init` (`i`, `1 -> 1`)

Remove the last element of a list.

### `snoc` (`ɔ`, `2 -> 1`)

Append an element to a list.

### `unsnoc` (`Ɔ`, `1 -> 2`)

Get the last element and the rest of a list.

### `cons0` (`ç`, `1 -> 1`)

Prepend a zero to a list.

### `reverse` (`↔`, `1 -> 1`)

Reverse a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

### `prefix` (`p`, `1 -> 1`)

Get a prefix of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `suffix` (`s`, `1 -> 1`)

Get a suffix of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `take` (`T`, `2 -> 1`)

Get the first n elements of a list.

This function is automatically vectorized on the second argument.

### `subset` (`S`, `1 -> 1`)

Get a finite subset of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `subsequence` (`q`, `1 -> 1`)

Get a finite contiguous subsequence of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `join` (`,`, `2 -> 1`)

Concatenate two lists.

If one of the arguments is a number or a char, it is converted to a singleton list before concatenation.

### `split` (`;`, `1 -> 2`)

Split a list into two parts.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `replicate` (`ř`, `2 -> 1`)

Create a list with n copies of an element.

This function is automatically vectorized on the second argument.

### `minimum` (`ṁ`, `1 -> 1`)

Get the minimum of a list.

If there are multiple minimums, return the first one.

Fail when the list is empty.

The order used in this function is different from the one used in min and max. It can compare two arbitrary values, not just numbers or chars.

### `maximum` (`Ṁ`, `1 -> 1`)

Get the maximum of a list.

If there are multiple maximums, return the first one.

Fail when the list is empty.

The order used in this function is different from the one used in min and max. It can compare two arbitrary values, not just numbers or chars.

### `concat` (`j`, `1 -> 1`)

Concatenate a list of lists or a list.

If one item in the list is a number or a char, it is converted to a singleton list before concatenation.

### `unconcat` (`J`, `1 -> 1`)

Split a list into a list of lists.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `nub` (`u`, `1 -> 1`)

Remove duplicate elements from a list.

### `sort` (`o`, `1 -> 1`)

Sort a list.

### `permutation` (`↕`, `1 -> 1`)

Get a permutation of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `extract` (`ĕ`, `1 -> 2`)

Extract an element from a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

Returns the element and the rest of the list.

This function is non-deterministic.

### `allEqual` (`≡`, `1 -> 1`)

Check if all elements in a list are equal.

If it is, push the equal element, otherwise fail.

If the list is empty, this function fails.

### `isUnique` (`ů`, `1 -> 1`)

Check if all elements in a list are unique.

If it is, push the list itself, otherwise fail.

The empty list is considered unique.

### `free` (`f`, `2 -> 1`)

Check if a list is free of a given element.

This means that the list is not equal to the element, and recursively, every item of the list if free of that element.

If it is, push the list itself, otherwise fail.

### `enumerate` (`x`, `1 -> 2`)

Push a list of integers from 0 to the length of the argument minus 1 without popping the argument.

### `rotate` (`Ř`, `2 -> 1`)

Rotate a list by a given number of positions.

If the first argument is a number, it is converted to a range from 0 to that number minus 1.

This function is automatically vectorized on the second argument.

### `transpose` (`Ť`, `1 -> 1`)

Transpose a list of lists.

Fail if the sublists are not all of the same length.

### `setPartition` (`O`, `1 -> 1`)

Partition a list into a list of lists such that their concatenation is a permutation of the original list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

### `setMinus` (`∕`, `2 -> 1`)

For each element in the second list, remove the first occurrence of that element in the first list.

### `index` (`Ĩ`, `2 -> 1`)

Get the index of any occurrence of an element in a list.

Fail if the element does not occur in the list.

This function is non-deterministic.

### `count` (`Ĉ`, `2 -> 1`)

Count the number of occurrences of an element in a list.

### `tally` (`Ţ`, `1 -> 2`)

Count the number of occurrences of each element in a list.

Return a list of elements and a list of counts in the same order.

### `intersect` (`∩`, `2 -> 1`)

Get the intersection of two lists.

### `union` (`Ŭ`, `2 -> 1`)

Get the union of two lists.

### `chunks` (`ĉ`, `1 -> 1`)

Split a list into a list of chunks of equal elements.

### `uninterleave` (`ĭ`, `1 -> 2`)

uninterleave a list into a list of elements at even positions and a list of elements at odd positions.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

### `interleave` (`Ĭ`, `2 -> 1`)

Interleave two lists.

The length of the first list must be either equal to or one more than the length of the second list. Otherwise, this function fails.

### `minimumBy` (`ṃ`, `2 -> 1`)

Get the minimum value of a list according to a list of keys.

If there are multiple minimums, return any of them non-deterministically.

Fails when the two lists are of different lengths.

This function is non-deterministic.

### `maximumBy` (`Ṃ`, `2 -> 1`)

Get the maximum value of a list according to a list of keys.

If there are multiple maximums, return any of them non-deterministically.

Fails when the two lists are of different lengths.

This function is non-deterministic.

### `shortest` (`ş`, `1 -> 1`)

Get the shortest one in a list of lists.

If there are multiple shortest ones, return any of them non-deterministically.

This function is non-deterministic.

### `longest` (`Ş`, `1 -> 1`)

Get the longest one in a list of lists.

If there are multiple longest ones, return any of them non-deterministically.

This function is non-deterministic.

### `tuple` (`ŧ`, `2 -> 1`)

Create a list with length n, whose elements are taken from another list.

This function is non-deterministic, and automatically vectorized on the second argument.

## Particles

### `apply2` (`ᵃ`, `(0 -> n) -> (0 -> 2 * n) or (m -> n) -> (m + 1 -> 2 * n) where m > 0`)

Apply a function to the top two values of the stack.

If the function takes no argument, simply apply it twice.

### `noPop` (`ˣ`, `(m -> n) -> (m -> m + n)`)

Apply a function without popping the stack.

### `dip` (`ᵈ`, `(m -> n) -> (m + 1 -> n + 1)`)

Pop the top value of the stack, apply a function to the rest, and push the popped value back.

### `dupDip` (`ᵉ`, `(m -> n) -> (m -> n + 1)`)

Apply a function to the stack, and then push the original top value back.

### `dupDip2` (`ᵋ`, `(m -> n) -> (m -> n + 2)`)

Apply a function to the stack, and then push the original top two values back.

### `map` (`ᵐ`, `(0 -> 1) -> (1 -> 1) or (m -> 1) -> (m -> 1) where m > 0`)

Apply a function to each value in a list.

If the input is a string, apply the function to each character.

If the input is an number, apply the function to each integer from 0 to the input minus 1.

If the function takes no argument, return a list of n copies of the result of the function, where n is the length of the input.

### `mapWith` (`ᵚ`, `(1 -> 1) -> (2 -> 1) or (m -> 1) -> (m -> 1) where m > 1`)

Map a binary function over its first argument.

If the function is unary, return a list of n copies of the result of applying the function to the second argument, where n is the length of the first argument.

### `zipWith` (`ᶻ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of values.

If one of the input is a string, apply the function to each character.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

### `outer` (`ᵒ`, `(m -> 1) -> (m -> 1) where m > 1`)

Apply a function to every possible pair of values in two lists and return a list of lists.

If one of the input is a string, apply the function to each character.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

### `predicate` (`ᵖ`, `(m -> n) -> (1 -> 1)`)

Check if a function would succeed without actually applying it.

If the function fails, replace the top value with Fail.

Otherwise, do nothing.

### `predicateNot` (`ᵗ`, `(m -> n) -> (1 -> 1)`)

Check if a function would fail without actually applying it.

If the function does not fail, replace the top value with Fail.

Otherwise, do nothing.

### `filter` (`ᶠ`, `(m -> n) -> (1 -> 1)`)

For each value in a list, check if a function would succeed without actually applying it, and remove the value if it fails.

If the input is a string, convert it to a list of characters before filtering.

If the input is an number, convert it to a list of integers from 0 to the input minus 1 before filtering.

### `orApply` (`ᶜ`, `(n -> n) -> (n -> n)`)

Apply a function zero or one time non-deterministically.

### `iterate` (`ᶦ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times non-deterministically, until the top value of the stack is Fail.

This is different from `while` in that it returns the intermediate results.

### `nTimes` (`ᵑ`, `(n -> n) -> (n + 1 -> n)`)

Take an integer from the top of the stack, and apply a function that many times.

### `while` (`ʷ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times, until the top value of the stack is Fail.

This is different from `iterate` in that it does not return the intermediate results.

### `lengthWhile` (`ˡ`, `(n -> n) -> (n -> 1)`)

Apply a function zero or more times, until the top value of the stack is Fail, and return the number of times the function was applied.

### `firstInt` (`ᵏ`, `(m -> n) -> (0 -> 1)`)

Find the smallest non-negative integer for which a function does not fail, and return it.

### `fold1` (`ʳ`, `(m -> 1) -> (m - 1 -> 1) where m > 1`)

Apply a function to the first two values of a list, then apply it to the result and the third value, and so on until the end of the list.

If the input is a string, convert it to a list of characters before folding.

If the input is an number, convert it to a list of integers from 0 to the input minus 1 before folding.


