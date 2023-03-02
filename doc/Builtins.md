# Built-ins Functions and Particles

Nekomata is still in an early stage. The full names, short names, and meanings of built-in functions and particles are subject to change.

## Functions

### `choice` (`?`, `2 -> 1`)

Choose between two values.

This function is non-deterministic.

### `fail` (`!`, `0 -> 1`)

Push a value that always fails.

### `allValues` (`∀`, `1 -> 1`)

Get a list of all possible values for a non-deterministic object.

### `oneValue` (`∃`, `1 -> 1`)

Get a single value from a non-deterministic object.

Fails if the object has no values.

### `countValues` (`n`, `1 -> 1`)

Count the number of values in a non-deterministic object.

### `normalForm` (`∎`, `1 -> 1`)

Convert a non-deterministic object to the normal form.

I haven't given a formal definition for the normal form. This function basically lifts all the non-determinism in lists and strings to the top level.

### `drop` (`^`, `1 -> 0`)

Drop the top value of the stack.

### `dup` (`:`, `1 -> 2`)

Duplicate the top value of the stack.

### `swap` (`$`, `2 -> 2`)

Swap the top two values of the stack.

### `eq` (`=`, `2 -> 1`)

Check if two values are equal.

If they are, push the first value, otherwise fail.

### `ne` (`≠`, `2 -> 1`)

Check if two values are not equal.

If they are not, push the first value, otherwise fail.

### `nonempty` (`N`, `1 -> 1`)

Check if a list or string is non-empty.

If it is, push the list or string itself, otherwise fail.

### `nonzero` (`Z`, `1 -> 1`)

Check if an integer is non-zero.

If it is, push the integer itself, otherwise fail.

This function is automatically vectorized.

### `positive` (`P`, `1 -> 1`)

Check if an integer is positive.

If it is, push the integer itself, otherwise fail.

This function is automatically vectorized.

### `less` (`<`, `2 -> 1`)

Check if the first integer is less than the second.

If it is, push the first integer, otherwise fail.

This function is automatically vectorized.

### `lessEq` (`≤`, `2 -> 1`)

Check if the first integer is less than or equal to the second.

If it is, push the first integer, otherwise fail.

This function is automatically vectorized.

### `greater` (`>`, `2 -> 1`)

Check if the first integer is greater than the second.

If it is, push the first integer, otherwise fail.

This function is automatically vectorized.

### `greaterEq` (`≥`, `2 -> 1`)

Check if the first integer is greater than or equal to the second.

If it is, push the first integer, otherwise fail.

This function is automatically vectorized.

### `neg1` (`⨡`, `0 -> 1`)

The constant -1.

### `ten` (`⑩`, `0 -> 1`)

The constant 10.

### `neg` (`_`, `1 -> 1`)

Negate an integer.

This function is automatically vectorized.

### `abs` (`A`, `1 -> 1`)

Absolute value of an integer.

This function is automatically vectorized.

### `increment` (`→`, `1 -> 1`)

Increment an integer.

This function is automatically vectorized.

### `decrement` (`←`, `1 -> 1`)

Decrement an integer.

This function is automatically vectorized.

### `logicalNot` (`¬`, `1 -> 1`)

Returns 1 if the argument is 0, and 0 otherwise.

This function is automatically vectorized.

### `sign` (`±`, `1 -> 1`)

Returns -1 if the argument is negative, 0 if it is zero, and 1 if it is positive.

This function is automatically vectorized.

### `add` (`+`, `2 -> 1`)

Add two integers.

This function is automatically vectorized with padding zeros.

### `sub` (`-`, `2 -> 1`)

Subtract two integers.

This function is automatically vectorized with padding zeros.

### `mul` (`*`, `2 -> 1`)

Multiply two integers.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `div` (`÷`, `2 -> 1`)

Integer division of two integers. Result is rounded towards negative infinity.

Fails when the divisor is zero.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `mod` (`%`, `2 -> 1`)

Modulo two integers.

Fails when the divisor is zero.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `divExact` (`∣`, `2 -> 1`)

Divide two integers.

Fails when the divisor is zero or the result is not an exact integer.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `pow` (`E`, `2 -> 1`)

Raise an integer to a non-negative integer power.

Fails when the base is negative.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `min` (`m`, `2 -> 1`)

Get the minimum of two integers or two strings.

This function is automatically vectorized with padding.

### `max` (`M`, `2 -> 1`)

Get the maximum of two integers or two strings.

This function is automatically vectorized with padding.

### `range0` (`r`, `1 -> 1`)

Create a list of integers from 0 to n-1.

This function is automatically vectorized.

### `range1` (`R`, `1 -> 1`)

Create a list of integers from 1 to n.

This function is automatically vectorized.

### `natural` (`ℕ`, `0 -> 1`)

Non-deterministically choose a natural number.

This function is non-deterministic.

### `integer` (`ℤ`, `0 -> 1`)

Non-deterministically choose an integer.

This function is non-deterministic.

### `sum` (`∑`, `1 -> 1`)

Take the sum of a list of integers.

The addition is automatically vectorized with padding zeros.

### `product` (`∏`, `1 -> 1`)

Take the product of a list of integers.

The multiplication is automatically vectorized and fails when the two lists are of different lengths.

### `dot` (`∙`, `2 -> 1`)

Take the dot product of two lists of integers.

The current implementation is simply a composition of mul and sum.

### `fromBase` (`b`, `2 -> 1`)

Convert a list of digits to an integer.

The first argument is the list of digits, the second argument is the base.

This function is automatically vectorized over the base.

### `fromBaseRev` (`d`, `2 -> 1`)

Convert a list of digits in reverse order to an integer.

The first argument is the list of digits, the second argument is the base.

This function is automatically vectorized over the base.

### `toBase` (`B`, `2 -> 1`)

Convert an integer to a list of digits in reverse order.

The first argument is the integer, the second argument is the base.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

### `cumsum` (`∫`, `1 -> 1`)

Take the cumulative sum of a list of integers.

The addition is automatically vectorized with padding zeros.

### `binomial` (`K`, `2 -> 1`)

Compute the binomial coefficient.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `isPrime` (`Q`, `1 -> 1`)

Check if an integer is prime.

This function is automatically vectorized.

### `prime` (`ℙ`, `0 -> 1`)

Non-deterministically choose a prime number.

This function is non-deterministic.

### `bytes` (`e`, `1 -> 1`)

Convert a string to a list of integers according to Nekomata's custom encoding.

This function is automatically vectorized.

### `anyOf` (`~`, `1 -> 1`)

Choose an element from a list or a character from a string.

This function is non-deterministic.

### `emptyList` (`∅`, `0 -> 1`)

Push an empty list.

### `singleton` (`U`, `1 -> 1`)

Create a list with a single element.

### `pair` (`D`, `2 -> 1`)

Create a list with two elements.

### `removeFail` (`F`, `1 -> 1`)

Remove failed items from a list.

### `length` (`#`, `1 -> 1`)

Get the length of a list or a string.

### `lengthIs` (`L`, `2 -> 1`)

Check if the length of a list or a string is equal to a given integer.

If it is, push the list or string itself, otherwise fail.

### `nth` (`@`, `2 -> 1`)

Get the nth element of a list or a string.

This function is automatically vectorized on the second argument.

### `head` (`h`, `1 -> 1`)

Get the first element of a list or a string.

### `tail` (`t`, `1 -> 1`)

Remove the first element of a list or a string.

### `cons` (`c`, `2 -> 1`)

Prepend an element to a list.

### `uncons` (`C`, `1 -> 2`)

Get the first element list and the rest of a list or a string.

### `last` (`l`, `1 -> 1`)

Get the last element of a list or a string.

### `reverse` (`↔`, `1 -> 1`)

Reverse a list or a string.

### `prefix` (`p`, `1 -> 1`)

Get a prefix of a list or a string.

This function is non-deterministic.

### `suffix` (`s`, `1 -> 1`)

Get a suffix of a list or a string.

This function is non-deterministic.

### `take` (`T`, `2 -> 1`)

Get the first n elements of a list or a string.

This function is automatically vectorized on the second argument.

### `subset` (`S`, `1 -> 1`)

Get a finite subset of a list or a string.

This function is non-deterministic.

### `subsequence` (`q`, `1 -> 1`)

Get a finite contiguous subsequence of a list or a string.

This function is non-deterministic.

### `join` (`,`, `2 -> 1`)

Concatenate two lists or two strings.

If one of the arguments is a string, the other argument is converted to a string as well.

### `minimum` (`⊥`, `1 -> 1`)

Get the minimum of a list.

This order used in this function is different from the one used in min and max. It can compare two arbitrary values, not just integers or strings.

### `maximum` (`⊤`, `1 -> 1`)

Get the maximum of a list.

This order used in this function is different from the one used in min and max. It can compare two arbitrary values, not just integers or strings.

### `concat` (`j`, `1 -> 1`)

Concatenate a list of lists or a list of strings.

If one item in the list is a string, the other items are converted to strings as well.

### `unconcat` (`J`, `1 -> 1`)

Split a list or a string into a list of lists or a list of strings.

This function is non-deterministic.

### `nub` (`u`, `1 -> 1`)

Remove duplicate elements from a list or a string.

### `sort` (`o`, `1 -> 1`)

Sort a list or a string.

### `permutation` (`⇄`, `1 -> 1`)

Get a permutation of a list or a string.

This function is non-deterministic.

### `allEqual` (`≡`, `1 -> 1`)

Check if all elements in a list are equal.

If it is, push the equal element, otherwise fail.

If the list is empty, this function fails.

## Particles

### `apply2` (`ᵃ`, `(0 -> n) -> (0 -> 2 * n) or (m -> n) -> (m + 1 -> 2 * n) where m > 0`)

Apply a function to the top two values of the stack.

If the function takes no argument, simply apply it twice.

### `noPop` (`ˣ`, `(m -> n) -> (0 -> n)`)

Apply a function without popping the stack.

### `dip` (`ᵈ`, `(m -> n) -> (m + 1 -> n + 1)`)

Pop the top value of the stack, apply a function to the rest, and push the popped value back.

### `dupDip` (`ᵉ`, `(m -> n) -> (m -> n + 1)`)

Duplicate the top value of the stack, pop the top value, apply a function to the rest, and push the popped value back.

### `map` (`ᵐ`, `(m -> 1) -> (m -> 1) where m > 0`)

Apply a function to each value in a list.

If the input is a string, apply the function to each character.

If the input is an integer, apply the function to each integer from 0 to the input minus 1.

### `zipWith` (`ᶻ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of values.

If one of the input is a string, apply the function to each character.

If one of the input is an integer, apply the function to each integer from 0 to the input minus 1.

### `outer` (`ᵒ`, `(m -> 1) -> (m -> 1) where m > 1`)

Apply a function to every possible pair of values in two lists and return a list of lists.

If one of the input is a string, apply the function to each character.

If one of the input is an integer, apply the function to each integer from 0 to the input minus 1.

### `predicate` (`ᵖ`, `(m -> n) -> (1 -> 1)`)

Apply a function without pushing or popping the stack, but replace the top value with Fail if the function fails.

### `predicateNot` (`𐞥`, `(m -> n) -> (1 -> 1)`)

Apply a function without pushing or popping the stack, but replace the top value with Fail if the function succeeds.

### `orApply` (`ᶜ`, `(n -> n) -> (n -> n)`)

Apply a function zero or one time non-deterministically.

### `iterate` (`ⁱ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times non-deterministically, until the top value of the stack is Fail.

This is different from `while` in that it returns the intermediate results.

### `while` (`ʷ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times, until the top value of the stack is Fail.

This is different from `iterate` in that it does not return the intermediate results.

### `nTimes` (`ⁿ`, `(n -> n) -> (n + 1 -> n)`)

Take an integer from the top of the stack, and apply a function that many times.


