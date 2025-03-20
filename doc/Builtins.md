# Built-ins Functions and Particles

Nekomata is still in an early stage. The full names, short names, and meanings of built-in functions and particles are subject to change.

- [Functions](#functions)
- [Particles](#particles)

## Functions

### `choice` (`?`, `2 -> 1`)

Choose between two values.

This function is non-deterministic.

__Examples__:

- `1 2?` → `1 2`

### `fail` (`!`, `0 -> 1`)

Push a non-deterministic object with no values.

__Examples__:

- `!` → Fail

### `allValues` (`a`, `1 -> 1`)

Get a list of all possible values for a non-deterministic object.

__Examples__:

- `1 2?a` → `[1,2]`

### `firstValue` (`¡`, `1 -> 1`)

Get the first possible value from a non-deterministic object.

Fails if the object has no values.

__Examples__:

- `1 2?¡` → `1`

### `lastValue` (`¤`, `1 -> 1`)

Get the last possible value from a non-deterministic object.

Fails if the object has no values.

__Examples__:

- `1 2?¤` → `2`

### `countValues` (`n`, `1 -> 1`)

Count the number of values in a non-deterministic object.

__Examples__:

- `1 2?n` → `2`

### `uniqueValue` (`ũ`, `1 -> 1`)

Remove duplicate values from a non-deterministic object.

__Examples__:

- `[1,1,2]~ũ` → `1 2`

### `minValue` (`å`, `1 -> 1`)

Get the minimum possible value from a non-deterministic object.

Fails if the object has no values.

__Examples__:

- `1 2?å` → `1`

### `maxValue` (`Å`, `1 -> 1`)

Get the maximum possible value from a non-deterministic object.

Fails if the object has no values.

__Examples__:

- `1 2?Å` → `2`

### `if` (`I`, `2 -> 1`)

Choose the first value that doesn't fail between two values.

__Examples__:

- `1 2I` → `1`
- `! 2I` → `2`

### `andThen` (`¿`, `2 -> 1`)

Take two values, and return the first one if the second one doesn't fail. 

This is somewhat similar to the `seq` function in Haskell, which forces the first argument to be evaluated before the second.

__Examples__:

- `1 2¿` → `1`
- `1 !¿` → Fail

### `drop` (`^`, `1 -> 0`)

Drop the top value of the stack: `a ... -> ...`.

__Examples__:

- `1 2^` → `1`

### `dup` (`:`, `1 -> 2`)

Duplicate the top value of the stack: `a ... -> a a ...`.

__Examples__:

- `1:Ð` → `[1,1]`

### `swap` (`$`, `2 -> 2`)

Swap the top two values of the stack: `a b ... -> b a ...`.

__Examples__:

- `1 2$Ð` → `[2,1]`

### `rot3` (`§`, `3 -> 3`)

Rotate the top three values of the stack: `a b c ... -> c a b ...`.

__Examples__:

- `1 2 3§ÐÐ` → `[2,[3,1]]`

### `over` (`v`, `2 -> 3`)

Duplicate the second value of the stack, and put it on top of the stack: `a b ... -> b a b ...`.

__Examples__:

- `1 2vÐÐ` → `[1,[2,1]]`

### `eq` (`=`, `2 -> 1`)

Check if two values are equal.

If they are, push the first value, otherwise fail.

__Examples__:

- `1 1=` → `1`
- `1 2=` → Fail

### `ne` (`≠`, `2 -> 1`)

Check if two values are not equal.

If they are not, push the first value, otherwise fail.

__Examples__:

- `1 1≠` → Fail
- `1 2≠` → `1`

### `lt` (`Ļ`, `2 -> 1`)

Check if the first value is less than the second.

If it is, push the first value, otherwise fail.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `1 2Ļ` → `1`
- `1 1Ļ` → Fail
- `2 1Ļ` → Fail
- `1 'aĻ` → `1`
- `'a [1]Ļ` → `'a'`
- `[1,2] [2]Ļ` → `[1,2]`
- `[1,2] [1]Ļ` → Fail

### `gt` (`Ģ`, `2 -> 1`)

Check if the first value is greater than the second.

If it is, push the first value, otherwise fail.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `1 2Ģ` → Fail
- `1 1Ģ` → Fail
- `2 1Ģ` → `2`
- `'a 1Ģ` → `'a'`
- `[1] 'aĢ` → `[1]`
- `[1,2] [2]Ģ` → Fail
- `[1,2] [1]Ģ` → `[1,2]`

### `isNonempty` (`N`, `1 -> 1`)

Check if a list is non-empty.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1]N` → `[1]`
- `"Hello"N` → `Hello`
- `[]N` → Fail

### `isLong` (`Ł`, `1 -> 1`)

Check if the length of a list is greater than 1.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1,2]Ł` → `[1,2]`
- `[1]Ł` → Fail
- `[]Ł` → Fail

### `isNonzero` (`Z`, `1 -> 1`)

Check if a number is non-zero.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1Z` → `1`
- `0Z` → Fail
- `1_Z` → `-1`
- `[1,[2,3]]Z` → `[1,[2,3]]`

### `isPositive` (`P`, `1 -> 1`)

Check if a number is positive.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1P` → `1`
- `0P` → Fail
- `1_P` → Fail
- `[1,[2,3]]P` → `[1,[2,3]]`

### `isNonnegative` (`ň`, `1 -> 1`)

Check if a number is non-negative.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1ň` → `1`
- `0ň` → `0`
- `1_ň` → Fail
- `[1,[2,3]]ň` → `[1,[2,3]]`

### `isZero` (`ž`, `1 -> 1`)

Check if a number is zero.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1ž` → Fail
- `0ž` → `0`
- `1_ž` → Fail
- `[0,[0,0]]ž` → `[0,[0,0]]`

### `isBig` (`Ƶ`, `1 -> 1`)

Check if the absolute value of a number is greater than 1.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2Ƶ` → `2`
- `3\2Ƶ` → `3/2`
- `1Ƶ` → Fail
- `1\2Ƶ` → Fail
- `0Ƶ` → Fail
- `1_Ƶ` → Fail
- `2_Ƶ` → `-2`

### `isSmall` (`ƶ`, `1 -> 1`)

Check if the absolute value of a number is 

less than or equal to than 1.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2ƶ` → Fail
- `3\2ƶ` → Fail
- `1ƶ` → `1`
- `1\2ƶ` → `1/2`
- `0ƶ` → `0`
- `1_ƶ` → `-1`
- `2_ƶ` → Fail

### `less` (`<`, `2 -> 1`)

Check if the first number is less than the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2<` → `1`
- `1 1<` → Fail
- `2 1<` → Fail
- `[1,2,3] [2,3,4]<` → `[1,2,3]`
- `[1,2] [2,1]<` → Fail

### `lessEq` (`≤`, `2 -> 1`)

Check if the first number is less than or equal to the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2≤` → `1`
- `1 1≤` → `1`
- `2 1≤` → Fail
- `[1,2,3] [2,3,4]≤` → `[1,2,3]`
- `[1,2] [2,1]≤` → Fail

### `greater` (`>`, `2 -> 1`)

Check if the first number is greater than the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2>` → Fail
- `1 1>` → Fail
- `2 1>` → `2`
- `[2,3,4] [1,2,3]>` → `[2,3,4]`
- `[2,1] [1,2]>` → Fail

### `greaterEq` (`≥`, `2 -> 1`)

Check if the first number is greater than or equal to the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2≥` → Fail
- `1 1≥` → `1`
- `2 1≥` → `2`
- `[2,3,4] [1,2,3]≥` → `[2,3,4]`
- `[2,1] [1,2]≥` → Fail

### `neg1` (`£`, `0 -> 1`)

The constant -1.

__Examples__:

- `£` → `-1`

### `ten` (`¢`, `0 -> 1`)

The constant 10.

__Examples__:

- `¢` → `10`

### `octet` (`¥`, `0 -> 1`)

The constant 256.

__Examples__:

- `¥` → `256`

### `neg` (`_`, `1 -> 1`)

Negate a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1_` → `-1`
- `[1,[2,3]]_` → `[-1,[-2,-3]]`

### `abs` (`A`, `1 -> 1`)

Absolute value of a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1A` → `1`
- `1_A` → `1`
- `[-1,[2,-3]]A` → `[1,[2,3]]`

### `increment` (`→`, `1 -> 1`)

Increment a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1→` → `2`
- `[1,[2,3]]→` → `[2,[3,4]]`

### `decrement` (`←`, `1 -> 1`)

Decrement a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1←` → `0`
- `[1,[2,3]]←` → `[0,[1,2]]`

### `logicalNot` (`¬`, `1 -> 1`)

Takes a number and returns 1 if it is 0, and 0 otherwise.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1¬` → `0`
- `2¬` → `0`
- `0¬` → `1`
- `[-1,[0,1]]¬` → `[0,[1,0]]`

### `sign` (`±`, `1 -> 1`)

Returns -1 if the argument is negative, 0 if it is zero, and 1 if it is positive.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2±` → `1`
- `0±` → `0`
- `[-2,[0,2]]±` → `[-1,[0,1]]`

### `add` (`+`, `2 -> 1`)

Add two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding zeros.

__Examples__:

- `1 1+` → `2`
- `[1,2] [2,3]+` → `[3,5]`
- `1 [2,3]+` → `[3,4]`
- `[1] [2,3]+` → `[3,3]`
- `[[1],[0,1]] [[0,2],[2]]+` → `[[1,2],[2,1]]`

### `sub` (`-`, `2 -> 1`)

Subtract two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding zeros.

__Examples__:

- `1 1-` → `0`
- `[1,2] [2,3]-` → `[-1,-1]`
- `1 [2,3]-` → `[-1,-2]`
- `[1] [2,3]-` → `[-1,-3]`
- `[[1],[0,1]] [[0,2],[2]]-` → `[[1,-2],[-2,1]]`

### `absDiff` (`≈`, `2 -> 1`)

Absolute difference of two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding zeros.

__Examples__:

- `1 2≈` → `1`
- `[1,2] [3,1]≈` → `[2,1]`
- `2 [1,3]≈` → `[1,1]`
- `[1] [3,1]≈` → `[2,1]`

### `mul` (`*`, `2 -> 1`)

Multiply two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `2 3*` → `6`
- `[2,3] [3,4]*` → `[6,12]`
- `2 [3,4]*` → `[6,8]`
- `[2] [3,4]*` → Fail

### `div` (`/`, `2 -> 1`)

Division of two numbers.

Fails when the divisor is zero.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `6 3/` → `2`
- `3 6/` → `1/2`
- `[3,6] [2,3]/` → `[3/2,2]`
- `3 [2,3]/` → `[3/2,1]`
- `[3] [2,3]/` → Fail

### `divInt` (`÷`, `2 -> 1`)

Integer division of two numbers. Result is rounded towards negative infinity.

Fails when the divisor is zero.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `6 3÷` → `2`
- `3 6÷` → `0`
- `3_ 6÷` → `-1`
- `[3,6] [-2,3]÷` → `[-2,2]`
- `3 [-2,3]÷` → `[-2,1]`
- `[3] [-2,3]÷` → Fail

### `mod` (`%`, `2 -> 1`)

Modulo two numbers.

Fails when the divisor is zero.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `5 3%` → `2`
- `5_ 3%` → `1`
- `5 3_%` → `-1`
- `5_ 3_%` → `-2`
- `[5,6] [3,4]%` → `[2,2]`
- `5 [3,4]%` → `[2,1]`
- `[5] [3,4]%` → Fail

### `divExact` (`¦`, `2 -> 1`)

Divide two numbers.

Fails when the divisor is zero or the result is not an integer.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `6 3¦` → `2`
- `5 3¦` → Fail
- `[6,4] [3,4]¦` → `[2,1]`
- `6 [2,3]¦` → `[3,2]`
- `[6] [2,3]¦` → Fail

### `divMod` (`þ`, `2 -> 2`)

Divide two numbers and return both the quotient and the remainder.

Fails when the divisor is zero.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `5 3þÐ` → `[1,2]`

### `half` (`½`, `1 -> 1`)

Check if an integer is even, and divide it by 2.

Fails when the integer is odd.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `6½` → `3`
- `5½` → Fail
- `[6,4]½` → `[3,2]`

### `pow` (`E`, `2 -> 1`)

Raise a number to a power.

The first argument is the exponent, the second argument is the base.

Fails when the exponent is not an integer.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `2 3E` → `9`
- `3 2E` → `8`
- `2 1\2E` → `1/4`
- `1\2 2E` → Fail
- `[-2,0,2] 2E` → `[1/4,1,4]`
- `[2] [3,4]E` → Fail

### `recip` (`ŗ`, `1 -> 1`)

Reciprocal of a number.

Fails when the number is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2ŗ` → `1/2`
- `0ŗ` → Fail
- `[2,3]ŗ` → `[1/2,1/3]`

### `mul2` (`Ä`, `1 -> 1`)

Multiply a number by 2.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2Ä` → `4`
- `[2,3]Ä` → `[4,6]`

### `div2` (`ä`, `1 -> 1`)

Divide a number by 2.

This is different from `half` in that it may return a non-integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2ä` → `1`
- `[2,3]ä` → `[1,3/2]`

### `mod2` (`Ö`, `1 -> 1`)

Modulo a number by 2.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `5Ö` → `1`
- `[5,6]Ö` → `[1,0]`

### `powOf2` (`Ë`, `1 -> 1`)

Raise 2 to a power.

Fails when the exponent is not an integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `3Ë` → `8`
- `[-2,0,2]Ë` → `[1/4,1,4]`

### `denominator` (`ḍ`, `1 -> 1`)

Get the denominator of a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1\2ḍ` → `2`
- `2ḍ` → `1`
- `[2/3,3/5]ḍ` → `[3,5]`

### `numerator` (`ṇ`, `1 -> 1`)

Get the numerator of a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1\2ṇ` → `1`
- `2ṇ` → `2`
- `[2/3,3/5]ṇ` → `[2,3]`

### `min` (`m`, `2 -> 1`)

Get the minimum of two numbers or two chars.

This function is automatically vectorized with padding.

__Examples__:

- `1 2m` → `1`
- `[1,2] [2,1]m` → `[1,1]`
- `2 [1,3]m` → `[1,2]`
- `[2] [1,3]m` → `[1,3]`

### `max` (`M`, `2 -> 1`)

Get the maximum of two numbers or two chars.

This function is automatically vectorized with padding.

__Examples__:

- `1 2M` → `2`
- `[1,2] [2,1]M` → `[2,2]`
- `2 [1,3]M` → `[2,3]`
- `[2] [1,3]M` → `[2,3]`

### `ceil` (`K`, `1 -> 1`)

Round a number up to the nearest integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1K` → `1`
- `1\2K` → `1`
- `[5/2,-3/2]K` → `[3,-1]`

### `floor` (`k`, `1 -> 1`)

Round a number down to the nearest integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1k` → `1`
- `1\2k` → `0`
- `[5/2,-3/2]k` → `[2,-2]`

### `range0` (`r`, `1 -> 1`)

Create a list of integers from 0 to ceil(n)-1.

This function is automatically vectorized.

__Examples__:

- `3r` → `[0,1,2]`
- `5\2r` → `[0,1,2]`
- `1_r` → `[]`
- `[3,4]r` → `[[0,1,2],[0,1,2,3]]`

### `range1` (`R`, `1 -> 1`)

Create a list of integers from 1 to floor(n).

This function is automatically vectorized.

__Examples__:

- `3R` → `[1,2,3]`
- `5\2R` → `[1,2]`
- `1_R` → `[]`
- `[3,4]R` → `[[1,2,3],[1,2,3,4]]`

### `interval` (`ï`, `2 -> 1`)

Create a list of integers from ceil(x) to floor(y).

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `3 5ï` → `[3,4,5]`
- `5 3ï` → `[]`
- `3\2 7\2ï` → `[2,3]`
- `1_ [2,3,-3]ï` → `[[-1,0,1,2],[-1,0,1,2,3],[]]`
- `[3] [5,7]ï` → Fail

### `natural` (`Ň`, `0 -> 1`)

Non-deterministically choose a natural number.

This function is non-deterministic.

__Examples__:

- `Ň` → `0 1 2 3 4 5 ...`

### `integer` (`Ž`, `0 -> 1`)

Non-deterministically choose an integer.

This function is non-deterministic.

__Examples__:

- `Ž` → `0 1 -1 2 -2 3 ...`

### `sum` (`∑`, `1 -> 1`)

Take the sum of a list of numbers.

The addition is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3]∑` → `6`
- `[[1,2],[3,4]]∑` → `[4,6]`
- `[1,[2,3]]∑` → `[3,4]`
- `[[],[1],[0,1],[0,0,1]]∑` → `[1,1,1]`

### `product` (`∏`, `1 -> 1`)

Take the product of a list of numbers.

The multiplication is automatically vectorized and fails when the two lists are of different lengths.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3]∏` → `6`
- `[[1,2],[3,4]]∏` → `[3,8]`
- `[2,[3,4]]∏` → `[6,8]`
- `[[1],[2,3]]∏` → Fail

### `dot` (`∙`, `2 -> 1`)

Take the dot product of two lists of numbers.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

The current implementation is simply a composition of mul and sum.

__Examples__:

- `[1,2,3] [4,5,6]∙` → `32`
- `[1,2,3] [[1,2],[3,4],[5,6]]∙` → `[22,28]`

### `convolve` (`×`, `2 -> 1`)

Take the convolution of two lists of numbers.

This is equivalent to multiplying two polynomials.

If one of the arguments is a number, it simply multiplies the other argument by that number.

If the arguments are nested lists, it takes the multi-dimensional convolution, which is equivalent to multiplying multivariate polynomials.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3] [4,5,6]×` → `[4,13,28,27,18]`
- `[1,2,3] 4×` → `[4,8,12]`
- `[1,2,3] [[1,2],[3,4]]×` → `[[1,2],[5,8],[9,14],[9,12]]`
- `[[0,1],[1]] [[0,1],[1]]×` → `[[0,0,1],[0,2],[1]]`

### `mean` (`µ`, `1 -> 1`)

Take the mean of a list of numbers.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3]µ` → `2`
- `[[1,2],[3,5]]µ` → `[2,7/2]`

### `fromBase` (`b`, `2 -> 1`)

Convert a list of digits to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

If the base is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized over the base.

__Examples__:

- `[1,2,3] 10b` → `123`
- `[1,2,3] 1\10b` → `321/100`
- `[[1,2],[3,4],[5,6]] 10b` → `[135,246]`
- `[1,2,3] [10,100]b` → `[123,10203]`

### `fromBaseRev` (`d`, `2 -> 1`)

Convert a list of digits in reverse order to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

If the base is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized over the base.

__Examples__:

- `[1,2,3] 10d` → `321`
- `[1,2,3] 1\10d` → `123/100`
- `[[1,2],[3,4],[5,6]] 10d` → `[531,642]`
- `[1,2,3] [10,100]d` → `[321,30201]`

### `toBase` (`D`, `2 -> 1`)

Convert an integer to a list of digits.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

If the integer is smaller than zero, it is negated before conversion.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

__Examples__:

- `123 10D` → `[1,2,3]`
- `123 1\10D` → Fail
- `123_ 10D` → `[1,2,3]`
- `[135,246] 10D` → `[[1,3,5],[2,4,6]]`
- `[135,246] [10,100]D` → `[[[1,3,5],[2,4,6]],[[1,35],[2,46]]]`

### `toBaseRev` (`B`, `2 -> 1`)

Convert an integer to a list of digits in reverse order.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

If the integer is smaller than zero, it is negated before conversion.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

__Examples__:

- `123 10B` → `[3,2,1]`
- `123 1\10B` → Fail
- `123_ 10B` → `[3,2,1]`
- `[135,246] 10B` → `[[5,3,1],[6,4,2]]`
- `[135,246] [10,100]B` → `[[[5,3,1],[6,4,2]],[[35,1],[46,2]]]`

### `binary` (`Ƃ`, `1 -> 1`)

Convert an integer to a list of binary digits in reverse order.

If the integer is smaller than zero, it is negated before conversion.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `6Ƃ` → `[0,1,1]`
- `[-6,0,6]Ƃ` → `[[0,1,1],[],[0,1,1]]`

### `fromBinary` (`ƃ`, `1 -> 1`)

Convert a list of binary digits in reverse order to an integer.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[0,1,1]ƃ` → `6`
- `[[1,0,1],[0,1,1]]ƃ` → `[1,2,3]`

### `digits` (`Ɗ`, `1 -> 1`)

Convert an integer to a list of decimal digits.

If the integer is smaller than zero, it is negated before conversion.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `123Ɗ` → `[1,2,3]`
- `[-123,0,123]Ɗ` → `[[1,2,3],[],[1,2,3]]`

### `fromDigits` (`ɗ`, `1 -> 1`)

Convert a list of decimal digits to an integer.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3]ɗ` → `123`
- `[[1,2,3],[0,1,2]]ɗ` → `[10,21,32]`

### `cumsum` (`∫`, `1 -> 1`)

Take the cumulative sum of a list of numbers.

The addition is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,2,3]∫` → `[1,3,6]`
- `[[1,2],[3,4]]∫` → `[[1,2],[4,6]]`

### `delta` (`∆`, `1 -> 1`)

Take the difference between adjacent elements of a list of numbers.

The subtraction is automatically vectorized with padding zeros.

Fails when the input is empty.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[1,3,6]∆` → `[2,3]`
- `[[1,2],[4,6]]∆` → `[[3,4]]`
- `[1]∆` → `[]`
- `[]∆` → Fail

### `binomial` (`Ç`, `2 -> 1`)

Compute the binomial coefficient.

The second argument must be an integer.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `5 3Ç` → `10`
- `5_ 3Ç` → `-35`
- `5 3_Ç` → `0`
- `1\2 2Ç` → `-1/8`
- `[5,6] 3Ç` → `[10,20]`
- `5 [0,1,2,3,4,5]Ç` → `[1,5,10,10,5,1]`

### `factorial` (`F`, `1 -> 1`)

Compute the factorial of an integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `0F` → `1`
- `5F` → `120`
- `[5,6]F` → `[120,720]`

### `isPrime` (`Q`, `1 -> 1`)

Check if an integer is prime.

Negative numbers whose absolute values are prime are also considered to be prime.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1Q` → Fail
- `2Q` → `2`
- `2_ Q` → `-2`
- `[1,2,3,4,5]Q‼` → `[2,3,5]`

### `prime` (`Ƥ`, `0 -> 1`)

Non-deterministically choose a prime number.

This function is non-deterministic.

__Examples__:

- `Ƥ` → `2 3 5 7 11 13 17 19 23 ...`

### `primePi` (`ƥ`, `1 -> 1`)

Compute the number of positive primes less than or equal to a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `7ƥ` → `4`
- `7_ ƥ` → `0`
- `23\2 ƥ` → `5`
- `[10,100,1000]ƥ` → `[4,25,168]`

### `factor` (`ƒ`, `1 -> 2`)

Factorize a rational number, and return a list of prime factors and a list of exponents.

If the number is negative, it is negated before factorization.

Fails when the input is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `12300ƒÐ` → `[[2,3,5,41],[2,1,2,1]]`
- `123\100 ƒÐ` → `[[2,3,5,41],[-2,1,-2,1]]`
- `1ƒÐ` → `[[],[]]`
- `0ƒÐ` → Fail
- `[6,-6]ƒÐ` → `[[[2,3],[2,3]],[[1,1],[1,1]]]`

### `gcd` (`G`, `2 -> 1`)

Compute the greatest common divisor of two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `12 18G` → `6`
- `1\12 1\18G` → `1/36`
- `[12,18] [24,36]G` → `[12,18]`

### `lcm` (`g`, `2 -> 1`)

Compute the least common multiple of two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `12 18g` → `36`
- `1\12 1\18g` → `1/6`
- `[12,18] [24,36]g` → `[24,36]`

### `divisors` (`Ď`, `1 -> 1`)

Compute the list of positive divisors of an integer.

Fail when the input is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `12Ď` → `[1,2,3,4,6,12]`
- `12_ Ď` → `[1,2,3,4,6,12]`
- `0Ď` → Fail
- `[12,18]Ď` → `[[1,2,3,4,6,12],[1,2,3,6,9,18]]`

### `intPartition` (`Ṗ`, `1 -> 1`)

Partition an integer into a list of positive integers, whose sum is the original integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is non-deterministic and automatically vectorized.

__Examples__:

- `4Ṗ` → `[1,1,1,1] [1,1,2] [1,3] [2,2] [4]`
- `0Ṗ` → `[]`
- `4_ Ṗ` → Fail
- `[2,2]Ṗ` → `[[1,1],[1,1]] [[1,1],[2]] [[2],[1,1]] [[2],[2]]`

### `sqrt` (`√`, `1 -> 1`)

Compute the square root of a rational number.

Fails when the argument is not a perfect square.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `16√` → `4`
- `16\9√` → `4/3`
- `8√` → Fail
- `[16,25]√` → `[4,5]`

### `unitVec2` (`į`, `0 -> 1`)

Choose one of [0, 1] and [1, 0] non-deterministically.

This function is non-deterministic.

__Examples__:

- `į` → `[0,1] [1,0]`

### `orNeg` (`ŋ`, `1 -> 1`)

Optionally negate a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is non-deterministic and automatically vectorized.

When the input is a list, each element is optionally negated independently.

__Examples__:

- `1ŋ` → `1 -1`
- `0ŋ` → `0`
- `[-1,2]ŋ` → `[-1,2] [-1,-2] [1,2] [1,-2]`

### `bitAnd` (`&`, `2 -> 1`)

Bitwise AND of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `5 3&` → `1`
- `[5,6] [3,4]&` → `[1,4]`
- `5 [3,4]&` → `[1,4]`
- `[5] [3,4]&` → Fail

### `bitOr` (`|`, `2 -> 1`)

Bitwise OR of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding.

__Examples__:

- `5 3|` → `7`
- `[5,6] [3,4]|` → `[7,6]`
- `5 [3,4]|` → `[7,5]`
- `[5] [3,4]|` → `[7,4]`

### `bitXor` (`X`, `2 -> 1`)

Bitwise XOR of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding.

__Examples__:

- `5 3X` → `6`
- `[5,6] [3,4]X` → `[6,2]`
- `5 [3,4]X` → `[6,1]`
- `[5] [3,4]X` → `[6,4]`

### `popCount` (`Þ`, `1 -> 1`)

Count the number of 1s in the binary digits of an integer.

If the number is smaller than zero, the result is also negated.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `13Þ` → `3`
- `[-13,0,13]Þ` → `[-3,0,3]`

### `histogram` (`Ħ`, `1 -> 1`)

Compute the histogram of a list of integers.

The result is a list, whose length is the maximum of the input list, and whose nth element is the number of occurrences of n in the input.

Fails when the list contains negative integers or fractions.

If the input is a ragged list, it is flattened before computation.

If the input is a single integer, it is treated as a singleton list.

If the input is a single char, it is converted to a number according to Nekomata's code page, and then treated as a singleton list.

__Examples__:

- `0Ħ` → `[1]`
- `1Ħ` → `[0,1]`
- `[1,2,3,2,1]Ħ` → `[0,2,2,1]`
- `[[1,2],[3,2],[1]]Ħ` → `[0,2,2,1]`

### `sumEach` (`Ŝ`, `1 -> 1`)

Take the sum of each list in a list of lists of numbers.

The addition is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

__Examples__:

- `[[1,2],[3,4]]Ŝ` → `[3,7]`
- `[[1,2],[3,4],[5]]Ŝ` → `[3,7,5]`
- `[[[1,2],[3,4]],[[5,6],[7,8]]]Ŝ` → `[[4,6],[12,14]]`

### `unmul` (`ŝ`, `1 -> 2`)

Factorize an integer into two factors.

Fail when the input is not a positive integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is non-deterministic.

__Examples__:

- `12ŝÐ` → `[1,12] [2,6] [3,4] [4,3] [6,2] [12,1]`
- `13ŝÐ` → `[1,13] [13,1]`
- `0ŝ` → Fail
- `12_ ŝÐ` → Fail

### `charToInt` (`e`, `1 -> 1`)

Convert a char to an integer according to Nekomata's code page.

If the input is already an integer, it is left unchanged.

This function is automatically vectorized.

__Examples__:

- `'a e` → `97`
- `"Hello"e` → `[72,101,108,108,111]`

### `intToChar` (`H`, `1 -> 1`)

Convert an integer to a char according to Nekomata's code page.

If the input is already a char, it is left unchanged.

Fail when the integer is not in the range 0 to 255.

This function is automatically vectorized.

__Examples__:

- `97H` → `'a'`
- `[72,101,108,108,111]H` → `Hello`

### `read` (`Ĝ`, `1 -> 1`)

Parse a string (a list of chars) or a single char as a Nekomata value.

Fail when the string is not a valid Nekomata value.

__Examples__:

- `'1 Ĝ` → `1`
- `"[1,2,3]"Ĝ` → `[1,2,3]`

### `show` (`ĝ`, `1 -> 1`)

Convert a Nekomata value to a string (a list of chars).

__Examples__:

- `1ĝU` → `["1"]`
- `[1,2,3]ĝU` → `["[1,2,3]"]`
- `"Hello"ĝU` → `["\"Hello\""]`

### `anyOf` (`~`, `1 -> 1`)

Choose an element from a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[]~` → Fail
- `[1,2,3]~` → `1 2 3`
- `5~` → `0 1 2 3 4`

### `emptyList` (`Ø`, `0 -> 1`)

Push an empty list.

__Examples__:

- `Ø` → `[]`

### `singleton` (`U`, `1 -> 1`)

Create a list with a single element.

__Examples__:

- `1U` → `[1]`
- `[1]U` → `[[1]]`

### `unsingleton` (`z`, `1 -> 1`)

Get the only element of a list with a single element.

Fails when the list is empty or has more than one element.

__Examples__:

- `[1]z` → `1`
- `[[1]]z` → `[1]`
- `[]z` → Fail
- `[1,2]z` → Fail

### `pair` (`Ð`, `2 -> 1`)

Create a list with two elements.

__Examples__:

- `1 2Ð` → `[1,2]`
- `[1] 2Ð` → `[[1],2]`

### `unpair` (`đ`, `1 -> 2`)

Get the two elements of a list with two elements.

Fails when the length of the list is not 2.

__Examples__:

- `[1,2]đ+` → `3`
- `[]đ` → Fail
- `[1]đ` → Fail
- `[1,2,3]đ` → Fail

### `removeFail` (`‼`, `1 -> 1`)

Remove failed items from a list.

__Examples__:

- `[1,2,3]‼` → `[1,2,3]`
- `[1,0,3]P‼` → `[1,3]`

### `length` (`#`, `1 -> 1`)

Get the length of a list.

__Examples__:

- `[1,2,3]#` → `3`
- `[]#` → `0`

### `lengthIs` (`L`, `2 -> 1`)

Check if the length of a list is equal to a given integer.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1,2,3] 3L` → `[1,2,3]`
- `[1,2,3] 4L` → Fail

### `nth` (`@`, `2 -> 1`)

Get the nth element of a list.

The index is 0-based.

This function is automatically vectorized on the second argument.

__Examples__:

- `[1,2,3] 1@` → `2`
- `[1,2,3] [1,2]@` → `[2,3]`

### `head` (`h`, `1 -> 1`)

Get the first element of a list.

__Examples__:

- `[1,2,3]h` → `1`
- `[]h` → Fail

### `tail` (`t`, `1 -> 1`)

Remove the first element of a list.

__Examples__:

- `[1,2,3]t` → `[2,3]`
- `[]t` → Fail

### `cons` (`c`, `2 -> 1`)

Prepend an element to a list.

__Examples__:

- `[2,3] 1c` → `[1,2,3]`
- `[] 1c` → `[1]`

### `uncons` (`C`, `1 -> 2`)

Get the first element and the rest of a list.

__Examples__:

- `[1,2,3]CÐ` → `[[2,3],1]`
- `[]C` → Fail

### `last` (`l`, `1 -> 1`)

Get the last element of a list.

__Examples__:

- `[1,2,3]l` → `3`
- `[]l` → Fail

### `init` (`i`, `1 -> 1`)

Remove the last element of a list.

__Examples__:

- `[1,2,3]i` → `[1,2]`
- `[]i` → Fail

### `snoc` (`ɔ`, `2 -> 1`)

Append an element to a list.

__Examples__:

- `[1,2] 3ɔ` → `[1,2,3]`
- `[] 1ɔ` → `[1]`

### `unsnoc` (`Ɔ`, `1 -> 2`)

Get the last element and the rest of a list.

__Examples__:

- `[1,2,3]ƆÐ` → `[[1,2],3]`
- `[]Ɔ` → Fail

### `cons0` (`ç`, `1 -> 1`)

Prepend a zero to a list.

__Examples__:

- `[1,2,3]ç` → `[0,1,2,3]`
- `[]ç` → `[0]`

### `reverse` (`↔`, `1 -> 1`)

Reverse a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

__Examples__:

- `[1,2,3]↔` → `[3,2,1]`
- `3↔` → `[2,1,0]`

### `prefix` (`p`, `1 -> 1`)

Get a prefix of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]p` → `[] [1] [1,2] [1,2,3]`
- `3p` → `[] [0] [0,1] [0,1,2]`

### `suffix` (`s`, `1 -> 1`)

Get a suffix of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]s` → `[1,2,3] [2,3] [3] []`
- `3s` → `[0,1,2] [1,2] [2] []`

### `take` (`T`, `2 -> 1`)

Get the first n elements of a list.

Fail when the list is shorter than n.

This function is automatically vectorized on the second argument.

__Examples__:

- `[1,2,3] 2T` → `[1,2]`
- `[1,2,3] 4T` → Fail
- `[1,2,3] [2,3]T` → `[[1,2],[1,2,3]]`

### `subset` (`S`, `1 -> 1`)

Get a finite subset of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2] S` → `[] [1] [2] [1,2]`
- `2S` → `[] [0] [1] [0,1]`

### `subsequence` (`q`, `1 -> 1`)

Get a finite contiguous subsequence of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]q` → `[] [1] [1,2] [1,2,3] [2] [2,3] [3]`
- `3q` → `[] [0] [0,1] [0,1,2] [1] [1,2] [2]`

### `join` (`,`, `2 -> 1`)

Concatenate two lists.

If one of the arguments is a number or a char, it is converted to a singleton list before concatenation.

__Examples__:

- `[1,2] [3,4],` → `[1,2,3,4]`
- `[1,2] 3,` → `[1,2,3]`
- `1 [2,3],` → `[1,2,3]`
- `1 2,` → `[1,2]`

### `split` (`;`, `1 -> 2`)

Split a list into two parts.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3];Ð` → `[[],[1,2,3]] [[1],[2,3]] [[1,2],[3]] [[1,2,3],[]]`
- `3;Ð` → `[[],[0,1,2]] [[0],[1,2]] [[0,1],[2]] [[0,1,2],[]]`

### `replicate` (`ř`, `2 -> 1`)

Create a list with n copies of an element.

This function is automatically vectorized on the second argument.

__Examples__:

- `2 3ř` → `[2,2,2]`
- `'a 3ř` → `aaa`
- `[1,2] 3ř` → `[[1,2],[1,2],[1,2]]`
- `2 [3,4]ř` → `[[2,2,2],[2,2,2,2]]`

### `minimum` (`ṁ`, `1 -> 1`)

Get the minimum of a list.

If there are multiple minimums, return the first one.

Fail when the list is empty.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `[1,2,3]ṁ` → `1`
- `[[1,2],3]ṁ` → `3`
- `[[1,2],[3]]ṁ` → `[1,2]`
- `[1,'a',[1,2]]ṁ` → `1`

### `maximum` (`Ṁ`, `1 -> 1`)

Get the maximum of a list.

If there are multiple maximums, return the first one.

Fail when the list is empty.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `[1,2,3]Ṁ` → `3`
- `[[1,2],3]Ṁ` → `[1,2]`
- `[[1,2],[3]]Ṁ` → `[3]`
- `[1,'a',[1,2]]Ṁ` → `[1,2]`

### `minMax` (`ɱ`, `1 -> 2`)

Get both the minimum and the maximum of a list.

If there are multiple minimums or maximums, return the first one.

Fail when the list is empty.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `[1,2,3]ɱÐ` → `[1,3]`
- `[[1,2],3]ɱÐ` → `[3,[1,2]]`
- `[[1,2],[3]]ɱÐ` → `[[1,2],[3]]`
- `[1,'a',[1,2]]ɱÐ` → `[1,[1,2]]`

### `concat` (`j`, `1 -> 1`)

Concatenate a list of lists or a list.

If one item in the list is a number or a char, it is converted to a singleton list before concatenation.

__Examples__:

- `[[1,2],[3,4]]j` → `[1,2,3,4]`
- `[1,2,3]j` → `[1,2,3]`
- `[1,[2,3],4]j` → `[1,2,3,4]`
- `["abc","def"]j` → `abcdef`

### `unconcat` (`J`, `1 -> 1`)

Split a list into a list of lists.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]J` → `[[1],[2],[3]] [[1],[2,3]] [[1,2],[3]] [[1,2,3]]`
- `3J` → `[[0],[1],[2]] [[0],[1,2]] [[0,1],[2]] [[0,1,2]]`

### `nub` (`u`, `1 -> 1`)

Remove duplicate elements from a list.

__Examples__:

- `[1,2,2,3,1]u` → `[1,2,3]`
- `[3,1,3,2,1]u` → `[3,1,2]`

### `sort` (`o`, `1 -> 1`)

Sort a list.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `[3,1,2]o` → `[1,2,3]`
- `['a',[3,4],'b',[2],1,[5]]o` → `[1,'a','b',[2],[3,4],[5]]`

### `permutation` (`↕`, `1 -> 1`)

Get a permutation of a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]↕` → `[1,2,3] [1,3,2] [2,1,3] [2,3,1] [3,1,2] [3,2,1]`
- `[1,1,2]↕` → `[1,1,2] [1,2,1] [1,1,2] [1,2,1] [2,1,1] [2,1,1]`
- `3↕` → `[0,1,2] [0,2,1] [1,0,2] [1,2,0] [2,0,1] [2,1,0]`

### `extract` (`ĕ`, `1 -> 2`)

Extract an element from a list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

Returns the element and the rest of the list.

This function is non-deterministic.

__Examples__:

- `[1,2,3]ĕÐ` → `[[2,3],1] [[1,3],2] [[1,2],3]`
- `3ĕÐ` → `[[1,2],0] [[0,2],1] [[0,1],2]`

### `allEqual` (`≡`, `1 -> 1`)

Check if all elements in a list are equal.

If it is, push the equal element, otherwise fail.

If the list is empty, this function fails.

__Examples__:

- `[1,1,1]≡` → `1`
- `[1,2,1]≡` → Fail
- `[1]≡` → `1`
- `[]≡` → Fail

### `isUnique` (`ů`, `1 -> 1`)

Check if all elements in a list are unique.

If it is, push the list itself, otherwise fail.

The empty list is considered unique.

__Examples__:

- `[1,2,3]ů` → `[1,2,3]`
- `[1,2,1]ů` → Fail
- `[1]ů` → `[1]`
- `[]ů` → `[]`

### `free` (`f`, `2 -> 1`)

Check if a list is free of a given element.

This means that the list is not equal to the element, and recursively, every item of the list if free of that element.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1,2,3] 2f` → Fail
- `[1,3,1] 2f` → `[1,3,1]`
- `[1,[2,3]] 2f` → Fail
- `2 2f` → Fail
- `3 2f` → `3`
- `[1,2,3] [2,3]f` → `[1,2,3]`
- `[1,[2,3]] [2,3]f` → Fail

### `enumerate` (`x`, `1 -> 2`)

Push a list of integers from 0 to the length of the argument minus 1 without popping the original argument.

__Examples__:

- `[1,2,3]xÐ` → `[[1,2,3],[0,1,2]]`
- `[4,3,2,1]xÐ` → `[[4,3,2,1],[0,1,2,3]]`

### `rotate` (`Ř`, `2 -> 1`)

Rotate a list by a given number of positions.

If the first argument is a number, it is converted to a range from 0 to that number minus 1.

This function is automatically vectorized on the second argument.

__Examples__:

- `[1,2,3] 1Ř` → `[2,3,1]`
- `[1,2,3] 1_Ř` → `[3,1,2]`
- `[1,2,3] [4,5]Ř` → `[[2,3,1],[3,1,2]]`
- `3 1Ř` → `[1,2,0]`

### `transpose` (`Ť`, `1 -> 1`)

Transpose a list of lists.

Fail if the sublists are not all of the same length.

__Examples__:

- `[[1,2],[3,4],[5,6]]Ť` → `[[1,3,5],[2,4,6]]`
- `[[1,2],[3,4,5]]Ť` → Fail

### `setPartition` (`O`, `1 -> 1`)

Partition a list into a list of lists such that their concatenation is a permutation of the original list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic.

__Examples__:

- `[1,2,3]O` → `[[1,2,3]] [[2,3],[1]] [[1,3],[2]] [[3],[1,2]] [[3],[2],[1]]`
- `3O` → `[[0,1,2]] [[1,2],[0]] [[0,2],[1]] [[2],[0,1]] [[2],[1],[0]]`

### `setMinus` (`∕`, `2 -> 1`)

For each element in the second list, remove the first occurrence of that element in the first list.

If the second argument is a number or a char, it is converted to a singleton list.

__Examples__:

- `[1,2,3,2,1] [2,1]∕` → `[3,2,1]`
- `[1,2,3,2,1] [2,1,1,1]∕` → `[3,2]`
- `[1,2,3,2,1] [2,4]∕` → `[1,3,2,1]`
- `[1,2,3,2,1] 2∕` → `[1,3,2,1]`

### `index` (`Ĩ`, `2 -> 1`)

Get the index of any occurrence of an element in a list.

The index is 0-based.

Fail if the element does not occur in the list.

This function is non-deterministic.

__Examples__:

- `[1,2,3,2,1] 2Ĩ` → `1 3`
- `[1,2,3,2,1] 4Ĩ` → Fail

### `count` (`Ĉ`, `2 -> 1`)

Count the number of occurrences of an element in a list.

__Examples__:

- `[1,2,3,2,1] 2Ĉ` → `2`
- `[1,2,3,2,1] 4Ĉ` → `0`

### `tally` (`Ţ`, `1 -> 2`)

Count the number of occurrences of each element in a list.

Return a list of elements and a list of counts in the same order.

__Examples__:

- `[1,2,3,2,1]ŢÐ` → `[[1,2,3],[2,2,1]]`
- `[3,1,3,2,1]ŢÐ` → `[[3,1,2],[2,2,1]]`
- `[]ŢÐ` → `[[],[]]`

### `intersect` (`∩`, `2 -> 1`)

Get the multiset intersection of two lists.

If one of the arguments is a number or a char, it is converted to a singleton list.

__Examples__:

- `[1,2,3,2,1] [2,1]∩` → `[2,1]`
- `[1,2,3,2,1] [2,1,1,1]∩` → `[2,1,1]`
- `[1,2,3,2,1] [2,4]∩` → `[2]`
- `[1,1,2,3] [1,2,3,3]∩` → `[1,2,3]`
- `[1,2,3,2,1] 2∩` → `[2]`

### `union` (`Ŭ`, `2 -> 1`)

Get the multiset union of two lists.

__Examples__:

- `[1,2,3,2,1] [2,1]Ŭ` → `[1,2,3,2,1]`
- `[1,2,3,2,1] [2,1,1,1]Ŭ` → `[1,2,3,2,1,1]`
- `[1,2,3,2,1] [2,4]Ŭ` → `[1,2,3,2,1,4]`
- `[1,1,2,3] [1,2,3,3]Ŭ` → `[1,1,2,3,3]`
- `[1,2,3,2,1] 4Ŭ` → `[1,2,3,2,1,4]`

### `chunks` (`ĉ`, `1 -> 1`)

Split a list into a list of chunks of equal elements.

__Examples__:

- `[1,1,2,2,2,3,3,3,3]ĉ` → `[[1,1],[2,2,2],[3,3,3,3]]`
- `"aaabbbccaa"ĉ` → `["aaa","bbb","cc","aa"]`

### `rle` (`Y`, `1 -> 2`)

Run-length encode a list.

Returns a list of elements and a list of lengths.

__Examples__:

- `[1,1,2,2,2,3,3,3,3]YÐ` → `[[1,2,3],[2,3,4]]`
- `"aaabbbccaa"YÐ` → `["abca",[3,3,2,2]]`

### `unrle` (`y`, `2 -> 1`)

Run-length decode a list.

The first argument is a list of elements, the second argument is a list of lengths.

Fails when the two lists are of different lengths.

__Examples__:

- `[1,2,3] [2,3,4]y` → `[1,1,2,2,2,3,3,3,3]`
- `"abca" [3,3,2,2]y` → `aaabbbccaa`

### `slices` (`Š`, `2 -> 1`)

Split a list into a list of slices of a given length.

If the length of the list is not a multiple of the slice length, the last slice is shorter.

If the first argument is a number, it is converted to a range from 0 to that number minus 1.

Fails when the given length is not positive.

__Examples__:

- `[1,2,3,4,5,6] 2Š` → `[[1,2],[3,4],[5,6]]`
- `[1,2,3,4,5,6] 3Š` → `[[1,2,3],[4,5,6]]`
- `[1,2,3,4,5,6] 4Š` → `[[1,2,3,4],[5,6]]`
- `[1,2,3,4,5,6] 8Š` → `[[1,2,3,4,5,6]]`

### `uninterleave` (`ĭ`, `1 -> 2`)

uninterleave a list into a list of elements at even positions and a list of elements at odd positions.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

__Examples__:

- `[1,2,3,4,5,6]ĭÐ` → `[[1,3,5],[2,4,6]]`
- `[1,2,3,4,5]ĭÐ` → `[[1,3,5],[2,4]]`
- `5ĭÐ` → `[[0,2,4],[1,3]]`

### `interleave` (`Ĭ`, `2 -> 1`)

Interleave two lists.

The length of the first list must be either equal to or one more than the length of the second list. Otherwise, this function fails.

__Examples__:

- `[1,3,5] [2,4,6]Ĭ` → `[1,2,3,4,5,6]`
- `[1,3,5] [2,4]Ĭ` → `[1,2,3,4,5]`
- `[2,4] [1,3,5]Ĭ` → Fail

### `minimumBy` (`ṃ`, `2 -> 1`)

Get the minimum value of a list according to a list of keys.

If there are multiple minimums, return any of them non-deterministically.

Fails when the two lists are of different lengths.

This function is non-deterministic.

__Examples__:

- `[1,2,3,4,5] [2,4,5,1,3]ṃ` → `4`
- `[1,2,3,4,5] [1,2,1,2,1]ṃ` → `1 3 5`

### `maximumBy` (`Ṃ`, `2 -> 1`)

Get the maximum value of a list according to a list of keys.

If there are multiple maximums, return any of them non-deterministically.

Fails when the two lists are of different lengths.

This function is non-deterministic.

__Examples__:

- `[1,2,3,4,5] [2,4,5,1,3]Ṃ` → `3`
- `[1,2,3,4,5] [1,2,1,2,1]Ṃ` → `2 4`

### `shortest` (`ş`, `1 -> 1`)

Get the shortest one in a list of lists.

If there are multiple shortest ones, return any of them non-deterministically.

This function is non-deterministic.

__Examples__:

- `[[1,2,3],[4],[5,6]]ş` → `[4]`
- `[[1,2],[3,4],[5],[6]]ş` → `[5] [6]`

### `longest` (`Ş`, `1 -> 1`)

Get the longest one in a list of lists.

If there are multiple longest ones, return any of them non-deterministically.

This function is non-deterministic.

__Examples__:

- `[[1,2,3],[4],[5,6]]Ş` → `[1,2,3]`
- `[[1,2],[3,4],[5],[6]]Ş` → `[1,2] [3,4]`

### `tuple` (`ŧ`, `2 -> 1`)

Create a list with length n, whose elements are taken from another list.

If the first argument is a number, it is converted to a range from 0 to that number minus 1.

This function is non-deterministic, and automatically vectorized on the second argument.

__Examples__:

- `[1,2] 2ŧ` → `[1,1] [1,2] [2,1] [2,2]`
- `2 2ŧ` → `[0,0] [0,1] [1,0] [1,1]`

### `bifurcate` (`ƀ`, `1 -> 2`)

Push the reverse of a list without popping the original list.

If the argument is a number, it is converted to a range from 0 to that number minus 1.

__Examples__:

- `[1,2,3]ƀÐ` → `[[1,2,3],[3,2,1]]`
- `3ƀÐ` → `[[0,1,2],[2,1,0]]`

### `flatten` (`V`, `1 -> 1`)

Flatten a nested list.

If the argument is a number or a char, it is converted to a singleton list.

__Examples__:

- `[[1,2],[3,4]]V` → `[1,2,3,4]`
- `[1,2,3]V` → `[1,2,3]`
- `[1,[2,[3,4]]]V` → `[1,2,3,4]`
- `1V` → `[1]`

### `pad` (`Ḟ`, `1 -> 1`)

Pad a nested list with zeros to make it rectangular.

If the argument is a number or a char, it is unchanged.

__Examples__:

- `[[1,2],[3]]Ḟ` → `[[1,2],[3,0]]`
- `[[[1,2,3],[4,5]],6]Ḟ` → `[[[1,2,3],[4,5,0]],[[6,0,0],[0,0,0]]]`
- `[1,2]Ḟ` → `[1,2]`
- `1Ḟ` → `1`

### `ordering` (`õ`, `1 -> 1`)

Get the ordering of a list.

The n'th element of the result is the index of the n'th element in the sorted list.

__Examples__:

- `[3,1,2]õ` → `[1,2,0]`
- `[1,2,3]õ` → `[0,1,2]`
- `[1,1,2]õ` → `[0,1,2]`

### `elem` (`ē`, `2 -> 1`)

Check if an element is in a list.

If it is, push the element, otherwise fail.

__Examples__:

- `2 [1,2,3]ē` → `2`
- `4 [1,2,3]ē` → Fail
- `'a "abc"ē` → `'a'`

### `filterBy` (`ḟ`, `2 -> 1`)

Filter a list by whether the corresponding element in another list is not failed.

If the first list also contains failed items, those items are also removed.

Fail when the two lists are of different lengths.

__Examples__:

- `[1,2,3,4] [1,0,1,0]Zḟ` → `[1,3]`
- `[1,2,3,4] [1,0,1]Zḟ` → Fail

## Particles

### `onBoth` (`ᵃ`, `(0 -> n) -> (0 -> 2 * n) or (m -> n) -> (m + 1 -> 2 * n) where m > 0`)

Apply a function to the top two values of the stack.

If the function takes no argument, simply apply it twice.

__Examples__:

- `1 2 ᵃ{1+} Ð` → `[2,3]`

### `noPop` (`ˣ`, `(m -> n) -> (m -> m + n)`)

Apply a function without popping the stack.

__Examples__:

- `1 ˣ{1+} Ð` → `[1,2]`

### `dip` (`ᵈ`, `(m -> n) -> (m + 1 -> n + 1)`)

Pop the top value of the stack, apply a function to the rest, and push the popped value back.

__Examples__:

- `1 2 ᵈ{1+} Ð` → `[2,2]`

### `dupDip` (`ᵉ`, `(m -> n) -> (m -> n + 1)`)

Apply a function to the stack, and then push the original top value back.

__Examples__:

- `1 ᵈ{1+} Ð` → `[2,1]`

### `dupDip2` (`ᵋ`, `(m -> n) -> (m -> n + 2)`)

Apply a function to the stack, and then push the original top two values back.

__Examples__:

- `1 2 ᵋ{+} ÐÐ` → `[3,[1,2]]`

### `map` (`ᵐ`, `(0 -> 1) -> (1 -> 1) or (m -> 1) -> (m -> 1) where m > 0`)

Apply a function to each element in a list.

If the input is an number, apply the function to each integer from 0 to the input minus 1.

If the function takes no argument, return a list of n copies of the result of the function, where n is the length of the input.

__Examples__:

- `[1,2,3] ᵐ{1+}` → `[2,3,4]`

### `mapWith` (`ᵚ`, `(1 -> 1) -> (2 -> 1) or (m -> 1) -> (m -> 1) where m > 1`)

Map a binary function over its first argument.

If the function is unary, return a list of n copies of the result of applying the function to the second argument, where n is the length of the first argument.

__Examples__:

- `[1,2,3] 4 ᵚ{+}` → `[5,6,7]`

### `zipWith` (`ᶻ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of elements.

Fail if the lists have different lengths.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

__Examples__:

- `[1,2,3] [4,5,6] ᶻ{+}` → `[5,7,9]`

### `zipWithTrunc` (`ᶾ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of elements.

If the lists have different lengths, truncate the longer list to the length of the shorter list.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

__Examples__:

- `[1,2,3] [4,5,6,7] ᶾ{+}` → `[5,7,9]`

### `outer` (`ᵒ`, `(m -> 1) -> (m -> 1) where m > 1`)

Apply a function to every possible pair of elements in two lists and return a list of lists.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

__Examples__:

- `[1,2,3] [4,5] ᵒ{+}` → `[[5,6],[6,7],[7,8]]`

### `concatMap` (`ʲ`, `(0 -> 1) -> (1 -> 1) or (m -> 1) -> (m -> 1) where m > 0`)

Map a function over a list and concatenate the results.

See the documentation for `concat` and `map`.

__Examples__:

- `[[1,2],[3,4]] ʲ{1+}` → `[2,3,4,5]`

### `unconcatMap` (`ᴶ`, `(0 -> 1) -> (1 -> 1) or (m -> 1) -> (m -> 1) where m > 0`)

Unconcatenate a list, and then map a function over the results.

See the documentation for `unconcat` and `map`.

__Examples__:

- `[1,2,3] ᴶ{1+}` → `[[2],[3],[4]] [[2],[3,4]] [[2,3],[4]] [[2,3,4]]`

### `predicate` (`ᵖ`, `(m -> n) -> (1 -> 1)`)

Check if a function would succeed without actually applying it.

If the function fails, replace the top value with Fail.

Otherwise, do nothing.

__Examples__:

- `1 ᵖ{2<}` → `1`
- `1 ᵖ{2>}` → Fail

### `predicateNot` (`ᵗ`, `(m -> n) -> (1 -> 1)`)

Check if a function would fail without actually applying it.

If the function does not fail, replace the top value with Fail.

Otherwise, do nothing.

__Examples__:

- `1 ᵗ{2<}` → Fail
- `1 ᵗ{2>}` → `1`

### `filter` (`ᶠ`, `(m -> n) -> (1 -> 1)`)

For each element in a list, check if a function would succeed without actually applying it, and remove the element if it fails.

If the input is an number, convert it to a list of integers from 0 to the input minus 1 before filtering.

__Examples__:

- `[1,2,3] ᶠ{2<}` → `[1]`

### `orApply` (`ᶜ`, `(n -> n) -> (n -> n)`)

Apply a function zero or one time non-deterministically.

__Examples__:

- `1 ᶜ{1+}` → `1 2`

### `iterate` (`ᶦ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times non-deterministically, until the top value of the stack is Fail.

This is different from `while` in that it returns the intermediate results.

__Examples__:

- `1 ᶦ{1+}` → `1 2 3 4 5 ...`

### `nTimes` (`ᵑ`, `(n -> n) -> (n + 1 -> n)`)

Take an integer from the top of the stack, and apply a function that many times.

__Examples__:

- `1 3 ᵑ{1+}` → `4`

### `while` (`ʷ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times, until the top value of the stack is Fail.

This is different from `iterate` in that it does not return the intermediate results.

__Examples__:

- `1 ʷ{1+ 4<}` → `3`

### `lengthWhile` (`ˡ`, `(n -> n) -> (n -> 1)`)

Apply a function zero or more times, until the top value of the stack is Fail, and return the number of times the function was applied.

__Examples__:

- `1 ˡ{1+ 4<}` → `2`

### `fixedPoint` (`ʸ`, `(n -> n) -> (n -> n)`)

Apply a function zero or more times, until the top value of the stack no longer changes.

__Examples__:

- `1 ʸ{1+ 4m}` → `4`

### `firstInt` (`ᵏ`, `(m -> n) -> (0 -> 1)`)

Find the smallest non-negative integer for which a function does not fail, and return it.

__Examples__:

- `ᵏ{4>}` → `5`

### `fold1` (`ʳ`, `(m -> 1) -> (m - 1 -> 1) where m > 1`)

Apply a function to the first two elements of a list, then apply it to the result and the third element, and so on until the end of the list.

If the input is an number, convert it to a list of integers from 0 to the input minus 1 before folding.

__Examples__:

- `[1,2,3] ʳ{+}` → `6`

### `onAny` (`ʰ`, `(0 -> 1) -> (1 -> 1) or (m -> 1) -> (m -> 1) where m > 0`)

Apply a function to one element in a list. The element is chosen non-deterministically.

Fail if the list is empty.

If the input is an number, it is converted to a list of integers from 0 to the input minus 1 before applying the function.

__Examples__:

- `[1,2,3] ʰ{1+}` → `[2,2,3] [1,3,3] [1,2,4]`


