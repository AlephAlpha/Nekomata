# Built-ins Functions and Particles

Nekomata is still in an early stage. The full names, short names, and meanings of built-in functions and particles are subject to change.

## Functions

### `choice` (`?`, `2 -> 1`)

Choose between two values.

This function is non-deterministic.

__Examples__:

- `1 2?` → `1 2`

### `fail` (`!`, `0 -> 1`)

Push a non-deterministic object with no values.

### `allValues` (`a`, `1 -> 1`)

Get a list of all possible values for a non-deterministic object.

__Examples__:

- `1 2?a` → `[1,2]`

### `oneValue` (`¡`, `1 -> 1`)

Get the first possible value from a non-deterministic object.

Fails if the object has no values.

__Examples__:

- `1 2?¡` → `1`

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

### `normalForm` (`¤`, `1 -> 1`)

Convert a non-deterministic object to the normal form.

I haven't given a formal definition for the normal form. This function basically lifts all the non-determinism in lists to the top level.

### `if` (`I`, `2 -> 1`)

Choose the first value that doesn't fail between two values.

__Examples__:

- `1 2I` → `1`
- `! 2I` → `2`

### `andThen` (`¿`, `2 -> 1`)

Take two values, 

and return the first one if the second one doesn't fail.

__Examples__:

- `1 2¿` → `1`
- `1 !¿` → ``

### `drop` (`^`, `1 -> 0`)

Drop the top value of the stack: `a ... -> ...`.

### `dup` (`:`, `1 -> 2`)

Duplicate the top value of the stack: `a ... -> a a ...`.

### `swap` (`$`, `2 -> 2`)

Swap the top two values of the stack: `a b ... -> b a ...`.

### `rot3` (`§`, `3 -> 3`)

Swap the top two values of the stack: `a b c ... -> c b a ...`.

### `over` (`v`, `2 -> 3`)

Duplicate the second value of the stack, and put it on top of the stack: `a b ... -> b a b ...`.

### `eq` (`=`, `2 -> 1`)

Check if two values are equal.

If they are, push the first value, otherwise fail.

__Examples__:

- `1 1=` → `1`
- `1 2=` → ``

### `ne` (`≠`, `2 -> 1`)

Check if two values are not equal.

If they are not, push the first value, otherwise fail.

__Examples__:

- `1 1≠` → ``
- `1 2≠` → `1`

### `lt` (`Ļ`, `2 -> 1`)

Check if the first value is less than the second.

If it is, push the first value, otherwise fail.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `1 2Ļ` → `1`
- `1 1Ļ` → ``
- `2 1Ļ` → ``
- `1 'aĻ` → `1`
- `'a [1]Ļ` → `'a'`
- `[1,2] [2]Ļ` → `[1,2]`
- `[1,2] [1]Ļ` → ``

### `gt` (`Ģ`, `2 -> 1`)

Check if the first value is greater than the second.

If it is, push the first value, otherwise fail.

This function uses an ordering that is defined on all values. Numbers are smaller than chars, which are smaller than lists. Lists are compared in the lexicographic order.

__Examples__:

- `1 2Ģ` → ``
- `1 1Ģ` → ``
- `2 1Ģ` → `2`
- `'a 1Ģ` → `'a'`
- `[1] 'aĢ` → `[1]`
- `[1,2] [2]Ģ` → ``
- `[1,2] [1]Ģ` → `[1,2]`

### `isNonempty` (`N`, `1 -> 1`)

Check if a list is non-empty.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1]N` → `[1]`
- `"Hello"N` → `Hello`
- `[]N` → ``

### `isLong` (`Ł`, `1 -> 1`)

Check if the length of a list is greater than 1.

If it is, push the list itself, otherwise fail.

__Examples__:

- `[1,2]Ł` → `[1,2]`
- `[1]Ł` → ``
- `[]Ł` → ``

### `isNonzero` (`Z`, `1 -> 1`)

Check if a number is non-zero.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1Z` → `1`
- `0Z` → ``
- `1_Z` → `-1`
- `[1,[2,3]]Z` → `[1,[2,3]]`

### `isPositive` (`P`, `1 -> 1`)

Check if a number is positive.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1P` → `1`
- `0P` → ``
- `1_P` → ``
- `[1,[2,3]]P` → `[1,[2,3]]`

### `isNonnegative` (`ň`, `1 -> 1`)

Check if a number is non-negative.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1ň` → `1`
- `0ň` → `0`
- `1_ň` → ``
- `[1,[2,3]]ň` → `[1,[2,3]]`

### `isZero` (`ž`, `1 -> 1`)

Check if a number is zero.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `1ž` → ``
- `0ž` → `0`
- `1_ž` → ``
- `[0,[0,0]]ž` → `[0,[0,0]]`

### `isBig` (`Ƶ`, `1 -> 1`)

Check if the absolute value of a number is greater than 1.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2Ƶ` → `2`
- `1Ƶ` → ``
- `0Ƶ` → ``
- `1_Ƶ` → ``
- `2_Ƶ` → `-2`

### `isSmall` (`ƶ`, `1 -> 1`)

Check if the absolute value of a number is 

less than or equal to than 1.

If it is, push the number itself, otherwise fail.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2ƶ` → ``
- `1ƶ` → `1`
- `0ƶ` → `0`
- `1_ƶ` → `-1`
- `2_ƶ` → ``

### `less` (`<`, `2 -> 1`)

Check if the first number is less than the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2<` → `1`
- `1 1<` → ``
- `2 1<` → ``
- `[1,2,3] [2,3,4]<` → `[1,2,3]`
- `[1,2] [2,1]<` → ``

### `lessEq` (`≤`, `2 -> 1`)

Check if the first number is less than or equal to the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2≤` → `1`
- `1 1≤` → `1`
- `2 1≤` → ``
- `[1,2,3] [2,3,4]≤` → `[1,2,3]`
- `[1,2] [2,1]≤` → ``

### `greater` (`>`, `2 -> 1`)

Check if the first number is greater than the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2>` → ``
- `1 1>` → ``
- `2 1>` → `2`
- `[2,3,4] [1,2,3]>` → `[2,3,4]`
- `[2,1] [1,2]>` → ``

### `greaterEq` (`≥`, `2 -> 1`)

Check if the first number is greater than or equal to the second.

If it is, push the first number, otherwise fail.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page before comparison, but the result is still a char.

This function is automatically vectorized.

__Examples__:

- `1 2≥` → ``
- `1 1≥` → `1`
- `2 1≥` → `2`
- `[2,3,4] [1,2,3]≥` → `[2,3,4]`
- `[2,1] [1,2]≥` → ``

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
- `[2] [3,4]*` → ``

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
- `[3] [2,3]/` → ``

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
- `[3] [-2,3]÷` → ``

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
- `[5] [3,4]%` → ``

### `divExact` (`¦`, `2 -> 1`)

Divide two numbers.

Fails when the divisor is zero or the result is not an integer.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

__Examples__:

- `6 3¦` → `2`
- `5 3¦` → ``
- `[6,4] [3,4]¦` → `[2,1]`
- `6 [2,3]¦` → `[3,2]`
- `[6] [2,3]¦` → ``

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
- `5½` → ``
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
- `1\2 2E` → ``
- `[-2,0,2] 2E` → `[1/4,1,4]`
- `[2] [3,4]E` → ``

### `recip` (`ŗ`, `1 -> 1`)

Reciprocal of a number.

Fails when the number is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

__Examples__:

- `2ŗ` → `1/2`
- `0ŗ` → ``
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
- `[3] [5,7]ï` → ``

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

### `product` (`∏`, `1 -> 1`)

Take the product of a list of numbers.

The multiplication is automatically vectorized and fails when the two lists are of different lengths.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `dot` (`∙`, `2 -> 1`)

Take the dot product of two lists of numbers.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

The current implementation is simply a composition of mul and sum.

### `convolve` (`×`, `2 -> 1`)

Take the convolution of two lists of numbers.

If one of the arguments is a number, it simply multiplies the other argument by that number.

If the arguments are nested lists, it takes the multi-dimensional convolution.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `mean` (`µ`, `1 -> 1`)

Take the mean of a list of numbers.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `fromBase` (`b`, `2 -> 1`)

Convert a list of digits to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

If the base is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized over the base.

### `fromBaseRev` (`d`, `2 -> 1`)

Convert a list of digits in reverse order to a number.

The first argument is the list of digits, the second argument is the base.

This does not require the digits and the base to be integers.

If the base is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized over the base.

### `toBase` (`D`, `2 -> 1`)

Convert an integer to a list of digits.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

### `toBaseRev` (`B`, `2 -> 1`)

Convert an integer to a list of digits in reverse order.

The first argument is the integer, the second argument is the base.

Fails when the inputs are not integers, or the base is less than 2.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized over both arguments. If both arguments are lists, the result is a list of lists of digits.

### `binary` (`Ƃ`, `1 -> 1`)

Convert an integer to a list of binary digits in reverse order.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `fromBinary` (`ƃ`, `1 -> 1`)

Convert a list of binary digits in reverse order to an integer.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `digits` (`Ɗ`, `1 -> 1`)

Convert an integer to a list of decimal digits.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `fromDigits` (`ɗ`, `1 -> 1`)

Convert a list of decimal digits to an integer.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `cumsum` (`∫`, `1 -> 1`)

Take the cumulative sum of a list of numbers.

The addition is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `delta` (`∆`, `1 -> 1`)

Take the difference between adjacent elements of a list of numbers.

The subtraction is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `binomial` (`Ç`, `2 -> 1`)

Compute the binomial coefficient.

The second argument must be an integer.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `factorial` (`F`, `1 -> 1`)

Compute the factorial of an integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `isPrime` (`Q`, `1 -> 1`)

Check if an integer is prime.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `prime` (`Ƥ`, `0 -> 1`)

Non-deterministically choose a prime number.

This function is non-deterministic.

### `primePi` (`ƥ`, `1 -> 1`)

Compute the number of primes less than or equal to an integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `factor` (`ƒ`, `1 -> 2`)

Factorize a rational number, and return a list of prime factors and a list of exponents.

Fails when the input is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `gcd` (`G`, `2 -> 1`)

Compute the greatest common divisor of two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `lcm` (`g`, `2 -> 1`)

Compute the least common multiple of two numbers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `divisors` (`Ď`, `1 -> 1`)

Compute the list of divisors of an integer.

Fail when the input is zero.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `intPartition` (`Ṗ`, `1 -> 1`)

Partition an integer into a list of integers, whose sum is the original integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is non-deterministic and automatically vectorized.

### `sqrt` (`√`, `1 -> 1`)

Compute the square root of a rational number.

Fails when the argument is not a perfect square.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `unitVec2` (`į`, `0 -> 1`)

Choose one of [0, 1] and [1, 0] non-deterministically.

This function is non-deterministic.

### `orNeg` (`ŋ`, `1 -> 1`)

Optionally negate a number.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is non-deterministic and automatically vectorized.

When the input is a list, each element is optionally negated independently.

### `bitAnd` (`&`, `2 -> 1`)

Bitwise AND of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized and fails when the two lists are of different lengths.

### `bitOr` (`|`, `2 -> 1`)

Bitwise OR of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding.

### `bitXor` (`X`, `2 -> 1`)

Bitwise XOR of two integers.

If one or both of the arguments are chars, they are converted to numbers according to Nekomata's code page.

This function is automatically vectorized with padding.

### `popCount` (`Þ`, `1 -> 1`)

Count the number of 1s in the binary digits of an integer.

If the argument is a char, it is converted to a number according to Nekomata's code page.

This function is automatically vectorized.

### `histogram` (`Ħ`, `1 -> 1`)

Compute the histogram of a list of integers.

The result is a list, whose length is the maximum of the input list, and whose nth element is the number of occurrences of n in the input.

If the input is a ragged list, it is flattened before computation.

If the input is a single integer, it is treated as a singleton list.

If the input is a single char, it is converted to a number according to Nekomata's code page, and then treated as a singleton list.

### `sumEach` (`Ŝ`, `1 -> 1`)

Take the sum of each list in a list of lists of numbers.

The addition is automatically vectorized with padding zeros.

If some of the elements are chars, they are converted to numbers according to Nekomata's code page.

### `charToInt` (`e`, `1 -> 1`)

Convert a char to an integer according to Nekomata's code page.

If the input is already an integer, it is left unchanged.

This function is automatically vectorized.

### `intToChar` (`H`, `1 -> 1`)

Convert an integer to a char according to Nekomata's code page.

If the input is already a char, it is left unchanged.

Fail when the integer is not in the range 0 to 255.

This function is automatically vectorized.

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

### `minMax` (`ɱ`, `1 -> 2`)

Get both the minimum and the maximum of a list.

If there are multiple minimums or maximums, return the first one.

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

Push a list of integers from 0 to the length of the argument minus 1 without popping the original argument.

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

### `rle` (`Y`, `1 -> 2`)

Run-length encode a list.

Returns a list of elements and a list of lengths.

### `unrle` (`y`, `2 -> 1`)

Run-length decode a list.

The first argument is a list of elements, the second argument is a list of lengths.

Fails when the two lists are of different lengths.

### `slices` (`Š`, `2 -> 1`)

Split a list into a list of slices of a given length.

If the length of the list is not a multiple of the slice length, the last slice is shorter.

If the first argument is a number, it is converted to a range from 0 to that number minus 1.

Fails when the given length is not positive.

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

### `bifurcate` (`ƀ`, `1 -> 2`)

Push the reverse of a list without popping the original list.

### `flatten` (`V`, `1 -> 1`)

Flatten a nested list.

If the argument is a number or a char, it is converted to a singleton list.

### `pad` (`Ḟ`, `1 -> 1`)

Pad a nested list with zeros to make it rectangular.

If the argument is a number or a char, it is unchanged.

### `ordering` (`õ`, `1 -> 1`)

Get the ordering of a list.

The n'th element of the result is the index of the n'th element in the sorted list.

### `elem` (`ē`, `2 -> 1`)

Check if an element is in a list.

If it is, push the element, otherwise fail.

### `filterBy` (`ḟ`, `2 -> 1`)

Filter a list by whether the corresponding element in another list is not failed.

If the first list also contains failed items, those items are also removed.

Fail when the two lists are of different lengths.

## Particles

### `onBoth` (`ᵃ`, `(0 -> n) -> (0 -> 2 * n) or (m -> n) -> (m + 1 -> 2 * n) where m > 0`)

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

If the input is an number, apply the function to each integer from 0 to the input minus 1.

If the function takes no argument, return a list of n copies of the result of the function, where n is the length of the input.

### `mapWith` (`ᵚ`, `(1 -> 1) -> (2 -> 1) or (m -> 1) -> (m -> 1) where m > 1`)

Map a binary function over its first argument.

If the function is unary, return a list of n copies of the result of applying the function to the second argument, where n is the length of the first argument.

### `zipWith` (`ᶻ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of values.

Fail if the lists have different lengths.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

### `zipWithTrunc` (`ᶾ`, `(m -> 1) -> (m -> 1) where m > 1`)

Zip two lists and apply a function to each pair of values.

If the lists have different lengths, truncate the longer list to the length of the shorter list.

If one of the input is an number, apply the function to each integer from 0 to the input minus 1.

### `outer` (`ᵒ`, `(m -> 1) -> (m -> 1) where m > 1`)

Apply a function to every possible pair of values in two lists and return a list of lists.

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

If the input is an number, convert it to a list of integers from 0 to the input minus 1 before folding.


