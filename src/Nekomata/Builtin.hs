module Nekomata.Builtin (
    Builtin (..),
    BuiltinNotFoundError (..),
    builtins,
    builtinMap,
    builtinShortMap,
    info,
    infoMarkdown,
    infoByName,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Builtin.Basic
import Nekomata.Builtin.List
import Nekomata.Builtin.Math
import Nekomata.Builtin.String
import Nekomata.Function

-- | A builtin function in Nekomata
data Builtin = Builtin
    { name :: String
    , short :: Char
    , func :: Function
    , help :: String
    }

instance Show Builtin where
    show b = "\\" ++ name b

-- | Get the info string for a builtin function
info :: Builtin -> String
info b =
    show b
        ++ " ('"
        ++ [short b]
        ++ "', "
        ++ show (arity (func b))
        ++ "):\n"
        ++ help b

-- | Get the info string for a builtin function in Markdown format
infoMarkdown :: Builtin -> String
infoMarkdown b =
    "### `"
        ++ name b
        ++ "` (`"
        ++ [short b]
        ++ "`, `"
        ++ show (arity (func b))
        ++ "`)\n\n"
        ++ concatMap (++ "\n\n") (lines (help b))

-- | Get the info string for a builtin function by name
infoByName :: String -> Maybe String
infoByName name' =
    info <$> case name' of
        [short'] -> Map.lookup short' builtinShortMap
        '\\' : name'' -> Map.lookup name'' builtinMap
        _ -> Map.lookup name' builtinMap

-- | The list of all builtin functions
builtins :: [Builtin]
builtins =
    [ Builtin
        "choice"
        '?'
        choice
        "Choose between two values.\n\
        \This function is non-deterministic."
    , Builtin
        "fail"
        '!'
        fail'
        "Push a non-deterministic object with no values."
    , Builtin
        "allValues"
        'a'
        allValues
        "Get a list of all possible values for a non-deterministic object."
    , Builtin
        "oneValue"
        '¡'
        oneValue
        "Get the first possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
    , Builtin
        "countValues"
        'n'
        countValues'
        "Count the number of values in a non-deterministic object."
    , Builtin
        "uniqueValue"
        'ũ'
        uniqueValue
        "Remove duplicate values from a non-deterministic object."
    , Builtin
        "minValue"
        'å'
        minValue
        "Get the minimum possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
    , Builtin
        "maxValue"
        'Å'
        maxValue
        "Get the maximum possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
    , Builtin
        "normalForm"
        '¤'
        normalForm'
        "Convert a non-deterministic object to the normal form.\n\
        \I haven't given a formal definition for the normal form. \
        \This function basically lifts all the non-determinism \
        \in lists to the top level."
    , Builtin
        "if"
        'I'
        if'
        "Choose the first value that doesn't fail between two values."
    , Builtin
        "andThen"
        '¿'
        andThen
        "Take two values, \n\
        \and return the first one if the second one doesn't fail."
    , Builtin
        "drop"
        '^'
        drop'
        "Drop the top value of the stack: `a ... -> ...`."
    , Builtin
        "dup"
        ':'
        dup
        "Duplicate the top value of the stack: `a ... -> a a ...`."
    , Builtin
        "swap"
        '$'
        swap
        "Swap the top two values of the stack: `a b ... -> b a ...`."
    , Builtin
        "rot3"
        '§'
        rot3
        "Swap the top two values of the stack: `a b c ... -> c b a ...`."
    , Builtin
        "over"
        'v'
        over
        "Duplicate the second value of the stack, \
        \and put it on top of the stack: `a b ... -> b a b ...`."
    , Builtin
        "eq"
        '='
        eq
        "Check if two values are equal.\n\
        \If they are, push the first value, otherwise fail."
    , Builtin
        "ne"
        '≠'
        ne
        "Check if two values are not equal.\n\
        \If they are not, push the first value, otherwise fail."
    , Builtin
        "lt"
        'Ļ'
        lt
        "Check if the first value is less than the second.\n\
        \If it is, push the first value, otherwise fail.\n\
        \Unlike `less`, this function does not automatically vectorize, \
        \so you can use it to compare two lists."
    , Builtin
        "gt"
        'Ģ'
        gt
        "Check if the first value is greater than the second.\n\
        \If it is, push the first value, otherwise fail.\n\
        \Unlike `greater`, this function does not automatically vectorize, \
        \so you can use it to compare two lists."
    , Builtin
        "isNonempty"
        'N'
        isNonempty
        "Check if a list is non-empty.\n\
        \If it is, push the list itself, otherwise fail."
    , Builtin
        "isNonzero"
        'Z'
        isNonzero
        "Check if a number is non-zero.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isPositive"
        'P'
        isPositive
        "Check if a number is positive.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isNonnegative"
        'ň'
        isNonnegative
        "Check if a number is non-negative.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isZero"
        'ž'
        isZero
        "Check if a number is zero.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isBig"
        'Ƶ'
        isBig
        "Check if the absolute value of a number is greater than 1.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isSmall"
        'ƶ'
        isSmall
        "Check if the absolute value of a number is \n\
        \less than or equal to than 1.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "less"
        '<'
        less
        "Check if the first number is less than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
    , Builtin
        "lessEq"
        '≤'
        lessEq
        "Check if the first number is less than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
    , Builtin
        "greater"
        '>'
        greater
        "Check if the first number is greater than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
    , Builtin
        "greaterEq"
        '≥'
        greaterEq
        "Check if the first number is greater than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
    , Builtin
        "neg1"
        '£'
        neg1
        "The constant -1."
    , Builtin
        "ten"
        '¢'
        ten
        "The constant 10."
    , Builtin
        "octet"
        '¥'
        octet
        "The constant 256."
    , Builtin
        "neg"
        '_'
        neg
        "Negate a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "abs"
        'A'
        abs'
        "Absolute value of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "increment"
        '→'
        increment
        "Increment a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "decrement"
        '←'
        decrement
        "Decrement a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "logicalNot"
        '¬'
        logicalNot
        "Takes a number and returns 1 if it is 0, and 0 otherwise.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "sign"
        '±'
        sign
        "Returns -1 if the argument is negative, 0 if it is zero, \
        \and 1 if it is positive.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "add"
        '+'
        add
        "Add two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "sub"
        '-'
        sub
        "Subtract two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "absDiff"
        '≈'
        absDiff
        "Absolute difference of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "mul"
        '*'
        mul
        "Multiply two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "div"
        '/'
        div'
        "Division of two numbers.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divInt"
        '÷'
        divInt
        "Integer division of two numbers. \
        \Result is rounded towards negative infinity.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "mod"
        '%'
        mod'
        "Modulo two numbers.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divExact"
        '¦'
        divExact
        "Divide two numbers.\n\
        \Fails when the divisor is zero or \
        \the result is not an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divMod"
        'þ'
        divMod'
        "Divide two numbers and return both the quotient and the remainder.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "half"
        '½'
        half
        "Check if an integer is even, and divide it by 2.\n\
        \Fails when the integer is odd.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "pow"
        'E'
        pow
        "Raise a number to a power.\n\
        \The first argument is the exponent, \
        \the second argument is the base.\n\
        \Fails when the exponent is not an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "recip"
        'ŗ'
        recip'
        "Reciprocal of a number.\n\
        \Fails when the number is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "mul2"
        'Ä'
        mul2
        "Multiply a number by 2.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "div2"
        'ä'
        div2
        "Divide a number by 2.\n\
        \This is different from `half` in that \
        \it may return a non-integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "mod2"
        'Ö'
        mod2
        "Modulo a number by 2.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "powOf2"
        'Ë'
        powOf2
        "Raise 2 to a power.\n\
        \Fails when the exponent is not an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "denominator"
        'ḍ'
        denominator'
        "Get the denominator of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "numerator"
        'ṇ'
        numerator'
        "Get the numerator of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "min"
        'm'
        min'
        "Get the minimum of two numbers or two chars.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "max"
        'M'
        max'
        "Get the maximum of two numbers or two chars.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "ceil"
        'K'
        ceil
        "Round a number up to the nearest integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "floor"
        'k'
        floor'
        "Round a number down to the nearest integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "range0"
        'r'
        range0
        "Create a list of integers from 0 to ceil(n)-1.\n\
        \This function is automatically vectorized."
    , Builtin
        "range1"
        'R'
        range1
        "Create a list of integers from 1 to n.\n\
        \This function is automatically vectorized."
    , Builtin
        "interval"
        'ï'
        interval
        "Create a list of integers from ceil(x) to floor(y).\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "natural"
        'Ň'
        natural
        "Non-deterministically choose a natural number.\n\
        \This function is non-deterministic."
    , Builtin
        "integer"
        'Ž'
        integer
        "Non-deterministically choose an integer.\n\
        \This function is non-deterministic."
    , Builtin
        "sum"
        '∑'
        sum'
        "Take the sum of a list of numbers.\n\
        \The addition is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "product"
        '∏'
        product'
        "Take the product of a list of numbers.\n\
        \The multiplication is automatically vectorized \
        \and fails when the two lists are of different lengths.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "dot"
        '∙'
        dot
        "Take the dot product of two lists of numbers.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \The current implementation is simply a composition of \
        \mul and sum."
    , Builtin
        "convolve"
        '×'
        convolve
        "Take the convolution of two lists of numbers.\n\
        \If one of the arguments is a number, \
        \it simply multiplies the other argument by that number.\n\
        \If the arguments are nested lists, \
        \it takes the multi-dimensional convolution.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "mean"
        'µ'
        mean
        "Take the mean of a list of numbers.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "fromBase"
        'b'
        fromBase
        "Convert a list of digits to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
        \If the base is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized over the base."
    , Builtin
        "fromBaseRev"
        'd'
        fromBaseRev
        "Convert a list of digits in reverse order to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
        \If the base is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized over the base."
    , Builtin
        "toBase"
        'D'
        toBase
        "Convert an integer to a list of digits.\n\
        \The first argument is the integer, \
        \the second argument is the base.\n\
        \Fails when the inputs are not integers, \
        \or the base is less than 2.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized over both arguments. \
        \If both arguments are lists, \
        \the result is a list of lists of digits."
    , Builtin
        "toBaseRev"
        'B'
        toBaseRev
        "Convert an integer to a list of digits in reverse order.\n\
        \The first argument is the integer, \
        \the second argument is the base.\n\
        \Fails when the inputs are not integers, \
        \or the base is less than 2.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized over both arguments. \
        \If both arguments are lists, \
        \the result is a list of lists of digits."
    , Builtin
        "binary"
        'Ƃ'
        binary'
        "Convert an integer to a list of binary digits in reverse order.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "fromBinary"
        'ƃ'
        fromBinary
        "Convert a list of binary digits in reverse order to an integer.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "digits"
        'Ɗ'
        digits
        "Convert an integer to a list of decimal digits.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "fromDigits"
        'ɗ'
        fromDigits
        "Convert a list of decimal digits to an integer.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "cumsum"
        '∫'
        cumsum
        "Take the cumulative sum of a list of numbers.\n\
        \The addition is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "delta"
        '∆'
        delta
        "Take the difference between adjacent elements of a list of numbers.\n\
        \The subtraction is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
    , Builtin
        "binomial"
        'Ç'
        binomial
        "Compute the binomial coefficient.\n\
        \The second argument must be an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "factorial"
        'F'
        factorial
        "Compute the factorial of an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "isPrime"
        'Q'
        isPrime'
        "Check if an integer is prime.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "prime"
        'Ƥ'
        prime
        "Non-deterministically choose a prime number.\n\
        \This function is non-deterministic."
    , Builtin
        "primePi"
        'ƥ'
        primePi
        "Compute the number of primes less than or equal to an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "factor"
        'ƒ'
        factor
        "Factorize a rational number, \
        \and return a list of prime factors and a list of exponents.\n\
        \Fails when the input is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "gcd"
        'G'
        gcd'
        "Compute the greatest common divisor of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "lcm"
        'g'
        lcm'
        "Compute the least common multiple of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divisors"
        'Ď'
        divisors
        "Compute the list of divisors of an integer.\n\
        \Fail when the input is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "intPartition"
        'Ṗ'
        intPartition
        "Partition an integer into a list of integers, \
        \whose sum is the original integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is non-deterministic and automatically vectorized."
    , Builtin
        "sqrt"
        '√'
        sqrt'
        "Compute the square root of a rational number.\n\
        \Fails when the argument is not a perfect square.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "unitVec2"
        'į'
        unitVec2
        "Choose one of [0, 1] and [1, 0] non-deterministically.\n\
        \This function is non-deterministic."
    , Builtin
        "orNeg"
        'ŋ'
        orNeg
        "Optionally negate a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is non-deterministic and automatically vectorized.\n\
        \When the input is a list, \
        \each element is optionally negated independently."
    , Builtin
        "bitAnd"
        '&'
        bitAnd
        "Bitwise AND of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "bitOr"
        '|'
        bitOr
        "Bitwise OR of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "bitXor"
        'X'
        bitXor
        "Bitwise XOR of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "popCount"
        'Þ'
        popCount'
        "Count the number of 1s in the binary digits of an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "histogram"
        'Ħ'
        histogram
        "Compute the histogram of a list of integers.\n\
        \The result is a list, \
        \whose length is the maximum of the input list, \
        \and whose nth element is the number of occurrences \
        \of n in the input.\n\
        \If the input is a ragged list, \
        \it is flattened before computation.\n\
        \If the input is a single integer, \
        \it is treated as a singleton list.\n\
        \If the input is a single char, \
        \it is converted to a number according to Nekomata's code page, \
        \and then treated as a singleton list."
    , Builtin
        "charToInt"
        'e'
        charToInt'
        "Convert a char to an integer according to Nekomata's code page.\n\
        \This function is automatically vectorized."
    , Builtin
        "intToChar"
        'H'
        intToChar'
        "Convert an integer to a char according to Nekomata's code page.\n\
        \Fail when the integer is not in the range 0 to 255.\
        \This function is automatically vectorized."
    , Builtin
        "read"
        'Ĝ'
        read'
        "Parse a string (a list of chars) or a single char \
        \as a Nekomata value.\n\
        \Fail when the string is not a valid Nekomata value."
    , Builtin
        "show"
        'ĝ'
        show'
        "Convert a Nekomata value to a string (a list of chars)."
    , Builtin
        "anyOf"
        '~'
        anyOf'
        "Choose an element from a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "emptyList"
        'Ø'
        emptyList
        "Push an empty list."
    , Builtin
        "singleton"
        'U'
        singleton'
        "Create a list with a single element."
    , Builtin
        "unsingleton"
        'z'
        unsingleton
        "Get the only element of a list with a single element.\n\
        \Fails when the list is empty or has more than one element."
    , Builtin
        "pair"
        'Ð'
        pair
        "Create a list with two elements."
    , Builtin
        "unpair"
        'đ'
        unpair
        "Get the two elements of a list with two elements.\n\
        \Fails when the length of the list is not 2."
    , Builtin
        "removeFail"
        '‼'
        removeFail
        "Remove failed items from a list."
    , Builtin
        "length"
        '#'
        length'
        "Get the length of a list."
    , Builtin
        "lengthIs"
        'L'
        lengthIs
        "Check if the length of a list is equal to a given integer.\n\
        \If it is, push the list itself, otherwise fail."
    , Builtin
        "nth"
        '@'
        nth
        "Get the nth element of a list.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "head"
        'h'
        head'
        "Get the first element of a list."
    , Builtin
        "tail"
        't'
        tail'
        "Remove the first element of a list."
    , Builtin
        "cons"
        'c'
        cons
        "Prepend an element to a list."
    , Builtin
        "uncons"
        'C'
        uncons
        "Get the first element and the rest of a list."
    , Builtin
        "last"
        'l'
        last'
        "Get the last element of a list."
    , Builtin
        "init"
        'i'
        init'
        "Remove the last element of a list."
    , Builtin
        "snoc"
        'ɔ'
        snoc
        "Append an element to a list."
    , Builtin
        "unsnoc"
        'Ɔ'
        unsnoc
        "Get the last element and the rest of a list."
    , Builtin
        "cons0"
        'ç'
        cons0
        "Prepend a zero to a list."
    , Builtin
        "reverse"
        '↔'
        reverse'
        "Reverse a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1."
    , Builtin
        "prefix"
        'p'
        prefix
        "Get a prefix of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "suffix"
        's'
        suffix
        "Get a suffix of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "take"
        'T'
        take'
        "Get the first n elements of a list.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "subset"
        'S'
        subset
        "Get a finite subset of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "subsequence"
        'q'
        subsequence
        "Get a finite contiguous subsequence of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "join"
        ','
        join'
        "Concatenate two lists.\n\
        \If one of the arguments is a number or a char, \
        \it is converted to a singleton list before concatenation."
    , Builtin
        "split"
        ';'
        split
        "Split a list into two parts.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "replicate"
        'ř'
        replicate'
        "Create a list with n copies of an element.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "minimum"
        'ṁ'
        minimum'
        "Get the minimum of a list.\n\
        \If there are multiple minimums, return the first one.\n\
        \Fail when the list is empty.\n\
        \The order used in this function is different from the one \
        \used in min and max. It can compare two arbitrary values, \
        \not just numbers or chars."
    , Builtin
        "maximum"
        'Ṁ'
        maximum'
        "Get the maximum of a list.\n\
        \If there are multiple maximums, return the first one.\n\
        \Fail when the list is empty.\n\
        \The order used in this function is different from the one \
        \used in min and max. It can compare two arbitrary values, \
        \not just numbers or chars."
    , Builtin
        "minMax"
        'ɱ'
        minMax
        "Get both the minimum and the maximum of a list.\n\
        \If there are multiple minimums or maximums, return the first one.\n\
        \Fail when the list is empty.\n\
        \The order used in this function is different from the one \
        \used in min and max. It can compare two arbitrary values, \
        \not just numbers or chars."
    , Builtin
        "concat"
        'j'
        concat'
        "Concatenate a list of lists or a list.\n\
        \If one item in the list is a number or a char, \
        \it is converted to a singleton list before concatenation."
    , Builtin
        "unconcat"
        'J'
        unconcat
        "Split a list into a list of lists.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "nub"
        'u'
        nub
        "Remove duplicate elements from a list."
    , Builtin
        "sort"
        'o'
        sort
        "Sort a list."
    , Builtin
        "permutation"
        '↕'
        permutation
        "Get a permutation of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "extract"
        'ĕ'
        extract
        "Extract an element from a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \Returns the element and the rest of the list.\n\
        \This function is non-deterministic."
    , Builtin
        "allEqual"
        '≡'
        allEqual
        "Check if all elements in a list are equal.\n\
        \If it is, push the equal element, otherwise fail.\n\
        \If the list is empty, this function fails."
    , Builtin
        "isUnique"
        'ů'
        isUnique
        "Check if all elements in a list are unique.\n\
        \If it is, push the list itself, otherwise fail.\n\
        \The empty list is considered unique."
    , Builtin
        "free"
        'f'
        free
        "Check if a list is free of a given element.\n\
        \This means that the list is not equal to the element, \
        \and recursively, every item of the list if free of that element.\n\
        \If it is, push the list itself, otherwise fail."
    , Builtin
        "enumerate"
        'x'
        enumerate
        "Push a list of integers from 0 to the length of the argument minus 1 \
        \without popping the original argument."
    , Builtin
        "rotate"
        'Ř'
        rotate
        "Rotate a list by a given number of positions.\n\
        \If the first argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "transpose"
        'Ť'
        transpose
        "Transpose a list of lists.\n\
        \Fail if the sublists are not all of the same length."
    , Builtin
        "setPartition"
        'O'
        setPartition
        "Partition a list into a list of lists such that their concatenation \
        \is a permutation of the original list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
    , Builtin
        "setMinus"
        '∕'
        setMinus
        "For each element in the second list, \
        \remove the first occurrence of that element in the first list."
    , Builtin
        "index"
        'Ĩ'
        index
        "Get the index of any occurrence of an element in a list.\n\
        \Fail if the element does not occur in the list.\n\
        \This function is non-deterministic."
    , Builtin
        "count"
        'Ĉ'
        count
        "Count the number of occurrences of an element in a list."
    , Builtin
        "tally"
        'Ţ'
        tally
        "Count the number of occurrences of each element in a list.\n\
        \Return a list of elements and a list of counts in the same order."
    , Builtin
        "intersect"
        '∩'
        intersect
        "Get the intersection of two lists."
    , Builtin
        "union"
        'Ŭ'
        union
        "Get the union of two lists."
    , Builtin
        "chunks"
        'ĉ'
        chunks
        "Split a list into a list of chunks of equal elements."
    , Builtin
        "rle"
        'Y'
        rle
        "Run-length encode a list.\n\
        \Returns a list of elements and a list of lengths."
    , Builtin
        "unrle"
        'y'
        unrle
        "Run-length decode a list.\n\
        \The first argument is a list of elements, \
        \the second argument is a list of lengths.\n\
        \Fails when the two lists are of different lengths."
    , Builtin
        "slices"
        'Š'
        slices
        "Split a list into a list of slices of a given length.\n\
        \If the length of the list is not a multiple of the slice length, \
        \the last slice is shorter.\n\
        \If the first argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \Fails when the given length is not positive."
    , Builtin
        "uninterleave"
        'ĭ'
        uninterleave
        "uninterleave a list into a list of elements \
        \at even positions and a list of elements at odd positions.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1."
    , Builtin
        "interleave"
        'Ĭ'
        interleave
        "Interleave two lists.\n\
        \The length of the first list must be either equal to or one more than \
        \the length of the second list. Otherwise, this function fails."
    , Builtin
        "minimumBy"
        'ṃ'
        minimumBy
        "Get the minimum value of a list according to a list of keys.\n\
        \If there are multiple minimums, \
        \return any of them non-deterministically.\n\
        \Fails when the two lists are of different lengths.\n\
        \This function is non-deterministic."
    , Builtin
        "maximumBy"
        'Ṃ'
        maximumBy
        "Get the maximum value of a list according to a list of keys.\n\
        \If there are multiple maximums, \
        \return any of them non-deterministically.\n\
        \Fails when the two lists are of different lengths.\n\
        \This function is non-deterministic."
    , Builtin
        "shortest"
        'ş'
        shortest
        "Get the shortest one in a list of lists.\n\
        \If there are multiple shortest ones, \
        \return any of them non-deterministically.\n\
        \This function is non-deterministic."
    , Builtin
        "longest"
        'Ş'
        longest
        "Get the longest one in a list of lists.\n\
        \If there are multiple longest ones, \
        \return any of them non-deterministically.\n\
        \This function is non-deterministic."
    , Builtin
        "tuple"
        'ŧ'
        tuple
        "Create a list with length n, \
        \whose elements are taken from another list.\n\
        \This function is non-deterministic, \
        \and automatically vectorized on the second argument."
    , Builtin
        "bifurcate"
        'ƀ'
        bifurcate
        "Push the reverse of a list without popping the original list."
    , Builtin
        "flatten"
        'V'
        flatten
        "Flatten a nested list.\n\
        \If the argument is a number or a char, \
        \it is converted to a singleton list."
    , Builtin
        "pad"
        'Ḟ'
        pad
        "Pad a nested list with zeros to make it rectangular.\n\
        \If the argument is a number or a char, it is unchanged."
    , Builtin
        "ordering"
        'õ'
        ordering
        "Get the ordering of a list.\n\
        \The n'th element of the result is the index of the n'th element \
        \in the sorted list."
    , Builtin
        "elem"
        'ē'
        elem'
        "Check if an element is in a list.\n\
        \If it is, push the element, otherwise fail."
    , Builtin
        "filterBy"
        'ḟ'
        filterBy
        "Filter a list by whether the corresponding element in another list \
        \is not failed.\n\
        \If the first list also contains failed items, \
        \those items are also removed.\n\
        \Fail when the two lists are of different lengths."
    ]

-- | The map from names to builtin functions
builtinMap :: Map String Builtin
builtinMap = Map.fromList [(name b, b) | b <- builtins]

-- | The map from short names to builtin functions
builtinShortMap :: Map Char Builtin
builtinShortMap = Map.fromList [(short b, b) | b <- builtins]

-- | An error that occurs when a builtin function is not found
data BuiltinNotFoundError = BuiltinNotFound String | BuiltinShortNotFound Char
    deriving (Eq)

instance Show BuiltinNotFoundError where
    show (BuiltinNotFound name') =
        "cannot find builtin function with full name \"\\" ++ name' ++ "\""
    show (BuiltinShortNotFound short') =
        "cannot find builtin function with short name '" ++ [short'] ++ "'"
