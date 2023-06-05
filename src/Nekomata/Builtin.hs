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
        "Remove duplicate values from a non-deterministic object.\n\
        \This function is non-deterministic."
    , Builtin
        "normalForm"
        '¤'
        normalForm'
        "Convert a non-deterministic object to the normal form.\n\
        \I haven't given a formal definition for the normal form. \
        \This function basically lifts all the non-determinism \
        \in lists and strings to the top level."
    , Builtin
        "if"
        'I'
        if'
        "Choose the first value that doesn't fail between two values."
    , Builtin "drop" '^' drop' "Drop the top value of the stack."
    , Builtin "dup" ':' dup "Duplicate the top value of the stack."
    , Builtin "swap" '$' swap "Swap the top two values of the stack."
    , Builtin
        "rot3"
        '§'
        rot3
        "Rotate the top three values of the stack."
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
        "nonempty"
        'N'
        nonempty'
        "Check if a list or string is non-empty.\n\
        \If it is, push the list or string itself, otherwise fail."
    , Builtin
        "nonzero"
        'Z'
        nonzero
        "Check if a number is non-zero.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \This function is automatically vectorized."
    , Builtin
        "isPositive"
        'P'
        isPositive
        "Check if a number is positive.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \This function is automatically vectorized."
    , Builtin
        "less"
        '<'
        less
        "Check if the first number is less than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \This function is automatically vectorized."
    , Builtin
        "lessEq"
        '≤'
        lessEq
        "Check if the first number is less than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \This function is automatically vectorized."
    , Builtin
        "greater"
        '>'
        greater
        "Check if the first number is greater than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \This function is automatically vectorized."
    , Builtin
        "greaterEq"
        '≥'
        greaterEq
        "Check if the first number is greater than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
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
        \This function is automatically vectorized."
    , Builtin
        "abs"
        'A'
        abs'
        "Absolute value of a number.\n\
        \This function is automatically vectorized."
    , Builtin
        "increment"
        '→'
        increment
        "Increment a number.\n\
        \This function is automatically vectorized."
    , Builtin
        "decrement"
        '←'
        decrement
        "Decrement a number.\n\
        \This function is automatically vectorized."
    , Builtin
        "logicalNot"
        '¬'
        logicalNot
        "Takes a number and returns 1 if it is 0, and 0 otherwise.\n\
        \This function is automatically vectorized."
    , Builtin
        "sign"
        '±'
        sign
        "Returns -1 if the argument is negative, 0 if it is zero, \
        \and 1 if it is positive.\n\
        \This function is automatically vectorized."
    , Builtin
        "add"
        '+'
        add
        "Add two numbers.\n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "sub"
        '-'
        sub
        "Subtract two numbers.\n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "mul"
        '*'
        mul
        "Multiply two numbers.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "div"
        '/'
        div'
        "Division of two numbers.\n\
        \Fails when the divisor is zero.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divInt"
        '÷'
        divInt
        "Integer division of two numbers. \
        \Result is rounded towards negative infinity.\n\
        \Fails when the divisor is zero.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "mod"
        '%'
        mod'
        "Modulo two numbers.\n\
        \Fails when the divisor is zero.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divExact"
        '¦'
        divExact
        "Divide two numbers.\n\
        \Fails when the divisor is zero or \
        \the result is not an integer.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "half"
        '½'
        half
        "Divide an integer by two.\n\
        \Fails when the number is odd.\n\
        \This function is automatically vectorized."
    , Builtin
        "pow"
        'E'
        pow
        "Raise a number to a power.\n\
        \Fails when the exponent is not an integer.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "recip"
        'ŗ'
        recip'
        "Reciprocal of a number.\n\
        \Fails when the number is zero.\n\
        \This function is automatically vectorized."
    , Builtin
        "denominator"
        'ḍ'
        denominator'
        "Get the denominator of a number.\n\
        \This function is automatically vectorized."
    , Builtin
        "numerator"
        'ṇ'
        numerator'
        "Get the numerator of a number.\n\
        \This function is automatically vectorized."
    , Builtin
        "min"
        'm'
        min'
        "Get the minimum of two numbers or two strings.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "max"
        'M'
        max'
        "Get the maximum of two numbers or two strings.\n\
        \This function is automatically vectorized with padding."
    , Builtin
        "ceil"
        'K'
        ceil
        "Round a number up to the nearest integer.\n\
        \This function is automatically vectorized."
    , Builtin
        "floor"
        'k'
        floor'
        "Round a number down to the nearest integer.\n\
        \This function is automatically vectorized."
    , Builtin
        "range0"
        'r'
        range0
        "Create a list of integers from 0 to n-1.\n\
        \This function is automatically vectorized."
    , Builtin
        "range1"
        'R'
        range1
        "Create a list of integers from 1 to n.\n\
        \This function is automatically vectorized."
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
        \The addition is automatically vectorized with padding zeros."
    , Builtin
        "product"
        '∏'
        product'
        "Take the product of a list of numbers.\n\
        \The multiplication is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "dot"
        '∙'
        dot
        "Take the dot product of two lists of numbers.\n\
        \The current implementation is simply a composition of \
        \mul and sum."
    , Builtin
        "convolve"
        '×'
        convolve
        "Take the convolution of two lists of numbers."
    , Builtin
        "mean"
        'µ'
        mean
        "Take the mean of a list of numbers."
    , Builtin
        "fromBase"
        'b'
        fromBase
        "Convert a list of digits to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
        \This function is automatically vectorized over the base."
    , Builtin
        "fromBaseRev"
        'd'
        fromBaseRev
        "Convert a list of digits in reverse order to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
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
        \This function is automatically vectorized over both arguments. \
        \If both arguments are lists, \
        \the result is a list of lists of digits."
    , Builtin
        "toBase2Rev"
        'Ƃ'
        toBase2Rev
        "Convert an integer to a list of binary digits in reverse order.\n\
        \This function is automatically vectorized."
    , Builtin
        "cumsum"
        '∫'
        cumsum
        "Take the cumulative sum of a list of numbers.\n\
        \The addition is automatically vectorized with padding zeros."
    , Builtin
        "delta"
        '∆'
        delta
        "Take the difference between adjacent elements of a list of numbers.\n\
        \The subtraction is automatically vectorized with padding zeros."
    , Builtin
        "binomial"
        'Ç'
        binomial
        "Compute the binomial coefficient.\n\
        \The second argument must be an integer.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "factorial"
        'F'
        factorial
        "Compute the factorial of an integer.\n\
        \This function is automatically vectorized."
    , Builtin
        "isPrime"
        'Q'
        isPrime'
        "Check if an integer is prime.\n\
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
        \This function is automatically vectorized."
    , Builtin
        "factor"
        'ƒ'
        factor
        "Factorize a rational number, \
        \and return a list of prime factors and a list of exponents.\n\
        \Fails when the input is zero.\n\
        \This function is automatically vectorized."
    , Builtin
        "gcd"
        'G'
        gcd'
        "Compute the greatest common divisor of two numbers.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "lcm"
        'g'
        lcm'
        "Compute the least common multiple of two numbers.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "intPartition"
        'Ṗ'
        intPartition
        "Partition an integer into a list of integers, \
        \whose sum is the original integer.\n\
        \This function is non-deterministic and automatically vectorized."
    , Builtin
        "sqrt"
        '√'
        sqrt'
        "Compute the square root of a rational number.\n\
        \Fails when the argument is not a perfect square.\n\
        \This function is automatically vectorized."
    , Builtin
        "charToInt"
        'e'
        charToInt
        "Convert a string to a list of integers according to Nekomata's \
        \custom encoding.\n\
        \This function is automatically vectorized."
    , Builtin
        "intToChar"
        'H'
        intToChar
        "Convert an integer or a list of integers to a string according to \
        \Nekomata's custom encoding.\n\
        \Fail when the integer is not in the range 0 to 255."
    , Builtin
        "anyOf"
        '~'
        anyOf'
        "Choose an element from a list or a character from a string.\n\
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
        "Get the length of a list or a string."
    , Builtin
        "lengthIs"
        'L'
        lengthIs
        "Check if the length of a list or a string is equal to a given \
        \integer.\n\
        \If it is, push the list or string itself, otherwise fail."
    , Builtin
        "nth"
        '@'
        nth
        "Get the nth element of a list or a string.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "head"
        'h'
        head'
        "Get the first element of a list or a string."
    , Builtin
        "tail"
        't'
        tail'
        "Remove the first element of a list or a string."
    , Builtin
        "cons"
        'c'
        cons
        "Prepend an element to a list."
    , Builtin
        "uncons"
        'C'
        uncons
        "Get the first element and the rest of a list or a string."
    , Builtin
        "last"
        'l'
        last'
        "Get the last element of a list or a string."
    , Builtin
        "init"
        'i'
        init'
        "Remove the last element of a list or a string."
    , Builtin
        "snoc"
        'ɔ'
        snoc
        "Append an element to a list."
    , Builtin
        "unsnoc"
        'Ɔ'
        unsnoc
        "Get the last element and the rest of a list or a string."
    , Builtin
        "cons0"
        'ç'
        cons0
        "Prepend a zero to a list."
    , Builtin
        "reverse"
        '↔'
        reverse'
        "Reverse a list or a string."
    , Builtin
        "prefix"
        'p'
        prefix
        "Get a prefix of a list or a string.\n\
        \This function is non-deterministic."
    , Builtin
        "suffix"
        's'
        suffix
        "Get a suffix of a list or a string.\n\
        \This function is non-deterministic."
    , Builtin
        "take"
        'T'
        take'
        "Get the first n elements of a list or a string.\n\
        \This function is automatically vectorized on the second argument."
    , Builtin
        "subset"
        'S'
        subset
        "Get a finite subset of a list or a string.\n\
        \This function is non-deterministic."
    , Builtin
        "subsequence"
        'q'
        subsequence
        "Get a finite contiguous subsequence of a list or a string.\n\
        \This function is non-deterministic."
    , Builtin
        "join"
        ','
        join'
        "Concatenate two lists or two strings.\n\
        \If one of the arguments is a string, \
        \the other argument is converted to a string as well."
    , Builtin
        "split"
        ';'
        split
        "Split a list or a string into two parts.\n\
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
        \This order used in this function is different from the one \
        \used in min and max. It can compare two arbitrary values, \
        \not just numbers or strings."
    , Builtin
        "maximum"
        'Ṁ'
        maximum'
        "Get the maximum of a list.\n\
        \This order used in this function is different from the one \
        \used in min and max. It can compare two arbitrary values, \
        \not just numbers or strings."
    , Builtin
        "concat"
        'j'
        concat'
        "Concatenate a list of lists or a list of strings.\n\
        \If one item in the list is a string, \
        \the other items are converted to strings as well."
    , Builtin
        "unconcat"
        'J'
        unconcat
        "Split a list or a string into a list of lists or a list of strings.\n\
        \This function is non-deterministic."
    , Builtin
        "nub"
        'u'
        nub
        "Remove duplicate elements from a list or a string."
    , Builtin
        "sort"
        'o'
        sort
        "Sort a list or a string."
    , Builtin
        "permutation"
        '↕'
        permutation
        "Get a permutation of a list or a string.\n\
        \This function is non-deterministic."
    , Builtin
        "allEqual"
        '≡'
        allEqual
        "Check if all elements in a list are equal.\n\
        \If it is, push the equal element, otherwise fail.\n\
        \If the list is empty, this function fails."
    , Builtin
        "free"
        'f'
        free
        "Check if a list is free of a given element.\n\
        \This means that the element does not occur in the list, \
        \its sublists, or its subsublists, etc.\n\
        \If it is, push the list itself, otherwise fail."
    , Builtin
        "enumerate"
        'x'
        enumerate
        "Push a list of integers from 0 to the length of the argument minus 1 \
        \without popping the argument."
    , Builtin
        "rotate"
        'Ř'
        rotate
        "Rotate a list or a string by a given number of positions.\n\
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
        "Split a list or a string into a list of chunks of equal elements."
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
