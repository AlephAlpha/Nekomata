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

import Control.Arrow (second)
import Control.Monad (join, liftM2, (>=>))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Math.NumberTheory.Primes
import Math.NumberTheory.Primes.Testing (isCertifiedPrime)
import Nekomata.CodePage
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

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
        "Push a value that always fails."
    , Builtin
        "allValues"
        'a'
        allValues
        "Get a list of all possible values for a non-deterministic object."
    , Builtin
        "oneValue"
        '¡'
        oneValue
        "Get a single value from a non-deterministic object.\n\
        \Fails if the object has no values."
    , Builtin
        "countValues"
        'n'
        countValues'
        "Count the number of values in a non-deterministic object."
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
        "pow"
        'E'
        pow
        "Raise a number to a an power.\n\
        \Fails when the exponent is not an integer.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
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
        "bytes"
        'e'
        bytes
        "Convert a string to a list of integers according to Nekomata's \
        \custom encoding.\n\
        \This function is automatically vectorized."
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
        "pair"
        'Ð'
        pair
        "Create a list with two elements."
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
        "Get the first element list and the rest of a list or a string."
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
        "Get the last element list and the rest of a list or a string."
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

-- Basic functions

choice :: Function
choice = Function (Arity 2 1) $ \i (x :+ y :+ s) -> Choice i y x :+ s

fail' :: Function
fail' = constant (Fail :: TryData)

allValues :: Function
allValues = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> (ds, toTryData . fromList $ values ds x)) :+ s

oneValue :: Function
oneValue = Function (Arity 1 1) $
    \_ (x :+ s) ->
        Cut (\ds -> maybe (ds, Fail) (second toTryData) $ firstValue ds x) :+ s

countValues' :: Function
countValues' = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> (ds, toTryData $ countValues ds x)) :+ s

normalForm' :: Function
normalForm' = Function (Arity 1 1) $ \_ (x :+ s) -> normalForm x :+ s

if' :: Function
if' = choice .* oneValue

-- Stack manipulation

drop' :: Function
drop' = Function (Arity 1 0) $ \_ (_ :+ s) -> s

dup :: Function
dup = Function (Arity 1 2) $ \_ (x :+ s) -> x :+ x :+ s

swap :: Function
swap = Function (Arity 2 2) $ \_ (x :+ y :+ s) -> y :+ x :+ s

-- Predicates

eq :: Function
eq = predicate2 $ \_ x y -> tryEq x y

ne :: Function
ne = predicate2 $ \_ x y -> tryNe x y

nonempty' :: Function
nonempty' = predicate nonempty''
  where
    nonempty'' _ (DListT x) = nonempty_ <$> x
    nonempty'' _ (DStringT x) = nonempty_ <$> x
    nonempty'' _ _ = Fail
    nonempty_ :: ListTry a -> Bool
    nonempty_ (Cons _ _) = True
    nonempty_ Nil = False

nonzero :: Function
nonzero = predicateVec nonzero'
  where
    nonzero' _ (DNumT x) = (/= 0) . unDet <$> x
    nonzero' _ _ = Fail

isPositive :: Function
isPositive = predicateVec isPositive'
  where
    isPositive' _ (DNumT x) = (> 0) . unDet <$> x
    isPositive' _ _ = Fail

less :: Function
less = predicateVec2 less'
  where
    less' _ (DNumT x) (DNumT y) = tryLt x y
    less' _ (DStringT x) (DStringT y) = tryLt x y
    less' _ _ _ = Fail

lessEq :: Function
lessEq = predicateVec2 lessEq'
  where
    lessEq' _ (DNumT x) (DNumT y) = tryLe x y
    lessEq' _ (DStringT x) (DStringT y) = tryLe x y
    lessEq' _ _ _ = Fail

greater :: Function
greater = predicateVec2 greater'
  where
    greater' _ (DNumT x) (DNumT y) = tryGt x y
    greater' _ (DStringT x) (DStringT y) = tryGt x y
    greater' _ _ _ = Fail

greaterEq :: Function
greaterEq = predicateVec2 greaterEq'
  where
    greaterEq' _ (DNumT x) (DNumT y) = tryGe x y
    greaterEq' _ (DStringT x) (DStringT y) = tryGe x y
    greaterEq' _ _ _ = Fail

-- Math functions

neg1 :: Function
neg1 = constant (-1 :: Integer)

ten :: Function
ten = constant (10 :: Integer)

neg :: Function
neg = unaryVec neg'
  where
    neg' _ (DNumT x) = liftNum negate x
    neg' _ _ = Fail

abs' :: Function
abs' = unaryVec abs''
  where
    abs'' _ (DNumT x) = liftNum abs x
    abs'' _ _ = Fail

increment :: Function
increment = unaryVec increment'
  where
    increment' _ (DNumT x) = liftNum (+ 1) x
    increment' _ _ = Fail

decrement :: Function
decrement = unaryVec decrement'
  where
    decrement' _ (DNumT x) = liftNum (subtract 1) x
    decrement' _ _ = Fail

logicalNot :: Function
logicalNot = unaryVec logicalNot'
  where
    logicalNot' _ (DNumT x) = liftNum logicalNot_ x
    logicalNot' _ _ = Fail
    logicalNot_ :: Rational -> Rational
    logicalNot_ 0 = 1
    logicalNot_ _ = 0

sign :: Function
sign = unaryVec sign'
  where
    sign' _ (DNumT x) = liftNum signum x
    sign' _ _ = Fail

add :: Function
add = binary add'

add' :: Id -> DataTry -> DataTry -> TryData
add' = vec2Pad add''
  where
    add'' _ (DNumT x) (DNumT y) = liftNum2 (+) x y
    add'' _ _ _ = Fail

sub :: Function
sub = binary sub'

sub' :: Id -> DataTry -> DataTry -> TryData
sub' i x y = neg' (leftId i) y >>= add' (rightId i) x
  where
    neg' = vec1 neg''
    neg'' _ (DNumT x') = liftNum negate x'
    neg'' _ _ = Fail

mul :: Function
mul = binary mul'

mul' :: Id -> DataTry -> DataTry -> TryData
mul' = vec2Fail mul''
  where
    mul'' _ (DNumT x) (DNumT y) = liftNum2 (*) x y
    mul'' _ _ _ = Fail

div' :: Function
div' = binaryVecFail div''
  where
    div'' _ (DNumT x) (DNumT y) = liftNum2 div_ x y
    div'' _ _ _ = Fail
    div_ _ 0 = Fail
    div_ x y = Val $ x / y

divInt :: Function
divInt = binaryVecFail divInt'
  where
    divInt' _ (DNumT x) (DNumT y) = liftNum2 div_ x y
    divInt' _ _ _ = Fail
    div_ :: Rational -> Rational -> Try Rational
    div_ _ 0 = Fail
    div_ x y = Val . fromInteger . floor $ x / y

mod' :: Function
mod' = binaryVecFail mod''
  where
    mod'' _ (DNumT x) (DNumT y) = liftNum2 mod_ x y
    mod'' _ _ _ = Fail
    mod_ _ 0 = Fail
    mod_ x y = Val $ x - y * fromInteger (floor $ x / y)

divExact :: Function
divExact = binaryVecFail divExact'
  where
    divExact' _ (DNumT x) (DNumT y) = liftNum2 divExact_ x y
    divExact' _ _ _ = Fail
    divExact_ _ 0 = Fail
    divExact_ x y =
        let q = x / y in if denominator q == 1 then Val $ numerator q else Fail

pow :: Function
pow = binaryVecFail pow'
  where
    pow' _ (DNumT x) (DNumT y) = liftNum2 pow'' x y
    pow' _ _ _ = Fail
    pow'' x y = toTryInt y <&> pow_ x
    pow_ x y = if y >= 0 then x ^ y else 1 / (x ^ negate y)

min' :: Function
min' = binaryVecPad min''
  where
    min'' _ (DNumT x) (DNumT y) = liftNum2 tryMin x y
    min'' _ (DStringT x) (DStringT y) = liftString2 (AsString .: tryMin) x y
    min'' _ _ _ = Fail

max' :: Function
max' = binaryVecPad max''
  where
    max'' _ (DNumT x) (DNumT y) = liftInt2 tryMax x y
    max'' _ (DStringT x) (DStringT y) = liftString2 (AsString .: tryMax) x y
    max'' _ _ _ = Fail

range0 :: Function
range0 = unaryVec range0'
  where
    range0' _ (DNumT x) = liftNum range0_ x
    range0' _ _ = Fail
    range0_ :: Rational -> [Integer]
    range0_ x = enumFromTo 0 . floor $ x - 1

range1 :: Function
range1 = unaryVec range1'
  where
    range1' _ (DNumT x) = liftNum range1_ x
    range1' _ _ = Fail
    range1_ :: Rational -> [Integer]
    range1_ = enumFromTo 1 . floor

natural :: Function
natural = nullary $
    \i -> toTryData <$> anyOf i $ fromList [0 :: Integer ..]

integer :: Function
integer = nullary $
    \i -> toTryData <$> anyOf i $ fromList integers
  where
    integers = (0 :: Integer) : [y | x <- [1 ..], y <- [x, -x]]

sum' :: Function
sum' = unary sum''
  where
    sum'' i (DListT xs) =
        liftList
            (tryFoldl add' i . DNumT . Val $ Det 0)
            xs
    sum'' _ _ = Fail

product' :: Function
product' = unary product''
  where
    product'' i (DListT xs) =
        liftList
            (tryFoldl mul' i . DNumT . Val $ Det 1)
            xs
    product'' _ _ = Fail

dot :: Function
dot = mul .* sum'

fromBase :: Function
fromBase = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) (DNumT b) =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) b) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldl (mulAdd b) i (DNumT . Val $ Det 0)
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) x >>= add' (rightId i) y

fromBaseRev :: Function
fromBaseRev = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) (DNumT b) =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) b) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldr (mulAdd b) i (DNumT . Val $ Det 0)
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) y >>= add' (rightId i) x

toBaseRev :: Function
toBaseRev = binaryVecOuter toBaseRev'

toBaseRev' :: Id -> DataTry -> DataTry -> TryData
toBaseRev' _ (DNumT x) (DNumT b) = liftInt2 toBaseRev_ x b
toBaseRev' _ _ _ = Fail
toBaseRev_ :: Integer -> Integer -> TryList Integer
toBaseRev_ _ b | b < 1 = Fail
toBaseRev_ x b | x < 0 = toBaseRev_ (-x) b
toBaseRev_ x 1 = Val . fromList $ replicate (fromIntegral x) 1
toBaseRev_ 0 _ = Val Nil
toBaseRev_ x b = Val $ Cons (x `mod` b) (toBaseRev_ (x `div` b) b)

toBase :: Function
toBase = binaryVecOuter toBase'
  where
    toBase' i x y = toBaseRev' (leftId i) x y >>= reverse'' (rightId i)

cumsum :: Function
cumsum = unary cumsum'
  where
    cumsum' i (DListT xs) = liftList (tryScanl1 add' i) xs
    cumsum' _ _ = Fail

delta :: Function
delta = unary delta'
  where
    delta' i (DListT xs) = liftList (delta_ i) xs
    delta' _ _ = Fail
    delta_ _ Nil = Fail
    delta_ i s@(Cons _ xs) = xs >>= flip (zipWithTrunc sub' i) s

binomial :: Function
binomial = binaryVecFail binomial'
  where
    binomial' _ (DNumT n) (DNumT k) = liftNum2 binomial'' n k
    binomial' _ _ _ = Fail
    binomial'' n k = binomial_ n <$> toTryInt k
    binomial_ n k =
        product [n + 1 - fromInteger i | i <- [1 .. k]]
            / fromInteger (product [1 .. k])

isPrime' :: Function
isPrime' = predicateVec isPrime''
  where
    isPrime'' _ (DNumT x) = isCertifiedPrime <$> toTryInt' x
    isPrime'' _ _ = Fail

prime :: Function
prime = nullary $
    \i ->
        toTryData
            <$> anyOf i . fromList
            $ map unPrime [nextPrime (1 :: Integer) ..]

-- String functions

bytes :: Function
bytes = unaryVec bytes'
  where
    bytes' _ (DStringT x) =
        liftString (fmap (fmap toInteger . flip elemIndex codePage)) x
    bytes' _ _ = Fail

-- List functions

anyOf' :: Function
anyOf' = unary anyOf''
  where
    anyOf'' i (DStringT xs) = Val . DStringT $ xs >>= fmap singleton . anyOf i
    anyOf'' i (DListT xs) = join (xs >>= anyOf i)
    anyOf'' _ _ = Fail

emptyList :: Function
emptyList = constant . Val . DListT $ Val Nil

singleton' :: Function
singleton' = unary . const $ toTryData . singleton

pair :: Function
pair = binary pair'
  where
    pair' _ x y = toTryData [x, y]

removeFail :: Function
removeFail = unary removeFail'
  where
    removeFail' _ (DListT xs) = Val . DListT $ xs >>= filterTry
    removeFail' _ x = Val x

length' :: Function
length' = unary length''
  where
    length'' _ (DStringT xs) = liftString length_ xs
    length'' _ (DListT xs) = liftList length_ xs
    length'' _ _ = toTryData (1 :: Integer)
    length_ :: ListTry a -> Try Integer
    length_ Nil = Val 0
    length_ (Cons _ xs) = xs >>= length_ <&> (+ 1)

lengthIs :: Function
lengthIs = binary lengthIs'
  where
    lengthIs' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . (`lengthIs_` x)) y) xs
    lengthIs' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`lengthIs_` x) y) xs
    lengthIs' _ _ _ = Fail
    lengthIs_ :: Integer -> ListTry a -> TryList a
    lengthIs_ 0 Nil = Val Nil
    lengthIs_ _ Nil = Fail
    lengthIs_ n _ | n <= 0 = Fail
    lengthIs_ n (Cons x xs) = Cons x . lengthIs_ (n - 1) <$> xs

nth :: Function
nth = binaryVecArg2 nth'
  where
    nth' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (`nth_` x) y) xs
    nth' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`nth_` x) y) xs
    nth' _ _ _ = Fail
    nth_ :: Integer -> ListTry a -> Try a
    nth_ 0 (Cons x _) = Val x
    nth_ n (Cons _ xs) | n > 0 = xs >>= nth_ (n - 1)
    nth_ _ _ = Fail

head' :: Function
head' = unary head''
  where
    head'' _ (DStringT xs) = liftString head_ xs
    head'' _ (DListT xs) = liftList head_ xs
    head'' _ _ = Fail
    head_ :: ListTry a -> Try a
    head_ Nil = Fail
    head_ (Cons x _) = Val x

tail' :: Function
tail' = unary tail''
  where
    tail'' _ (DStringT xs) = liftString (AsString . tail_) xs
    tail'' _ (DListT xs) = liftList tail_ xs
    tail'' _ _ = Fail
    tail_ :: ListTry a -> TryList a
    tail_ Nil = Fail
    tail_ (Cons _ xs) = xs

cons :: Function
cons = binary cons'
  where
    cons' _ (DListT xs) y = liftList (Cons (Val y) . Val) xs
    cons' _ x y = liftList (Cons (Val y) . Val) (Val . singleton $ Val x)

uncons :: Function
uncons = unary2 uncons'
  where
    uncons' _ (DStringT xs) =
        liftString12 (uncons_ >=> \(ys, y) -> Val (AsString ys, y)) xs
    uncons' _ (DListT xs) = liftList12 uncons_ xs
    uncons' _ _ = (Fail, Fail)
    uncons_ :: ListTry a -> Try (TryList a, a)
    uncons_ Nil = Fail
    uncons_ (Cons x xs) = Val (xs, x)

last' :: Function
last' = unary last''
  where
    last'' _ (DStringT xs) = liftString last_ xs
    last'' _ (DListT xs) = liftList last_ xs
    last'' _ _ = Fail
    last_ :: ListTry a -> Try (Maybe a)
    last_ Nil = Val Nothing
    last_ (Cons x xs) = xs >>= last_ >>= Val . Just . fromMaybe x

init' :: Function
init' = unary init''
  where
    init'' _ (DStringT xs) = liftString (AsString . init_) xs
    init'' _ (DListT xs) = liftList init_ xs
    init'' _ _ = Fail
    init_ :: ListTry a -> Try (Maybe (ListTry a))
    init_ Nil = Val Nothing
    init_ (Cons x xs) = xs >>= init_ >>= Val . Just . maybe Nil (Cons x . Val)

snoc :: Function
snoc = singleton' .* join'

unsnoc :: Function
unsnoc = unary2 unsnoc'
  where
    unsnoc' _ (DStringT xs) =
        liftString12 (unsnoc'' >=> \(ys, y) -> Val (AsString ys, y)) xs
    unsnoc' _ (DListT xs) = liftList12 unsnoc'' xs
    unsnoc' _ _ = (Fail, Fail)
    unsnoc'' = fmap unzipMaybe . unsnoc_
    unsnoc_ :: ListTry a -> Try (Maybe (ListTry a, a))
    unsnoc_ Nil = Val Nothing
    unsnoc_ (Cons x xs) =
        xs
            >>= unsnoc_
            >>= Val
                . Just
                . maybe (Nil, x) (\(ys, y) -> (Cons x (Val ys), y))
    unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
    unzipMaybe Nothing = (Nothing, Nothing)
    unzipMaybe (Just (a, b)) = (Just a, Just b)

cons0 :: Function
cons0 = constant (0 :: Integer) .* cons

reverse' :: Function
reverse' = unary reverse''

reverse'' :: Id -> DataTry -> TryData
reverse'' _ (DStringT xs) = liftString (AsString . reverse_ Nil) xs
reverse'' _ (DListT xs) = liftList (reverse_ Nil) xs
reverse'' _ _ = Fail

reverse_ :: ListTry a -> ListTry a -> TryList a
reverse_ ys Nil = Val ys
reverse_ ys (Cons x xs) = xs >>= reverse_ (Cons x (Val ys))

prefix :: Function
prefix = unary prefix'
  where
    prefix' i (DStringT xs) = liftString (AsString . prefix_ i) xs
    prefix' i (DListT xs) = liftList (prefix_ i) xs
    prefix' _ _ = Fail
    prefix_ :: Id -> ListTry a -> TryList a
    prefix_ _ Nil = Val Nil
    prefix_ i (Cons x xs) =
        Choice (leftId i) (Val Nil) (Val . Cons x $ xs >>= prefix_ (rightId i))

suffix :: Function
suffix = unary suffix'
  where
    suffix' i (DStringT xs) = liftString (AsString . suffix_ i) xs
    suffix' i (DListT xs) = liftList (suffix_ i) xs
    suffix' _ _ = Fail
    suffix_ :: Id -> ListTry a -> TryList a
    suffix_ _ Nil = Val Nil
    suffix_ i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix_ (rightId i))

take' :: Function
take' = binaryVecArg2 take''
  where
    take'' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . (`take_` x)) y) xs
    take'' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`take_` x) y) xs
    take'' _ _ _ = Fail
    take_ :: Integer -> ListTry a -> TryList a
    take_ 0 _ = Val Nil
    take_ n (Cons x xs) | n > 0 = Cons x . take_ (n - 1) <$> xs
    take_ _ _ = Fail

subset :: Function
subset = unary subset'
  where
    subset' i (DStringT xs) =
        liftString (AsString . subset_ i) xs
    subset' i (DListT xs) = liftList (subset_ i) xs
    subset' _ _ = Fail
    subset_ :: Id -> ListTry a -> TryList a
    subset_ i xs = Choice (leftId i) (Val Nil) (nonemptySubset (rightId i) xs)
    nonemptySubset _ Nil = Fail
    nonemptySubset i (Cons x xs) =
        Choice (leftId i) (Val $ singleton x) $
            xs >>= nonemptySubset (leftId (rightId i)) >>= \ys ->
                Choice (rightId (rightId i)) (Val ys) (Val . Cons x $ Val ys)

subsequence :: Function
subsequence = unary subsequence'
  where
    subsequence' i (DStringT xs) = liftString (AsString . subsequence_ i) xs
    subsequence' i (DListT xs) = liftList (subsequence_ i) xs
    subsequence' _ _ = Fail
    subsequence_ :: Id -> ListTry a -> TryList a
    subsequence_ i xs =
        Choice
            (leftId i)
            (Val Nil)
            (prefix' (leftId (rightId i)) xs >>= suffix' (rightId (rightId i)))
    prefix' _ Nil = Fail
    prefix' i (Cons x xs) =
        Choice
            (leftId i)
            (Val $ singleton x)
            (Val . Cons x $ xs >>= prefix' (rightId i))
    suffix' _ Nil = Fail
    suffix' i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix' (rightId i))

join' :: Function
join' = binary join''

join'' :: Id -> DataTry -> DataTry -> TryData
join'' _ (DStringT xs) (DStringT ys) = liftString2 (AsString .: join_) xs ys
join'' i x@(DStringT _) y =
    join'' i x (DStringT (fromList . map Det . show <$> toTry y))
join'' i x y@(DStringT _) =
    join'' i (DStringT (fromList . map Det . show <$> toTry x)) y
join'' _ (DListT xs) (DListT ys) = liftList2 join_ xs ys
join'' _ _ _ = Fail
join_ :: ListTry a -> ListTry a -> TryList a
join_ Nil ys = Val ys
join_ (Cons x xs) ys = Val . Cons x $ liftJoinM2 join_ xs (Val ys)

split :: Function
split = unary2 split'
  where
    split' i (DStringT xs) =
        liftString12 (split_ i >=> \(x, y) -> Val (AsString x, AsString y)) xs
    split' i (DListT xs) = liftList12 (split_ i) xs
    split' _ _ = (Fail, Fail)
    split_ :: Id -> ListTry a -> Try (ListTry a, ListTry a)
    split_ _ Nil = Val (Nil, Nil)
    split_ i s@(Cons x xs) =
        Choice
            (leftId i)
            (Val (Nil, s))
            (xs >>= split_ (rightId i) <&> \(ys, zs) -> (Cons x (Val ys), zs))

minimum' :: Function
minimum' = unary minimum''
  where
    minimum'' i (DListT xs) = liftList (tryFoldl1 (const tryMin) i) xs
    minimum'' _ _ = Fail

maximum' :: Function
maximum' = unary maximum''
  where
    maximum'' i (DListT xs) = liftList (tryFoldl1 (const tryMax) i) xs
    maximum'' _ _ = Fail

concat' :: Function
concat' = unary concat''
  where
    concat'' i (DListT xs) = xs >>= concat_ i
    concat'' _ _ = Fail
    concat_ _ Nil = Val . DListT $ Val Nil
    concat_ i (Cons x xs) = liftJoinM2 (tryFoldl join'' i) x xs

unconcat :: Function
unconcat = unary unconcat'
  where
    unconcat' i (DStringT xs) =
        liftString (fmap (fmap AsString) . unconcat_ i) xs
    unconcat' i (DListT xs) = liftList (unconcat_ i) xs
    unconcat' _ _ = Fail
    unconcat_ :: Id -> ListTry a -> TryList (TryList a)
    unconcat_ _ Nil = Val Nil
    unconcat_ i (Cons x xs) =
        Choice
            (leftId i)
            (Val $ Cons (Val $ singleton x) (xs >>= unconcat_ (rightId i)))
            (xs >>= unconcat_ (rightId i) >>= prependToFirst x)
    prependToFirst _ Nil = Fail
    prependToFirst x (Cons y ys) = Val $ Cons (Val $ Cons x y) ys

nub :: Function
nub = unary nub'
  where
    nub' i (DStringT xs) = liftString (AsString . nub_ i) xs
    nub' i (DListT xs) = liftList (nub_ i) xs
    nub' _ _ = Fail
    nub_ :: TryEq a => Id -> ListTry a -> ListTry a
    nub_ _ Nil = Nil
    nub_ i (Cons x xs) =
        Cons x $
            xs
                >>= tryFilter (const $ tryNe x) (leftId i)
                <&> nub_ (rightId i)

sort :: Function
sort = unary sort'
  where
    sort' i (DStringT xs) = liftString (AsString . sort_ i) xs
    sort' i (DListT xs) = liftList (sort_ i) xs
    sort' _ _ = Fail
    sort_ :: TryOrd a => Id -> ListTry a -> TryList a
    sort_ _ xs = mergeLists (Val . singleton <$> xs)
    mergeLists :: TryOrd a => ListTry (TryList a) -> TryList a
    mergeLists Nil = Val Nil
    mergeLists (Cons x xs) = xs >>= mergeLists' x
    mergeLists' :: TryOrd a => TryList a -> ListTry (TryList a) -> TryList a
    mergeLists' x Nil = x
    mergeLists' x (Cons y ys) =
        ys >>= mergePairs >>= mergeLists' (liftJoinM2 merge x y)
    mergePairs :: TryOrd a => ListTry (TryList a) -> TryList (TryList a)
    mergePairs Nil = Val Nil
    mergePairs (Cons x xs) = mergePairs' x <$> xs
    mergePairs' x Nil = singleton x
    mergePairs' x (Cons y ys) = Cons (liftJoinM2 merge x y) (ys >>= mergePairs)
    merge :: TryOrd a => ListTry a -> ListTry a -> TryList a
    merge Nil ys = Val ys
    merge xs Nil = Val xs
    merge (Cons x xs) (Cons y ys) =
        tryLe x y <&> \b ->
            if b
                then Cons x (liftJoinM2 merge xs (Val $ Cons y ys))
                else Cons y (liftJoinM2 merge (Val $ Cons x xs) ys)

permutation :: Function
permutation = unary permutation'
  where
    permutation' i (DStringT xs) = liftString (AsString . permutation_ i) xs
    permutation' i (DListT xs) = liftList (permutation_ i) xs
    permutation' _ _ = Fail
    permutation_ :: Id -> ListTry a -> TryList a
    permutation_ _ Nil = Val Nil
    permutation_ i xs =
        extract' (leftId i) xs >>= \(x, xs') ->
            Val $ Cons x (xs' >>= permutation_ (rightId i))
    extract' :: Id -> ListTry a -> Try (a, TryList a)
    extract' _ Nil = Fail
    extract' i (Cons x xs) =
        Choice
            (leftId i)
            (Val (x, xs))
            (xs >>= extract' (rightId i) <&> second (Val . Cons x))

allEqual :: Function
allEqual = unary allEqual'
  where
    allEqual' i (DStringT xs) = liftString (tryFoldl1 tryEq' i . fmap Val) xs
    allEqual' i (DListT xs) = liftList (tryFoldl1 tryEq' i) xs
    allEqual' _ _ = Fail
    tryEq' :: TryEq a => Id -> a -> a -> Try a
    tryEq' _ x y = tryEq x y <&> \b -> if b then x else y

free :: Function
free = predicate2 free'
  where
    free' i x@(DListT xs) y = liftM2 (&&) (tryNe x y) (xs >>= free'' i y)
    free' _ x y = tryNe x y
    free'' :: Id -> DataTry -> ListTry TryData -> Try Bool
    free'' _ _ Nil = Val True
    free'' i y (Cons x xs) =
        liftM2
            (&&)
            (x >>= flip (free' (leftId i)) y)
            (xs >>= free'' (rightId i) y)

enumerate :: Function
enumerate = dup .* length' .* range0
