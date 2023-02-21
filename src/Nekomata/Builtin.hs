module Nekomata.Builtin (
    Builtin (..),
    BuiltinNotFoundError (..),
    builtins,
    builtinMap,
    builtinShortMap,
    info,
    infoByName,
) where

import Control.Monad (join, (>=>))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
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
        '∀'
        allValues
        "Get a list of all possible values for a non-deterministic object."
    , Builtin
        "oneValue"
        '∃'
        oneValue
        "Get a single value from a non-deterministic object. \n\
        \Fails if the object has no values."
    , Builtin
        "countValues"
        'n'
        countValues'
        "Count the number of values in a non-deterministic object."
    , Builtin "drop" '^' drop' "Drop the top value of the stack."
    , Builtin "dup" ':' dup "Duplicate the top value of the stack."
    , Builtin "swap" '$' swap "Swap the top two values of the stack."
    , Builtin
        "eq"
        '='
        eq
        "Check if two values are equal. \n\
        \If they are, push the first value, otherwise fail."
    , Builtin
        "ne"
        '≠'
        ne
        "Check if two values are not equal. \n\
        \If they are not, push the first value, otherwise fail."
    , Builtin
        "nonempty"
        'N'
        nonempty'
        "Check if a list or string is non-empty. \n\
        \If it is, push the list or string itself, otherwise fail."
    , Builtin
        "nonzero"
        'Z'
        nonzero
        "Check if an integer is non-zero. \n\
        \If it is, push the integer itself, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "positive"
        'P'
        positive
        "Check if an integer is positive. \n\
        \If it is, push the integer itself, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "less"
        '<'
        less
        "Check if the first integer is less than the second. \n\
        \If it is, push the first integer, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "lessEq"
        '≤'
        lessEq
        "Check if the first integer is less than or equal to the second. \n\
        \If it is, push the first integer, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "greater"
        '>'
        greater
        "Check if the first integer is greater than the second. \n\
        \If it is, push the first integer, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "greaterEq"
        '≥'
        greaterEq
        "Check if the first integer is greater than or equal to the second. \n\
        \If it is, push the first integer, otherwise fail.\n\
        \This function is automatically vectorized with filtering."
    , Builtin
        "neg"
        '_'
        neg
        "Negate an integer. \n\
        \This function is automatically vectorized."
    , Builtin
        "abs"
        'A'
        abs'
        "Absolute value of an integer. \n\
        \This function is automatically vectorized."
    , Builtin
        "increment"
        '→'
        increment
        "Increment an integer. \n\
        \This function is automatically vectorized."
    , Builtin
        "decrement"
        '←'
        decrement
        "Decrement an integer. \n\
        \This function is automatically vectorized."
    , Builtin
        "logicalNot"
        '¬'
        logicalNot
        "Returns 1 if the argument is 0, and 0 otherwise. \n\
        \This function is automatically vectorized."
    , Builtin
        "sign"
        '±'
        sign
        "Returns -1 if the argument is negative, 0 if it is zero, \
        \and 1 if it is positive. \n\
        \This function is automatically vectorized."
    , Builtin
        "add"
        '+'
        add
        "Add two integers. \n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "sub"
        '-'
        sub
        "Subtract two integers. \n\
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "mul"
        '*'
        mul
        "Multiply two integers. \n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "div"
        '÷'
        div'
        "Integer division of two integers. \
        \Result is rounded towards negative infinity. \n\
        \Fails when the divisor is zero. \n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "mod"
        '%'
        mod'
        "Modulo two integers. \n\
        \Fails when the divisor is zero. \n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divExact"
        '∣'
        divExact
        "Divide two integers. \n\
        \Fails when the divisor is zero or \
        \the result is not an exact integer. \n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "min"
        'm'
        min'
        "Get the minimum of two integers. \n\
        \This function is automatically vectorized with padding."
    , Builtin
        "max"
        'M'
        max'
        "Get the maximum of two integers. \n\
        \This function is automatically vectorized with padding."
    , Builtin
        "range0"
        'r'
        range0
        "Create a list of integers from 0 to n-1. \n\
        \This function is automatically vectorized."
    , Builtin
        "range1"
        'R'
        range1
        "Create a list of integers from 1 to n. \n\
        \This function is automatically vectorized."
    , Builtin
        "natural"
        'ℕ'
        natural
        "Non-deterministically choose a natural number. \n\
        \This function is non-deterministic."
    , Builtin
        "integer"
        'ℤ'
        integer
        "Non-deterministically choose an integer. \n\
        \This function is non-deterministic."
    , Builtin
        "fromBase"
        'B'
        fromBase
        "Convert a list of digits to an integer. \n\
        \The first argument is the list of digits, \
        \the second argument is the base. \n\
        \This function is automatically vectorized over the base."
    , Builtin
        "anyOf"
        '~'
        anyOf'
        "Choose an element from a list or a character from a string. \n\
        \This function is non-deterministic."
    , Builtin
        "emptyList"
        '∅'
        emptyList
        "Push an empty list."
    , Builtin
        "singleton"
        'u'
        singleton'
        "Create a list with a single element."
    , Builtin
        "pair"
        'd'
        pair
        "Create a list with two elements."
    , Builtin
        "removeFail"
        'F'
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
        \integer. \n\
        \If it is, push the list or string itself, otherwise fail."
    , Builtin
        "nth"
        '@'
        nth
        "Get the nth element of a list or a string. \n\
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
        "reverse"
        '↔'
        reverse'
        "Reverse a list or a string."
    , Builtin
        "prefix"
        'p'
        prefix
        "Get a prefix of a list or a string. \n\
        \This function is non-deterministic."
    , Builtin
        "suffix"
        's'
        suffix
        "Get a suffix of a list or a string. \n\
        \This function is non-deterministic."
    , Builtin
        "subset"
        'S'
        subset
        "Get a finite subset of a list or a string. \n\
        \This function is non-deterministic."
    , Builtin
        "subsequence"
        'q'
        subsequence
        "Get a finite contiguous subsequence of a list or a string. \n\
        \This function is non-deterministic."
    , Builtin
        "join"
        '⧺'
        join'
        "Concatenate two lists or two strings.\n\
        \If one of the arguments is a string, \
        \the other argument is converted to a string as well."
    , Builtin
        "sum"
        '∑'
        sum'
        "Take the sum of a list of integers. \n\
        \The addition is automatically vectorized with padding zeros."
    , Builtin
        "product"
        '∏'
        product'
        "Take the product of a list of integers. \n\
        \The multiplication is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "dot"
        '∙'
        dot
        "Take the dot product of two lists of integers. \n\
        \The current implementation is simply a composition of \
        \mul and sum."
    , Builtin
        "concat"
        '∐'
        concat'
        "Concatenate a list of lists or a list of strings. \n\
        \If one item in the list is a string, \
        \the other items are converted to strings as well."
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
    \_ (x :+ s) -> Cut (\ds -> toTryData . fromList $ values ds x) :+ s

oneValue :: Function
oneValue = Function (Arity 1 1) $ \_ (x :+ s) -> firstValue x :+ s

countValues' :: Function
countValues' = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> toTryData $ countValues ds x) :+ s

-- Stack manipulation

drop' :: Function
drop' = Function (Arity 1 0) $ \_ (_ :+ s) -> s

dup :: Function
dup = Function (Arity 0 1) $ \_ (x :+ s) -> x :+ x :+ s

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
    nonzero' _ (DIntT x) = (/= 0) . fromDet <$> x
    nonzero' _ _ = Fail

positive :: Function
positive = predicateVec positive'
  where
    positive' _ (DIntT x) = (> 0) . fromDet <$> x
    positive' _ _ = Fail

less :: Function
less = predicateVec2 less'
  where
    less' _ (DIntT x) (DIntT y) = (<) <$> toTry x <*> toTry y
    less' _ (DStringT x) (DStringT y) = (<) <$> toTry x <*> toTry y
    less' _ _ _ = Fail

lessEq :: Function
lessEq = predicateVec2 lessEq'
  where
    lessEq' _ (DIntT x) (DIntT y) = (<=) <$> toTry x <*> toTry y
    lessEq' _ (DStringT x) (DStringT y) = (<=) <$> toTry x <*> toTry y
    lessEq' _ _ _ = Fail

greater :: Function
greater = predicateVec2 greater'
  where
    greater' _ (DIntT x) (DIntT y) = (>) <$> toTry x <*> toTry y
    greater' _ (DStringT x) (DStringT y) = (>) <$> toTry x <*> toTry y
    greater' _ _ _ = Fail

greaterEq :: Function
greaterEq = predicateVec2 greaterEq'
  where
    greaterEq' _ (DIntT x) (DIntT y) = (>=) <$> toTry x <*> toTry y
    greaterEq' _ (DStringT x) (DStringT y) = (>=) <$> toTry x <*> toTry y
    greaterEq' _ _ _ = Fail

-- Math functions

neg :: Function
neg = unaryVec neg'
  where
    neg' _ (DIntT x) = liftInt negate x
    neg' _ _ = Fail

abs' :: Function
abs' = unaryVec abs''
  where
    abs'' _ (DIntT x) = liftInt abs x
    abs'' _ _ = Fail

increment :: Function
increment = unaryVec increment'
  where
    increment' _ (DIntT x) = liftInt (+ 1) x
    increment' _ _ = Fail

decrement :: Function
decrement = unaryVec decrement'
  where
    decrement' _ (DIntT x) = liftInt (subtract 1) x
    decrement' _ _ = Fail

logicalNot :: Function
logicalNot = unaryVec logicalNot'
  where
    logicalNot' _ (DIntT x) = liftInt logicalNot_ x
    logicalNot' _ _ = Fail
    logicalNot_ :: Integer -> Integer
    logicalNot_ 0 = 1
    logicalNot_ _ = 0

sign :: Function
sign = unaryVec sign'
  where
    sign' _ (DIntT x) = liftInt signum x
    sign' _ _ = Fail

add :: Function
add = binaryVecPad add'

add' :: Id -> DataTry -> DataTry -> TryData
add' _ (DIntT x) (DIntT y) = liftInt2 (+) x y
add' _ _ _ = Fail

sub :: Function
sub = compose neg add

mul :: Function
mul = binaryVecFail mul'

mul' :: Id -> DataTry -> DataTry -> TryData
mul' _ (DIntT x) (DIntT y) = liftInt2 (*) x y
mul' _ _ _ = Fail

div' :: Function
div' = binaryVecFail div''
  where
    div'' _ (DIntT x) (DIntT y) = liftInt2 div_ x y
    div'' _ _ _ = Fail
    div_ _ 0 = Fail
    div_ x y = Val $ x `div` y

mod' :: Function
mod' = binaryVecFail mod''
  where
    mod'' _ (DIntT x) (DIntT y) = liftInt2 mod_ x y
    mod'' _ _ _ = Fail
    mod_ _ 0 = Fail
    mod_ x y = Val $ x `mod` y

divExact :: Function
divExact = binaryVecFail divExact'
  where
    divExact' _ (DIntT x) (DIntT y) = liftInt2 divExact_ x y
    divExact' _ _ _ = Fail
    divExact_ x y = if y /= 0 && x `mod` y == 0 then Val $ x `div` y else Fail

min' :: Function
min' = binaryVecPad min''
  where
    min'' _ (DIntT x) (DIntT y) = liftInt2 min x y
    min'' _ _ _ = Fail

max' :: Function
max' = binaryVecPad max''
  where
    max'' _ (DIntT x) (DIntT y) = liftInt2 max x y
    max'' _ _ _ = Fail

range0 :: Function
range0 = unaryVec range0'
  where
    range0' _ (DIntT x) = liftInt (enumFromTo 0 . subtract 1) x
    range0' _ _ = Fail

range1 :: Function
range1 = unaryVec range1'
  where
    range1' _ (DIntT x) = liftInt (enumFromTo 1) x
    range1' _ _ = Fail

natural :: Function
natural = nullary $
    \i -> toTryData <$> anyOf i $ fromList [0 :: Integer ..]

integer :: Function
integer = nullary $
    \i -> toTryData <$> anyOf i $ fromList integers
  where
    integers = (0 :: Integer) : [y | x <- [1 ..], y <- [x, -x]]

fromBase :: Function
fromBase = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) (DIntT b) =
        liftList (\x -> liftInt (\b' -> fromBase_ i b' x) b) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldl (mulAdd b) i (DIntT . Val $ Det 0)
    mulAdd b i x y =
        toTryData b >>= vec2Pad mul' i x >>= vec2Pad add' (leftId i) y

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
    lengthIs' _ (DStringT xs) (DIntT y) =
        liftString (\x -> liftInt (fmap AsDString . (`lengthIs_` x)) y) xs
    lengthIs' _ (DListT xs) (DIntT y) =
        liftList (\x -> liftInt (`lengthIs_` x) y) xs
    lengthIs' _ _ _ = Fail
    lengthIs_ :: Integer -> ListTry a -> Try (ListTry a)
    lengthIs_ 0 Nil = Val Nil
    lengthIs_ _ Nil = Fail
    lengthIs_ n _ | n <= 0 = Fail
    lengthIs_ n (Cons x xs) = Cons x . lengthIs_ (n - 1) <$> xs

nth :: Function
nth = binaryVecArg2 nth'
  where
    nth' _ (DStringT xs) (DIntT y) =
        liftString
            (\x -> liftInt (fmap (AsDString . singleton) . (`nth_` x)) y)
            xs
    nth' _ (DListT xs) (DIntT y) =
        liftList (\x -> liftInt (`nth_` x) y) xs
    nth' _ _ _ = Fail
    nth_ :: Integer -> ListTry a -> Try a
    nth_ 0 (Cons x _) = Val x
    nth_ n (Cons _ xs) = xs >>= nth_ (n - 1)
    nth_ _ Nil = Fail

head' :: Function
head' = unary head''
  where
    head'' _ (DStringT xs) =
        liftString (fmap (AsDString . singleton) . head_) xs
    head'' _ (DListT xs) = liftList head_ xs
    head'' _ _ = Fail
    head_ :: ListTry a -> Try a
    head_ Nil = Fail
    head_ (Cons x _) = Val x

tail' :: Function
tail' = unary tail''
  where
    tail'' _ (DStringT xs) = liftString (fmap AsDString . tail_) xs
    tail'' _ (DListT xs) = liftList tail_ xs
    tail'' _ _ = Fail
    tail_ :: ListTry a -> Try (ListTry a)
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
        liftString12
            (uncons_ >=> \(ys, y) -> Val (AsDString <$> ys, AsDString [y]))
            xs
    uncons' _ (DListT xs) = liftList12 uncons_ xs
    uncons' _ _ = (Fail, Fail)
    uncons_ :: ListTry a -> Try (TryList a, a)
    uncons_ Nil = Fail
    uncons_ (Cons x xs) = Val (xs, x)

reverse' :: Function
reverse' = unary reverse''
  where
    reverse'' _ (DStringT xs) = liftString (fmap AsDString . reverse_ Nil) xs
    reverse'' _ (DListT xs) = liftList (reverse_ Nil) xs
    reverse'' _ _ = Fail
    reverse_ :: ListTry a -> ListTry a -> Try (ListTry a)
    reverse_ ys Nil = Val ys
    reverse_ ys (Cons x xs) = xs >>= reverse_ (Cons x (Val ys))

prefix :: Function
prefix = unary prefix'
  where
    prefix' i (DStringT xs) = liftString (fmap AsDString . prefix_ i) xs
    prefix' i (DListT xs) = liftList (prefix_ i) xs
    prefix' _ _ = Fail
    prefix_ :: Id -> ListTry a -> Try (ListTry a)
    prefix_ _ Nil = Val Nil
    prefix_ i (Cons x xs) =
        Choice (leftId i) (Val Nil) (Val . Cons x $ xs >>= prefix_ (rightId i))

suffix :: Function
suffix = unary suffix'
  where
    suffix' i (DStringT xs) = liftString (fmap AsDString . suffix_ i) xs
    suffix' i (DListT xs) = liftList (suffix_ i) xs
    suffix' _ _ = Fail
    suffix_ :: Id -> ListTry a -> Try (ListTry a)
    suffix_ _ Nil = Val Nil
    suffix_ i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix_ (rightId i))

subset :: Function
subset = unary subset'
  where
    subset' i (DStringT xs) =
        liftString (fmap AsDString . subset_ i) xs
    subset' i (DListT xs) = liftList (subset_ i) xs
    subset' _ _ = Fail
    subset_ :: Id -> ListTry a -> Try (ListTry a)
    subset_ i xs = Choice (leftId i) (Val Nil) (nonemptySubset (rightId i) xs)
    nonemptySubset _ Nil = Fail
    nonemptySubset i (Cons x xs) =
        Choice (leftId i) (Val $ singleton x) $
            xs >>= nonemptySubset (leftId (rightId i)) >>= \ys ->
                Choice (rightId (rightId i)) (Val ys) (Val . Cons x $ Val ys)

subsequence :: Function
subsequence = unary subsequence'
  where
    subsequence' i (DStringT xs) =
        liftString (fmap AsDString . subsequence_ i) xs
    subsequence' i (DListT xs) = liftList (subsequence_ i) xs
    subsequence' _ _ = Fail
    subsequence_ :: Id -> ListTry a -> Try (ListTry a)
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
join'' _ (DStringT xs) (DStringT ys) =
    liftString2 (\x y -> AsDString <$> join_ x y) xs ys
join'' i x@(DStringT _) y =
    join'' i x (DStringT (fromList . map Det . show <$> toTry y))
join'' i x y@(DStringT _) =
    join'' i (DStringT (fromList . map Det . show <$> toTry x)) y
join'' _ (DListT xs) (DListT ys) = liftList2 join_ xs ys
join'' _ _ _ = Fail
join_ :: ListTry a -> ListTry a -> Try (ListTry a)
join_ Nil ys = Val ys
join_ (Cons x xs) ys = Val . Cons x $ liftJoinM2 join_ xs (Val ys)

sum' :: Function
sum' = unary sum''
  where
    sum'' i (DListT xs) =
        liftList
            (tryFoldl (vec2Pad add') i . DIntT . Val $ Det 0)
            xs
    sum'' _ _ = Fail

product' :: Function
product' = unary product''
  where
    product'' i (DListT xs) =
        liftList
            (tryFoldl (vec2Fail mul') i . DIntT . Val $ Det 1)
            xs
    product'' _ _ = Fail

dot :: Function
dot = compose mul sum'

concat' :: Function
concat' = unary concat''
  where
    concat'' i (DListT xs) = xs >>= concat_ i
    concat'' _ _ = Fail
    concat_ _ Nil = Val . DListT $ Val Nil
    concat_ i (Cons x xs) = liftJoinM2 (tryFoldl join'' i) x xs
