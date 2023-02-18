module Nekomata.Builtin (
    Builtin (..),
    BuiltinNotFoundError (..),
    builtins,
    builtinMap,
    builtinShortMap,
    info,
) where

import Control.Monad (join)
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
        "nonEmpty"
        'N'
        nonEmpty'
        "Check if a list or string is non-empty. \n\
        \If it is, push the list or string itself, otherwise fail."
    , Builtin
        "nonZero"
        'Z'
        nonZero
        "Check if an integer is non-zero. \n\
        \If it is, push the integer itself, otherwise fail."
    , Builtin
        "positive"
        'P'
        positive
        "Check if an integer is positive. \n\
        \If it is, push the integer itself, otherwise fail."
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
        "anyOf"
        '~'
        anyOf'
        "Choose an element from a list or a character from a string. \n\
        \This function is non-deterministic."
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
        "Get the first element of a list and the rest of the list."
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
        "Get a subset of a list or a string. \n\
        \This function is non-deterministic."
    , Builtin
        "join"
        '⧺'
        join'
        "Concatenate two lists or two strings.\n\
        \If one of the arguments is a string, \
        \the other argument is converted to a string as well."
    ]

-- | The map from names to builtin functions
builtinMap :: Map String Builtin
builtinMap = Map.fromList [(name b, b) | b <- builtins]

-- | The map from short names to builtin functions
builtinShortMap :: Map Char Builtin
builtinShortMap = Map.fromList [(short b, b) | b <- builtins]

-- | An error that occurs when a builtin function is not found
data BuiltinNotFoundError = BuiltinNotFound String | BuiltinShortNotFound Char

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
oneValue = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> toTryData . maybe Fail Val $ values ds x) :+ s

countValues' :: Function
countValues' = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> toTryData $ countValues ds x) :+ s

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

nonEmpty' :: Function
nonEmpty' = predicate nonEmpty''
  where
    nonEmpty'' _ (DListT x) = nonEmpty_ <$> x
    nonEmpty'' _ (DStringT x) = nonEmpty_ <$> x
    nonEmpty'' _ _ = Fail
    nonEmpty_ :: ListTry a -> Bool
    nonEmpty_ (Cons _ _) = True
    nonEmpty_ Nil = False

nonZero :: Function
nonZero = predicate nonZero'
  where
    nonZero' _ (DIntT x) = (/= 0) . fromDet <$> x
    nonZero' _ _ = Fail

positive :: Function
positive = predicate positive'
  where
    positive' _ (DIntT x) = (> 0) . fromDet <$> x
    positive' _ _ = Fail

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
  where
    add' _ (DIntT x) (DIntT y) = liftInt2 (+) x y
    add' _ _ _ = Fail

sub :: Function
sub = binaryVecPad sub'
  where
    sub' _ (DIntT x) (DIntT y) = liftInt2 (-) x y
    sub' _ _ _ = Fail

mul :: Function
mul = binaryVecFail mul'
  where
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
    \i -> mapM toTryData [0 :: Integer ..] >>= anyOf i . fromList

integer :: Function
integer = nullary $
    \i -> mapM toTryData integers >>= anyOf i . fromList
  where
    integers = (0 :: Integer) : [y | x <- [1 ..], y <- [x, -x]]

-- List functions

anyOf' :: Function
anyOf' = unary anyOf''
  where
    anyOf'' i (DStringT xs) = Val . DStringT $ xs >>= fmap singleton . anyOf i
    anyOf'' i (DListT xs) = join (xs >>= anyOf i)
    anyOf'' _ _ = Fail

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
    subset_ _ Nil = Val Nil
    subset_ i (Cons x xs) =
        Choice
            i
            (Val . Cons x $ xs >>= subset_ (rightId i))
            (xs >>= subset_ (rightId i))

join' :: Function
join' = binary join''
  where
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
