module Nekomata.Builtin (
    Builtin (..),
    BuiltinNotFoundError (..),
    builtins,
    builtinMap,
    builtinShortMap,
    info,
) where

import Control.Monad (join)
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
        ++ "): "
        ++ help b

-- | The list of all builtin functions
builtins :: [Builtin]
builtins =
    [ Builtin
        "choice"
        '?'
        choice
        "Choose between two values. \
        \This function is non-deterministic."
    , Builtin
        "fail"
        '!'
        fail'
        "Push a value that always fails."
    , Builtin "drop" 'd' drop' "Drop the top value of the stack."
    , Builtin "dup" 'D' dup "Duplicate the top value of the stack."
    , Builtin "swap" 'S' swap "Swap the top two values of the stack."
    , Builtin
        "eq"
        '='
        eq
        "Check if two values are equal."
    , Builtin
        "ne"
        '≠'
        ne
        "Check if two values are not equal."
    , Builtin
        "neg"
        '_'
        neg
        "Negate an integer. \
        \This function is automatically vectorized."
    , Builtin
        "add"
        '+'
        add
        "Add two integers. \
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "sub"
        '-'
        sub
        "Subtract two integers. \
        \This function is automatically vectorized with padding zeros."
    , Builtin
        "mul"
        '*'
        mul
        "Multiply two integers. \
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "div"
        '÷'
        div'
        "Integer division of two integers. \
        \Result is rounded towards negative infinity. \
        \Fails when the divisor is zero. \
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "mod"
        '%'
        mod'
        "Modulo two integers. \
        \Fails when the divisor is zero. \
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "divExact"
        '∣'
        divExact
        "Divide two integers. \
        \Fails when the divisor is zero or \
        \the result is not an exact integer. \
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
    , Builtin
        "anyOf"
        'A'
        anyOf'
        "Choose an element from a list or a character from a string. \
        \This function is non-deterministic."
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
        "Cannot find builtin function with full name \"\\" ++ name' ++ "\"."
    show (BuiltinShortNotFound short') =
        "Cannot find builtin function with short name '" ++ [short'] ++ "'."

-- Basic functions

choice :: Function
choice = Function (Arity 2 1) $ \i (x :+ y :+ s) -> Choice i x y :+ s

fail' :: Function
fail' = constant (Fail :: TryData)

-- Stack manipulation

drop' :: Function
drop' = Function (Arity 1 0) $ \_ (_ :+ s) -> s

dup :: Function
dup = Function (Arity 1 2) $ \_ (x :+ s) -> x :+ x :+ s

swap :: Function
swap = Function (Arity 2 2) $ \_ (x :+ y :+ s) -> y :+ x :+ s

-- Comparison functions

eq :: Function
eq = predicate2 $ \_ x y -> tryEq x y

ne :: Function
ne = predicate2 $ \_ x y -> tryNe x y

-- Math functions

neg :: Function
neg = unaryVec neg'
  where
    neg' :: Id -> DataTry -> TryData
    neg' _ (DIntT x) = liftInt negate x
    neg' _ _ = Fail

add :: Function
add = binaryVecPad add'
  where
    add' :: Id -> DataTry -> DataTry -> TryData
    add' _ (DIntT x) (DIntT y) = liftInt2 (+) x y
    add' _ _ _ = Fail

sub :: Function
sub = binaryVecPad sub'
  where
    sub' :: Id -> DataTry -> DataTry -> TryData
    sub' _ (DIntT x) (DIntT y) = liftInt2 (-) x y
    sub' _ _ _ = Fail

mul :: Function
mul = binaryVecFail mul'
  where
    mul' :: Id -> DataTry -> DataTry -> TryData
    mul' _ (DIntT x) (DIntT y) = liftInt2 (*) x y
    mul' _ _ _ = Fail

div' :: Function
div' = binaryVecFail div''
  where
    div'' :: Id -> DataTry -> DataTry -> TryData
    div'' _ (DIntT x) (DIntT y) = liftInt2 div_ x y
    div'' _ _ _ = Fail
    div_ _ 0 = Fail
    div_ x y = Val $ x `div` y

mod' :: Function
mod' = binaryVecFail mod''
  where
    mod'' :: Id -> DataTry -> DataTry -> TryData
    mod'' _ (DIntT x) (DIntT y) = liftInt2 mod_ x y
    mod'' _ _ _ = Fail
    mod_ _ 0 = Fail
    mod_ x y = Val $ x `mod` y

divExact :: Function
divExact = binaryVecFail divExact'
  where
    divExact' :: Id -> DataTry -> DataTry -> TryData
    divExact' _ (DIntT x) (DIntT y) = liftInt2 divExact_ x y
    divExact' _ _ _ = Fail
    divExact_ x y = if y /= 0 && x `mod` y == 0 then Val $ x `div` y else Fail

-- List functions

anyOf' :: Function
anyOf' = unary anyOf''
  where
    anyOf'' :: Id -> DataTry -> TryData
    anyOf'' i (DStringT xs) = Val . DStringT $ xs >>= anyOf i . singleton
    anyOf'' i (DListT xs) = join (xs >>= anyOf i)
    anyOf'' _ _ = Fail
