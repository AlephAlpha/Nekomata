module Nekomata.Builtin (
    Builtin (..),
    builtins,
    builtinMap,
    info,
) where

import Control.Monad (liftM2)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Utils

-- | A builtin function in Nekomata
data Builtin = Builtin
    { name :: String
    , func :: Function
    , help :: String
    }

-- | Get the info string for a builtin function
info :: Builtin -> String
info b =
    let Arity inA outa = arity (func b)
     in name b ++ " (" ++ show inA ++ " -> " ++ show outa ++ "): " ++ help b

-- | The list of all builtin functions
builtins :: [Builtin]
builtins =
    [ Builtin
        "choice"
        choice
        "Choose between two values. \
        \This function is non-deterministic."
    , Builtin
        "fail"
        fail'
        "Push a value that always fails."
    , Builtin "drop" drop' "Drop the top value of the stack."
    , Builtin "dup" dup "Duplicate the top value of the stack."
    , Builtin "swap" swap "Swap the top two values of the stack."
    , Builtin
        "neg"
        neg
        "Negate an integer. \
        \This function is automatically vectorized."
    , Builtin
        "add"
        add
        "Add two integers. \
        \This function is automatically vectorized. \
        \When the two lists are of different lengths, \
        \the shorter list is padded with zeros."
    , Builtin
        "sub"
        sub
        "Subtract two integers. \
        \This function is automatically vectorized. \
        \When the two lists are of different lengths, \
        \the shorter list is padded with zeros."
    , Builtin
        "mul"
        mul
        "Multiply two integers. \
        \This function is automatically vectorized. \
        \Fails when the two lists are of different lengths."
    , Builtin
        "div"
        div'
        "Integer division of two integers. \
        \Fails when the divisor is zero. \
        \This function is automatically vectorized. \
        \Fails when the two lists are of different lengths."
    , Builtin
        "mod"
        mod'
        "Modulo two integers. \
        \Fails when the divisor is zero. \
        \This function is automatically vectorized. \
        \Fails when the two lists are of different lengths."
    , Builtin
        "divExact"
        divExact
        "Divide two integers. \
        \Fails when the divisor is zero or \
        \the result is not an exact integer. \
        \This function is automatically vectorized. \
        \Fails when the two lists are of different lengths."
    ]

-- | The map of all builtin functions
builtinMap :: Map String Builtin
builtinMap = Map.fromList [(name b, b) | b <- builtins]

-- Basic functions

choice :: Function
choice = Function (Arity 2 1) $ \i (x :+ y :+ s) -> Choice i x y :+ s

fail' :: Function
fail' = Function (Arity 0 1) $ \_ s -> Fail :+ s

-- Stack manipulation

drop' :: Function
drop' = Function (Arity 1 0) $ \_ (_ :+ s) -> s

dup :: Function
dup = Function (Arity 1 2) $ \_ (x :+ s) -> x :+ x :+ s

swap :: Function
swap = Function (Arity 2 2) $ \_ (x :+ y :+ s) -> y :+ x :+ s

-- Math functions

neg :: Function
neg = unaryVec neg'
  where
    neg' :: Id -> DataTry -> TryData
    neg' _ (DIntT x) = Val . DIntT $ fmap2 negate x
    neg' _ _ = Fail

add :: Function
add = binaryVecPad add'
  where
    add' :: Id -> DataTry -> DataTry -> TryData
    add' _ (DIntT x) (DIntT y) = Val . DIntT $ liftM2 (liftM2 (+)) x y
    add' _ _ _ = Fail

sub :: Function
sub = binaryVecPad sub'
  where
    sub' :: Id -> DataTry -> DataTry -> TryData
    sub' _ (DIntT x) (DIntT y) = Val . DIntT $ liftM2 (liftM2 (-)) x y
    sub' _ _ _ = Fail

mul :: Function
mul = binaryVecFail mul'
  where
    mul' :: Id -> DataTry -> DataTry -> TryData
    mul' _ (DIntT x) (DIntT y) = Val . DIntT $ liftM2 (liftM2 (*)) x y
    mul' _ _ _ = Fail

div' :: Function
div' = binaryVecFail div''
  where
    div'' :: Id -> DataTry -> DataTry -> TryData
    div'' _ (DIntT x) (DIntT y) = Val . DIntT $ liftJoinM2 div_ x y
    div'' _ _ _ = Fail
    div_ _ (Det 0) = Fail
    div_ (Det x) (Det y) = Val . Det $ x `div` y

mod' :: Function
mod' = binaryVecFail mod''
  where
    mod'' :: Id -> DataTry -> DataTry -> TryData
    mod'' _ (DIntT x) (DIntT y) = Val . DIntT $ liftJoinM2 mod_ x y
    mod'' _ _ _ = Fail
    mod_ _ (Det 0) = Fail
    mod_ (Det x) (Det y) = Val . Det $ x `mod` y

divExact :: Function
divExact = binaryVecFail divExact'
  where
    divExact' :: Id -> DataTry -> DataTry -> TryData
    divExact' _ (DIntT x) (DIntT y) = Val . DIntT $ liftJoinM2 divExact_ x y
    divExact' _ _ _ = Fail
    divExact_ (Det x) (Det y) =
        if y /= 0 && x `mod` y == 0
            then Val . Det $ x `div` y
            else Fail
