module Nekomata.Builtin (
    Builtin (..),
    builtins,
    builtinMap,
    info,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Function
import Nekomata.NonDet

-- | A builtin function in Nekomata
data Builtin = Builtin
    { name :: String
    , func :: Function
    , help :: String
    }

-- | Get the info string for a builtin function
info :: Builtin -> String
info b =
    name b
        ++ " ("
        ++ show (inArity (arity (func b)))
        ++ " -> "
        ++ show (outArity (arity (func b)))
        ++ "): "
        ++ help b

-- | The list of all builtin functions
builtins :: [Builtin]
builtins =
    [ Builtin
        "choice"
        choice
        "Choose between two values. \
        \This function is non-deterministic."
    , Builtin "drop" drop' "Drop the top value of the stack"
    , Builtin "dup" dup "Duplicate the top value of the stack"
    ]

-- | The map of all builtin functions
builtinMap :: Map String Builtin
builtinMap = Map.fromList [(name b, b) | b <- builtins]

choice :: Function
choice = Function (Arity 2 1) (\i (x :+ y :+ s) -> Choice i x y :+ s)

drop' :: Function
drop' = Function (Arity 1 0) (\_ (_ :+ s) -> s)

dup :: Function
dup = Function (Arity 1 2) (\_ (x :+ s) -> x :+ x :+ s)
