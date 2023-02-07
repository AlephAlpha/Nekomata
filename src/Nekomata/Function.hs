module Nekomata.Function where

import Nekomata.Data
import Nekomata.NonDet

{- | The stack of Nekomata

It is infinite so that we don't need to handle the special case of empty stack
-}
data Stack = TryData :+ Stack

infixr 5 :+

-- | Initialize the stack with a cycled list of values
initStack :: [TryData] -> Stack
initStack = foldr (:+) undefined . cycle

-- | The arity of a function
data Arity = Arity
    { inArity :: Int
    -- ^ The number of arguments
    , outArity :: Int
    -- ^ The number of results
    }
    deriving (Eq, Show)

-- | Compose two arities
composeArity :: Arity -> Arity -> Arity
composeArity (Arity in1 out1) (Arity in2 out2) =
    if in1 <= out2
        then Arity in2 (out1 + out2 - in1)
        else Arity (in1 - out2 + in2) out1

-- | A Nekomata function
data Function = Function
    { arity :: Arity
    , apply :: Id -> Stack -> Stack
    }

-- | Compose two functions
compose :: Function -> Function -> Function
compose f g =
    Function
        (composeArity (arity f) (arity g))
        (\i -> apply f (leftId i) . apply g (rightId i))

-- | Convert a nullary function to a Nekomata function
nullary :: (Id -> TryData) -> Function
nullary f = Function (Arity 0 1) (\i s -> f i :+ s)

-- | Convert a unary function to a Nekomata function
unary :: (Id -> DataTry -> TryData) -> Function
unary f = Function (Arity 1 1) (\i (x :+ s) -> (x >>= f i) :+ s)

-- | Convert a binary function to a Nekomata function
binary :: (Id -> DataTry -> DataTry -> TryData) -> Function
binary f =
    Function
        (Arity 2 1)
        (\i (x :+ y :+ s) -> (x >>= \x' -> y >>= f i x') :+ s)

-- | Convert a unary function that returns two values to a Nekomata function
unary2 :: (Id -> DataTry -> (TryData, TryData)) -> Function
unary2 f =
    Function
        (Arity 1 2)
        (\i (x :+ s) -> (x >>= fst . f i) :+ (x >>= snd . f i) :+ s)

-- | Convert a binary function that returns two values to a Nekomata function
binary2 :: (Id -> DataTry -> DataTry -> (TryData, TryData)) -> Function
binary2 f =
    Function
        (Arity 2 2)
        ( \i (x :+ y :+ s) ->
            (x >>= \x' -> y >>= fst . f i x')
                :+ (x >>= \x' -> y >>= snd . f i x')
                :+ s
        )

-- | Convert a predicate to a Nekomata function
predicate :: (DataTry -> Try Bool) -> Function
predicate f = unary (\_ x -> f x >>= \b -> if b then Val x else Fail)

-- | Convert a constant to a Nekomata function
constant :: Data -> Function
constant = nullary . const . fromValue

-- | Convert and vectorize a unary function
unaryVec :: (Id -> DataTry -> TryData) -> Function
unaryVec f = unary f'
  where
    f' i (DListT xs) = Val (DListT (fmap (>>= f' i) <$> xs))
    f' i x = f i x
