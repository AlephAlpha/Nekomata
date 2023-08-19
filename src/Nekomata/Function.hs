module Nekomata.Function where

import Control.Monad (liftM2)
import Nekomata.Data
import Nekomata.NonDet

{- | The stack of Nekomata

It is infinite so that we don't need to handle the special case of empty stack
-}
data Stack = TryData :+ Stack

infixr 5 :+

-- | Initialize the stack with a cycled list of values
initStack :: [TryData] -> Stack
initStack [] = foldr (:+) undefined $ repeat Fail
initStack xs = foldr (:+) undefined $ cycle xs

-- | The top value of the stack
top :: Stack -> TryData
top (x :+ _) = x

-- | Pop the top value of the stack
pop :: Stack -> Stack
pop (_ :+ s) = s

-- | Take the top @n@ values of the stack
takeStack :: Int -> Stack -> [TryData]
takeStack 0 _ = []
takeStack n (x :+ s) = x : takeStack (n - 1) s

-- | Drop the top @n@ values of the stack
dropStack :: Int -> Stack -> Stack
dropStack 0 s = s
dropStack n (_ :+ s) = dropStack (n - 1) s

-- | Prepend a list of values to the stack
prepend :: [TryData] -> Stack -> Stack
prepend xs s = foldr (:+) s xs

-- | Convert a @Try Stack@ to a @Stack@
tryStack :: Try Stack -> Stack
tryStack s = (s >>= top) :+ tryStack (pop <$> s)

-- | The arity of a function
data Arity = Arity
    { inArity :: Int
    -- ^ The number of arguments
    , outArity :: Int
    -- ^ The number of results
    }
    deriving (Eq, Ord)

instance Show Arity where
    show (Arity in' out') = show in' ++ " -> " ++ show out'

-- | Compose two arities
composeArity :: Arity -> Arity -> Arity
composeArity (Arity in1 out1) (Arity in2 out2) =
    if in2 <= out1
        then Arity in1 (out1 + out2 - in2)
        else Arity (in1 + in2 - out1) out2

-- | A Nekomata function
data Function = Function
    { arity :: Arity
    , apply :: Id -> Stack -> Stack
    }

-- | The identity function
identity :: Function
identity = Function (Arity 0 0) $ \_ s -> s

-- | Compose two functions
(.*) :: Function -> Function -> Function
(.*) f g =
    Function (composeArity (arity f) (arity g))
        $ \i -> apply g (leftId i) . apply f (rightId i)

infixr 9 .*

-- | Convert a nullary function to a Nekomata function
nullary :: (Id -> TryData) -> Function
nullary f = Function (Arity 0 1) $ \i s -> f i :+ s

-- | Convert a unary function to a Nekomata function
unary :: (Id -> DataTry -> TryData) -> Function
unary f = Function (Arity 1 1) $ \i (x :+ s) -> (x >>= f i) :+ s

-- | Convert a binary function to a Nekomata function
binary :: (Id -> DataTry -> DataTry -> TryData) -> Function
binary f =
    Function (Arity 2 1)
        $ \i (x :+ y :+ s) -> liftJoinM2 (f i) y x :+ s

-- | Convert a unary function that returns two values to a Nekomata function
unary2 :: (Id -> DataTry -> (TryData, TryData)) -> Function
unary2 f =
    Function (Arity 1 2)
        $ \i (x :+ s) -> let z = f i <$> x in (z >>= snd) :+ (z >>= fst) :+ s

-- | Convert a binary function that returns two values to a Nekomata function
binary2 :: (Id -> DataTry -> DataTry -> (TryData, TryData)) -> Function
binary2 f =
    Function (Arity 2 2)
        $ \i (x :+ y :+ s) ->
            let z = liftM2 (f i) y x
             in (z >>= snd) :+ (z >>= fst) :+ s

-- | Convert a predicate to a Nekomata function
predicate :: (Id -> DataTry -> Try Bool) -> Function
predicate f = unary $ \i x -> f i x >>= \b -> if b then Val x else Fail

{- | Convert a binary predicate to a Nekomata function

When the predicate returns 'True', the first argument is returned.
-}
predicate2 :: (Id -> DataTry -> DataTry -> Try Bool) -> Function
predicate2 f = binary $ \i x y -> f i x y >>= \b -> if b then Val x else Fail

-- | Convert a constant to a Nekomata function
constant :: (ToTryData a) => a -> Function
constant = nullary . const . toTryData

-- | Convert and vectorize a unary function
unaryVec :: (Id -> DataTry -> TryData) -> Function
unaryVec = unary . vec1

-- | Convert and vectorize a unary function that returns two values
unary2Vec :: (Id -> DataTry -> (TryData, TryData)) -> Function
unary2Vec = unary2 . vec12

-- | Convert and vectorize a binary function with padding
binaryVecPad :: (Id -> DataTry -> DataTry -> TryData) -> Function
binaryVecPad = binary . vec2Pad

-- | Convert and vectorize a binary function with failure on mismatched lengths
binaryVecFail :: (Id -> DataTry -> DataTry -> TryData) -> Function
binaryVecFail = binary . vec2Fail

-- | Convert and vectorize a binary function with outer product
binaryVecOuter :: (Id -> DataTry -> DataTry -> TryData) -> Function
binaryVecOuter = binary . vec2Outer

{- | Convert a binary function to a Nekomata function
and vectorize the first argument
-}
binaryVecArg1 :: (Id -> DataTry -> DataTry -> TryData) -> Function
binaryVecArg1 = binary . vec2Arg1

{- | Convert a binary function to a Nekomata function
and vectorize the second argument
-}
binaryVecArg2 :: (Id -> DataTry -> DataTry -> TryData) -> Function
binaryVecArg2 = binary . vec2Arg2

-- | Convert and vectorize a predicate to a Nekomata function
predicateVec :: (Id -> DataTry -> Try Bool) -> Function
predicateVec f = unaryVec $ \i x -> f i x >>= \b -> if b then Val x else Fail

{- | Convert and vectorize a binary predicate to a Nekomata function

When the predicate returns 'True', the first argument is returned.
-}
predicateVec2 :: (Id -> DataTry -> DataTry -> Try Bool) -> Function
predicateVec2 f = binaryVecFail
    $ \i x y -> f i x y >>= \b -> if b then Val x else Fail
