{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Nekomata.NonDet where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (second)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)

-- | Id for non-deterministic choice
newtype Id = Id Integer deriving (Eq, Ord, Show)

initId :: Id
initId = Id 1

leftId :: Id -> Id
leftId (Id i) = Id (2 * i)

rightId :: Id -> Id
rightId (Id i) = Id (2 * i + 1)

-- | A non-deterministic value
data Try a
    = Val a
    | Choice Id (Try a) (Try a)
    | Fail
    | Cut (Decisions -> (Decisions, Try a))

instance Functor Try where
    fmap f (Val x) = Val (f x)
    fmap f (Choice i t1 t2) = Choice i (fmap f t1) (fmap f t2)
    fmap _ Fail = Fail
    fmap f (Cut g) = Cut (second (fmap f) . g)

instance Applicative Try where
    pure = Val
    Val f <*> t = fmap f t
    Choice i t1 t2 <*> t = Choice i (t1 <*> t) (t2 <*> t)
    Fail <*> _ = Fail
    Cut g <*> t = Cut (second (<*> t) . g)

instance Monad Try where
    Val x >>= f = f x
    Choice i t1 t2 >>= f = Choice i (t1 >>= f) (t2 >>= f)
    Fail >>= _ = Fail
    Cut g >>= f = Cut (second (>>= f) . g)

-- | A wrapper for deterministic tryValues
newtype Det a = Det {unDet :: a}
    deriving (Eq, Ord, Show)

instance Functor Det where
    fmap f (Det x) = Det (f x)

instance Applicative Det where
    pure = Det
    Det f <*> Det x = Det (f x)

instance Monad Det where
    Det x >>= f = f x

-- | A type class for nested non-deterministic computations
class NonDet a where
    type Value a
    fromValue :: Value a -> a
    toTry :: a -> Try (Value a)

instance NonDet (Det a) where
    type Value (Det a) = a
    fromValue = Det
    toTry (Det x) = Val x

instance (NonDet a) => NonDet (Try a) where
    type Value (Try a) = Value a
    fromValue = Val . fromValue
    toTry = (>>= toTry)

-- | A decision for non-deterministic choice
data Decision = ChooseLeft | ChooseRight deriving (Eq, Show)

-- | A map from @Id@s to decisions
newtype Decisions = Decisions (Map Id Decision) deriving (Show)

-- | Get the decision for the given @Id@
getChoice :: Id -> Decisions -> Maybe Decision
getChoice i (Decisions m) = Map.lookup i m

-- | Set the decision for the given @Id@
setChoice :: Id -> Decision -> Decisions -> Decisions
setChoice i d (Decisions m) = Decisions $ Map.insert i d m

-- | Clear the decision for the given @Id@
clearChoice :: Id -> Decisions -> Decisions
clearChoice i (Decisions m) = Decisions $ Map.delete i m

-- | Initialize the map with no decisions
initDecisions :: Decisions
initDecisions = Decisions Map.empty

-- | Find all values and the corresponding decisions of a of a @Try@
tryValues :: (Alternative m) => Decisions -> Try a -> m (Decisions, a)
tryValues ds (Val x) = pure (ds, x)
tryValues ds (Choice i t1 t2) = case getChoice i ds of
    Just ChooseLeft -> tryValues ds t1
    Just ChooseRight -> tryValues ds t2
    Nothing ->
        tryValues (setChoice i ChooseLeft ds) t1
            <|> tryValues (setChoice i ChooseRight ds) t2
tryValues _ Fail = empty
tryValues ds (Cut g) = let (ds', t) = g ds in tryValues ds' t

{- | A @Maybe@ wrapper whose @Alternative@ instance chooses the rightmost @Just@
value
-}
newtype LastAlt a = LastAlt {getLastAlt :: Maybe a}
    deriving (Eq, Ord, Show, Functor, Applicative, Monad)

instance Alternative LastAlt where
    empty = LastAlt Nothing
    LastAlt l <|> LastAlt Nothing = LastAlt l
    LastAlt _ <|> r = r

-- | Find all values of a @NonDet@
values :: (NonDet a, Alternative m) => Decisions -> a -> m (Value a)
values ds = fmap snd . tryValues ds . toTry

-- | Find the first value and the corresponding decisions of a @NonDet@
firstValue :: (NonDet a) => Decisions -> a -> Maybe (Decisions, Value a)
firstValue ds = tryValues ds . toTry

-- | Find the last value and the corresponding decisions of a @NonDet@
lastValue :: (NonDet a) => Decisions -> a -> Maybe (Decisions, Value a)
lastValue ds = getLastAlt . tryValues ds . toTry

-- | Count all values of a @Try@
countTryValues :: Decisions -> Try a -> Integer
countTryValues _ (Val _) = 1
countTryValues ds (Choice i t1 t2) = case getChoice i ds of
    Just ChooseLeft -> countTryValues ds t1
    Just ChooseRight -> countTryValues ds t2
    Nothing ->
        countTryValues (setChoice i ChooseLeft ds) t1
            + countTryValues (setChoice i ChooseRight ds) t2
countTryValues _ Fail = 0
countTryValues ds (Cut g) = let (ds', t) = g ds in countTryValues ds' t

-- | Count all values of a @NonDet@
countValues :: (NonDet a) => Decisions -> a -> Integer
countValues ds = countTryValues ds . toTry

-- | Check if a @NonDet@ has a value
hasValue :: (NonDet a) => Decisions -> a -> Bool
hasValue ds = isJust . values ds
