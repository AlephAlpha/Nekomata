{-# LANGUAGE TypeFamilies #-}

module Nekomata.NonDet where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (MonadPlus (mplus, mzero))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Id for non-deterministic choice
newtype Id = Id Integer deriving (Eq, Ord, Show)

initId :: Id
initId = Id 1

leftId :: Id -> Id
leftId (Id i) = Id (2 * i)

rightId :: Id -> Id
rightId (Id i) = Id (2 * i + 1)

-- | A non-deterministic value
data Try a = Val a | Choice Id (Try a) (Try a) | Fail deriving (Show)

instance Functor Try where
    fmap f (Val x) = Val (f x)
    fmap f (Choice i t1 t2) = Choice i (fmap f t1) (fmap f t2)
    fmap _ Fail = Fail

instance Applicative Try where
    pure = Val
    Val f <*> t = fmap f t
    Choice i t1 t2 <*> t = Choice i (t1 <*> t) (t2 <*> t)
    Fail <*> _ = Fail

instance Monad Try where
    Val x >>= f = f x
    Choice i t1 t2 >>= f = Choice i (t1 >>= f) (t2 >>= f)
    Fail >>= _ = Fail

-- | A newtype wrapper for functions from @Id@ to @Try@
newtype TryF a = TryF {runTryF :: Id -> Try a}

instance Functor TryF where
    fmap f (TryF g) = TryF (fmap f . g)

{- | This @Applicative@ instance does not strictly follow the laws.
It only satisfies the laws up to 1-to-1 correspondence between @Id@s.
-}
instance Applicative TryF where
    pure x = TryF (\_ -> Val x)

    TryF f <*> TryF g = TryF (\i -> f (leftId i) <*> g (rightId i))

{- | This @Monad@ instance does not strictly follow the laws.
It only satisfies the laws up to 1-to-1 correspondence between @Id@s.
-}
instance Monad TryF where
    TryF f >>= g = TryF (\i -> f (leftId i) >>= \x -> runTryF (g x) (rightId i))

instance Alternative TryF where
    empty = TryF (\_ -> Fail)
    TryF f <|> TryF g = TryF (\i -> Choice i (f (leftId i)) (g (rightId i)))

instance MonadPlus TryF where
    mzero = empty
    mplus = (<|>)

-- | A wrapper for deterministic values
newtype Det a = Det a deriving (Eq, Show)

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

instance NonDet a => NonDet (Try a) where
    type Value (Try a) = Value a
    fromValue = Val . fromValue
    toTry t = t >>= toTry

data Decision = ChooseLeft | ChooseRight deriving (Eq, Show)

newtype Decisions = Decisions (Map Id Decision) deriving (Show)

getChoice :: Id -> Decisions -> Maybe Decision
getChoice i (Decisions m) = Map.lookup i m

setChoice :: Id -> Decision -> Decisions -> Decisions
setChoice i d (Decisions m) = Decisions (Map.insert i d m)

clearChoice :: Id -> Decisions -> Decisions
clearChoice i (Decisions m) = Decisions (Map.delete i m)

initDecisions :: Decisions
initDecisions = Decisions Map.empty

values :: Alternative m => Decisions -> Try a -> m a
values _ (Val x) = pure x
values ds (Choice i t1 t2) = case getChoice i ds of
    Just ChooseLeft -> values ds t1
    Just ChooseRight -> values ds t2
    Nothing -> values (setChoice i ChooseLeft ds) t1 <|> values (setChoice i ChooseRight ds) t2
values _ Fail = empty

allValues :: Decisions -> Try a -> [a]
allValues = values

oneValue :: Decisions -> Try a -> Maybe a
oneValue = values
