{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Nekomata.Data where

import Control.Monad (join, liftM2)
import Nekomata.NonDet

-- | A helper function to lift a binary function to a monad
liftJoinM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoinM2 f x y = join $ liftM2 f x y

data ListTry a = Nil | Cons a (TryList a)

-- | A non-deterministic list
type TryList a = Try (ListTry a)

instance Functor ListTry where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap (fmap f) xs)

-- | Convert from @[a]@ to @ListTry a@
fromList :: [a] -> ListTry a
fromList [] = Nil
fromList (x : xs) = Cons x (Val $ fromList xs)

{- | Zip two @TryList@s with a function

If one of the lists is shorter, the remaining elements of the longer list
are padded to the result.
-}
zipWithPad ::
    (Id -> a -> a -> Try a) ->
    Id ->
    ListTry (Try a) ->
    ListTry (Try a) ->
    ListTry (Try a)
zipWithPad _ _ Nil xs = xs
zipWithPad _ _ xs Nil = xs
zipWithPad f i (Cons x xs) (Cons y ys) =
    Cons
        (liftJoinM2 (f (leftId i)) x y)
        (liftM2 (zipWithPad f (rightId i)) xs ys)

{- | Zip two @TryList@s with a function

Fail if the lists have different lengths.
-}
zipWithFail ::
    (Id -> a -> b -> Try c) ->
    Id ->
    ListTry (Try a) ->
    ListTry (Try b) ->
    TryList (Try c)
zipWithFail _ _ Nil Nil = Val Nil
zipWithFail f i (Cons x xs) (Cons y ys) =
    Val $
        Cons
            (liftJoinM2 (f (leftId i)) x y)
            (liftJoinM2 (zipWithFail f (rightId i)) xs ys)
zipWithFail _ _ _ _ = Fail

-- | Choose an element from a @TryList@
anyOf :: Id -> ListTry a -> Try a
anyOf _ Nil = Fail
anyOf i (Cons x xs) = Choice (leftId i) (Val x) (xs >>= anyOf (rightId i))

-- | A singleton list
singleton :: a -> ListTry a
singleton x = Cons x (Val Nil)

tryMap :: (Id -> a -> Try b) -> Id -> ListTry (Try a) -> ListTry (Try b)
tryMap _ _ Nil = Nil
tryMap f i (Cons x xs) = Cons (x >>= f (leftId i)) (tryMap f (rightId i) <$> xs)

instance NonDet a => NonDet (ListTry a) where
    type Value (ListTry a) = [Value a]
    fromValue = fromList . map fromValue
    toTry Nil = Val []
    toTry (Cons x xs) = (:) <$> toTry x <*> toTry xs

-- | Nekomata's data type (deterministic)
data Data
    = DInt Integer
    | DString String
    | DList [Data]
    deriving (Eq)

instance Show Data where
    show (DInt x) = show x
    show (DString x) = show x
    show (DList x) = show x

data DataTry
    = DIntT (Try (Det Integer))
    | DStringT (TryList (Det Char))
    | DListT (TryList TryData)

-- | Nekomata's data type (non-deterministic)
type TryData = Try DataTry

instance NonDet DataTry where
    type Value DataTry = Data
    fromValue (DInt x) = DIntT $ fromValue x
    fromValue (DString x) = DStringT $ fromValue x
    fromValue (DList x) = DListT $ fromValue x
    toTry (DIntT t) = DInt <$> toTry t
    toTry (DStringT t) = DString <$> toTry t
    toTry (DListT t) = DList <$> toTry t

-- | A helper class for lifting functions to @TryData@
class ToTryData a where
    toTryData :: a -> TryData

-- | A wrapper to avoid overlapping instances
newtype AsDString a = AsDString {fromDString :: a}

instance ToTryData a => ToTryData (Det a) where
    toTryData = toTryData . fromDet

instance ToTryData a => ToTryData (Try a) where
    toTryData = (>>= toTryData)

instance ToTryData a => ToTryData (Maybe a) where
    toTryData = maybe Fail toTryData

instance ToTryData Integer where
    toTryData = Val . DIntT . Val . Det

instance ToTryData (AsDString String) where
    toTryData = Val . DStringT . fromValue . fromDString

instance ToTryData (AsDString (ListTry Char)) where
    toTryData = Val . DStringT . Val . fmap Det . fromDString

instance ToTryData a => ToTryData [a] where
    toTryData = Val . DListT . Val . fromList . map toTryData

instance ToTryData a => ToTryData (ListTry a) where
    toTryData = Val . DListT . Val . fmap toTryData

instance ToTryData Data where
    toTryData = Val . fromValue

instance ToTryData DataTry where
    toTryData = Val

-- | Lift a unary integer function to @TryData@
liftInt :: ToTryData a => (Integer -> a) -> (Try (Det Integer) -> TryData)
liftInt f = toTryData . fmap f . toTry

-- | Lift a binary integer function to @TryData@
liftInt2 ::
    ToTryData a =>
    (Integer -> Integer -> a) ->
    (Try (Det Integer) -> Try (Det Integer) -> TryData)
liftInt2 f x y = toTryData $ liftM2 f (toTry x) (toTry y)

-- | Lift a unary string function to @TryData@
liftString ::
    ToTryData a =>
    (ListTry Char -> a) ->
    (TryList (Det Char) -> TryData)
liftString f = toTryData . fmap (f . fmap fromDet)

-- | Lift a binary string function to @TryData@
liftString2 ::
    ToTryData a =>
    (ListTry Char -> ListTry Char -> a) ->
    (TryList (Det Char) -> TryList (Det Char) -> TryData)
liftString2 f x y =
    toTryData $
        liftM2 f (fmap fromDet <$> x) (fmap fromDet <$> y)

-- | Lift a unary list function to @TryData@
liftList ::
    ToTryData a =>
    (ListTry TryData -> a) ->
    (TryList TryData -> TryData)
liftList f = toTryData . fmap f

-- | Lift a binary list function to @TryData@
liftList2 ::
    ToTryData a =>
    (ListTry TryData -> ListTry TryData -> a) ->
    (TryList TryData -> TryList TryData -> TryData)
liftList2 f x y = toTryData $ liftM2 f x y

-- | A helper class for checking for equality
class TryEq a where
    tryEq :: a -> a -> Try Bool
    tryNe :: a -> a -> Try Bool
    tryNe x y = not <$> tryEq x y

instance Eq a => TryEq (Det a) where
    tryEq (Det x) (Det y) = Val $ x == y

instance TryEq Data where
    tryEq x y = Val $ x == y

instance TryEq a => TryEq (Try a) where
    tryEq = liftJoinM2 tryEq

instance TryEq a => TryEq (ListTry a) where
    tryEq Nil Nil = Val True
    tryEq (Cons x xs) (Cons y ys) = liftM2 (&&) (tryEq x y) (tryEq xs ys)
    tryEq _ _ = Val False

instance TryEq DataTry where
    tryEq (DIntT x) (DIntT y) = tryEq x y
    tryEq (DStringT x) (DStringT y) = tryEq x y
    tryEq (DListT x) (DListT y) = tryEq x y
    tryEq _ _ = Val False
