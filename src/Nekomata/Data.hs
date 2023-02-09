{-# LANGUAGE TypeFamilies #-}

module Nekomata.Data where

import Control.Monad (liftM2)
import Nekomata.NonDet
import Nekomata.Utils

data ListTry a = Nil | Cons a (TryList a) deriving (Show)

-- | A non-deterministic list
type TryList a = Try (ListTry a)

instance Functor ListTry where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap (fmap f) xs)

{- | Zip two @TryList@s with a function

If one of the lists is shorter, the remaining elements of the longer list
are padded to the result.
-}
zipWithPad :: (a -> a -> a) -> ListTry a -> ListTry a -> ListTry a
zipWithPad _ Nil xs = xs
zipWithPad _ xs Nil = xs
zipWithPad f (Cons x xs) (Cons y ys) =
    Cons (f x y) (liftM2 (zipWithPad f) xs ys)

{- | Zip two @TryList@s with a function

Fail if the lists have different lengths.
-}
zipWithFail :: (a -> b -> c) -> ListTry a -> ListTry b -> TryList c
zipWithFail _ Nil Nil = Val Nil
zipWithFail f (Cons x xs) (Cons y ys) =
    Val $ Cons (f x y) (liftJoinM2 (zipWithFail f) xs ys)
zipWithFail _ _ _ = Fail

instance NonDet a => NonDet (ListTry a) where
    type Value (ListTry a) = [Value a]
    fromValue [] = Nil
    fromValue (x : xs) = Cons (fromValue x) (Val $ fromValue xs)
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
    deriving (Show)

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
