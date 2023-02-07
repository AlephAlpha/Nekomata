{-# LANGUAGE TypeFamilies #-}

module Nekomata.Data where

import Nekomata.NonDet

data ListTry a = Nil | Cons a (TryList a) deriving (Show)

-- | A non-deterministic list
type TryList a = Try (ListTry a)

instance Functor ListTry where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap (fmap f) xs)

instance NonDet a => NonDet (ListTry a) where
    type Value (ListTry a) = [Value a]
    fromValue [] = Nil
    fromValue (x : xs) = Cons (fromValue x) (Val (fromValue xs))
    toTry Nil = Val []
    toTry (Cons x xs) = (:) <$> toTry x <*> toTry xs

-- | Nekomata's data type (deterministic)
data Data
    = DInt Integer
    | DChar Char
    | DString String
    | DList [Data]
    deriving (Eq)

instance Show Data where
    show (DInt x) = show x
    show (DChar x) = show x
    show (DString x) = show x
    show (DList x) = show x

data DataTry
    = DIntT (Try (Det Integer))
    | DCharT (Try (Det Char))
    | DStringT (TryList (Det Char))
    | DListT (TryList TryData)
    deriving (Show)

-- | Nekomata's data type (non-deterministic)
type TryData = Try DataTry

instance NonDet DataTry where
    type Value DataTry = Data
    fromValue (DInt x) = DIntT (fromValue x)
    fromValue (DChar x) = DCharT (fromValue x)
    fromValue (DString x) = DStringT (fromValue x)
    fromValue (DList x) = DListT (fromValue x)
    toTry (DIntT t) = DInt <$> toTry t
    toTry (DCharT t) = DChar <$> toTry t
    toTry (DStringT t) = DString <$> toTry t
    toTry (DListT t) = DList <$> toTry t
