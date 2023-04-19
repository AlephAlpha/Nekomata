{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Nekomata.Data where

import Control.Monad (join, liftM2)
import Data.Functor ((<&>))
import Data.Ratio (denominator, numerator)
import Nekomata.NonDet

-- | A helper function to lift a binary function to a monad
liftJoinM2 :: (Monad m) => (a -> b -> m c) -> m a -> m b -> m c
liftJoinM2 f x y = join $ liftM2 f x y

-- | A helper function to compose a unary function with a binary function
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

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

{- | Zip two @TryList@s with a function

Truncate the result to the length of the shorter list.
-}
zipWithTrunc ::
    (Id -> a -> b -> Try c) ->
    Id ->
    ListTry (Try a) ->
    ListTry (Try b) ->
    TryList (Try c)
zipWithTrunc _ _ Nil _ = Val Nil
zipWithTrunc _ _ _ Nil = Val Nil
zipWithTrunc f i (Cons x xs) (Cons y ys) =
    Val $
        Cons
            (liftJoinM2 (f (leftId i)) x y)
            (liftJoinM2 (zipWithTrunc f (rightId i)) xs ys)

-- | Choose an element from a @TryList@
anyOf :: Id -> ListTry a -> Try a
anyOf _ Nil = Fail
anyOf i (Cons x xs) = Choice (leftId i) (Val x) (xs >>= anyOf (rightId i))

-- | Choose an element from a list
anyOf' :: Id -> [a] -> Try a
anyOf' _ [] = Fail
anyOf' i (x : xs) = Choice (leftId i) (Val x) (anyOf' (rightId i) xs)

-- | A singleton list
singleton :: a -> ListTry a
singleton x = Cons x (Val Nil)

-- | Map a non-deterministic function over a @TryList@
tryMap :: (Id -> a -> Try b) -> Id -> ListTry (Try a) -> ListTry (Try b)
tryMap _ _ Nil = Nil
tryMap f i (Cons x xs) = Cons (x >>= f (leftId i)) (tryMap f (rightId i) <$> xs)

-- | Map over the first argument of a binary function
tryMap1 ::
    (Id -> a -> b -> Try c) ->
    Id ->
    b ->
    ListTry (Try a) ->
    ListTry (Try c)
tryMap1 f i y = tryMap (\i' x -> f i' x y) i

-- | Map over the second argument of a binary function
tryMap2 ::
    (Id -> a -> b -> Try c) ->
    Id ->
    a ->
    ListTry (Try b) ->
    ListTry (Try c)
tryMap2 f i x = tryMap (`f` x) i

-- | Fold a non-deterministic function over a @TryList@ from right to left
tryFoldr :: (Id -> a -> b -> Try b) -> Id -> b -> ListTry (Try a) -> Try b
tryFoldr _ _ b Nil = Val b
tryFoldr f i b (Cons x xs) =
    liftJoinM2 (f (leftId i)) x (xs >>= tryFoldr f (rightId i) b)

-- | Fold a non-deterministic function over a @TryList@ from right to left
tryFoldr1 :: (Id -> a -> a -> Try a) -> Id -> ListTry (Try a) -> Try a
tryFoldr1 _ _ Nil = Fail
tryFoldr1 f i (Cons x xs) = liftJoinM2 (tryFoldr f i) x xs

-- | Fold a non-deterministic function over a @TryList@ from left to right
tryFoldl :: (Id -> b -> a -> Try b) -> Id -> b -> ListTry (Try a) -> Try b
tryFoldl _ _ b Nil = Val b
tryFoldl f i b (Cons x xs) =
    liftJoinM2 (tryFoldl f (leftId i)) (x >>= f (rightId i) b) xs

-- | Fold a non-deterministic function over a @TryList@ from left to right
tryFoldl1 :: (Id -> a -> a -> Try a) -> Id -> ListTry (Try a) -> Try a
tryFoldl1 _ _ Nil = Fail
tryFoldl1 f i (Cons x xs) = liftJoinM2 (tryFoldl f i) x xs

-- | Scan a non-deterministic function over a @TryList@ from left to right
tryScanl :: (Id -> b -> a -> Try b) -> Id -> b -> ListTry (Try a) -> ListTry b
tryScanl _ _ b Nil = singleton b
tryScanl f i b (Cons x xs) =
    Cons b (liftM2 (tryScanl f (leftId i)) (x >>= f (rightId i) b) xs)

-- | Scan a non-deterministic function over a @TryList@ from left to right
tryScanl1 :: (Id -> a -> a -> Try a) -> Id -> ListTry (Try a) -> TryList a
tryScanl1 _ _ Nil = Val Nil
tryScanl1 f i (Cons x xs) = liftM2 (tryScanl f i) x xs

{- | Map a binary non-deterministic function over two @TryList@s and
return a @ListTry@ of @TryList@s
-}
tryOuter ::
    (Id -> a -> b -> Try c) ->
    Id ->
    ListTry (Try a) ->
    ListTry (Try b) ->
    ListTry (TryList (Try c))
tryOuter f i xs = tryMap (\i' y -> Val $ tryMap (\i'' x -> f i'' x y) i' xs) i

-- | Filter a @TryList@
tryFilter :: (Id -> a -> Try Bool) -> Id -> ListTry a -> TryList a
tryFilter _ _ Nil = Val Nil
tryFilter f i (Cons x xs) =
    f (leftId i) x >>= \b ->
        if b
            then Val $ Cons x (xs >>= tryFilter f (rightId i))
            else xs >>= tryFilter f (rightId i)

-- | Remove failed elements from a @TryList@
filterTry :: (NonDet a) => ListTry (Try a) -> TryList (Try a)
filterTry Nil = Val Nil
filterTry (Cons x xs) = Cut $ \ds ->
    ( ds
    , let xs' = xs >>= filterTry
       in if hasValue ds x then x <&> \x' -> Cons (Val x') xs' else xs'
    )

instance (NonDet a) => NonDet (ListTry a) where
    type Value (ListTry a) = [Value a]
    fromValue = fromList . map fromValue
    toTry Nil = Val []
    toTry (Cons x xs) = (:) <$> toTry x <*> toTry xs

-- | Nekomata's data type (deterministic)
data Data
    = DNum Rational
    | DString String
    | DList [Data]
    deriving (Eq, Ord)

instance Show Data where
    show (DNum x) =
        if denominator x == 1
            then show (numerator x)
            else show (numerator x) ++ "/" ++ show (denominator x)
    show (DString x) = "\"" ++ x ++ "\""
    show (DList x) = show x

data DataTry
    = DNumT (Try (Det Rational))
    | DStringT (TryList (Det Char))
    | DListT (TryList TryData)

-- | Nekomata's data type (non-deterministic)
type TryData = Try DataTry

instance NonDet DataTry where
    type Value DataTry = Data
    fromValue (DNum x) = DNumT $ fromValue x
    fromValue (DString x) = DStringT $ fromValue x
    fromValue (DList x) = DListT $ fromValue x
    toTry (DNumT t) = DNum <$> toTry t
    toTry (DStringT t) = DString <$> toTry t
    toTry (DListT t) = DList <$> toTry t

{- | Convert any @DataTry@ to a @TryList TryData@

For integers, it generates a list of integers from 0 to input minus 1.
For strings, it generates a list of strings with one character.
-}
toTryList :: DataTry -> TryList TryData
toTryList (DListT xs) = xs
toTryList (DNumT x) =
    fromList . map toTryData . range0_ <$> toTry x
  where
    range0_ :: Rational -> [Integer]
    range0_ x' = enumFromTo 0 . floor $ x' - 1
toTryList (DStringT xs) = fmap (Val . DStringT . Val . singleton) <$> xs

-- | Convert a @Rational@ to a @Try Integer@
toTryInt :: Rational -> Try Integer
toTryInt x
    | denominator x == 1 = Val $ numerator x
    | otherwise = Fail

-- | Convert a @Try (Det Rational)@ to a @Try Integer@
toTryInt' :: Try (Det Rational) -> Try Integer
toTryInt' x = x >>= toTryInt . unDet

-- | A helper class for lifting functions to @TryData@
class ToTryData a where
    toTryData :: a -> TryData

-- | A wrapper to avoid overlapping instances
newtype AsString a = AsString {fromAsString :: a}

instance (ToTryData a) => ToTryData (Det a) where
    toTryData = toTryData . unDet

instance (ToTryData a) => ToTryData (Try a) where
    toTryData = (>>= toTryData)

instance (ToTryData a) => ToTryData (Maybe a) where
    toTryData = maybe Fail toTryData

instance ToTryData Integer where
    toTryData = Val . DNumT . Val . Det . fromInteger

instance ToTryData Rational where
    toTryData = Val . DNumT . Val . Det

instance ToTryData Char where
    toTryData = Val . DStringT . Val . singleton . Det

instance ToTryData (AsString String) where
    toTryData = Val . DStringT . fromValue . fromAsString

instance ToTryData (AsString (ListTry Char)) where
    toTryData = Val . DStringT . Val . fmap Det . fromAsString

instance (ToTryData (AsString a)) => ToTryData (AsString (Try a)) where
    toTryData = toTryData . fmap AsString . fromAsString

instance (ToTryData (AsString a)) => ToTryData (AsString (Maybe a)) where
    toTryData = toTryData . fmap AsString . fromAsString

instance (ToTryData a) => ToTryData [a] where
    toTryData = Val . DListT . Val . fromList . map toTryData

instance (ToTryData a) => ToTryData (ListTry a) where
    toTryData = Val . DListT . Val . fmap toTryData

instance ToTryData Data where
    toTryData = Val . fromValue

instance ToTryData DataTry where
    toTryData = Val

{- | Convert a @TryData@ to the normal form

I haven't defined what the normal form really is.
This function basically lifts all the non-determinism in @ListTry@
to the top level.
-}
normalForm :: TryData -> TryData
normalForm = toTryData . toTry

-- | Lift a unary numeric function to @TryData@
liftNum :: (ToTryData a) => (Rational -> a) -> (Try (Det Rational) -> TryData)
liftNum f = toTryData . fmap f . toTry

-- | Lift a binary numeric function to @TryData@
liftNum2 ::
    (ToTryData a) =>
    (Rational -> Rational -> a) ->
    (Try (Det Rational) -> Try (Det Rational) -> TryData)
liftNum2 f x y = toTryData $ liftM2 f (toTry x) (toTry y)

-- | Lift a unary numeric function that returns two values to @TryData@
liftNum12 ::
    (ToTryData a, ToTryData b) =>
    (Rational -> Try (a, b)) ->
    (Try (Det Rational) -> (TryData, TryData))
liftNum12 f x =
    let y = x >>= f . unDet
     in (toTryData $ fst <$> y, toTryData $ snd <$> y)

-- | Lift a unary integer function to @TryData@
liftInt :: (ToTryData a) => (Integer -> a) -> (Try (Det Rational) -> TryData)
liftInt f = toTryData . fmap f . toTryInt'

-- | Lift a binary integer function to @TryData@
liftInt2 ::
    (ToTryData a) =>
    (Integer -> Integer -> a) ->
    (Try (Det Rational) -> Try (Det Rational) -> TryData)
liftInt2 f x y = toTryData $ liftM2 f (toTryInt' x) (toTryInt' y)

-- | Lift a unary string function to @TryData@
liftString ::
    (ToTryData a) =>
    (ListTry Char -> a) ->
    (TryList (Det Char) -> TryData)
liftString f = toTryData . fmap (f . fmap unDet)

-- | Lift a binary string function to @TryData@
liftString2 ::
    (ToTryData a) =>
    (ListTry Char -> ListTry Char -> a) ->
    (TryList (Det Char) -> TryList (Det Char) -> TryData)
liftString2 f x y =
    toTryData $
        liftM2 f (fmap unDet <$> x) (fmap unDet <$> y)

-- | Lift a unary string function that returns two values to @TryData@
liftString12 ::
    (ToTryData a, ToTryData b) =>
    (ListTry Char -> Try (a, b)) ->
    (TryList (Det Char) -> (TryData, TryData))
liftString12 f x =
    let y = x >>= f . fmap unDet
     in (toTryData $ fst <$> y, toTryData $ snd <$> y)

-- | Lift a unary list function to @TryData@
liftList ::
    (ToTryData a) =>
    (ListTry TryData -> a) ->
    (TryList TryData -> TryData)
liftList f = toTryData . fmap f

-- | Lift a binary list function to @TryData@
liftList2 ::
    (ToTryData a) =>
    (ListTry TryData -> ListTry TryData -> a) ->
    (TryList TryData -> TryList TryData -> TryData)
liftList2 f x y = toTryData $ liftM2 f x y

-- | Lift a unary list function that returns two values to @TryData@
liftList12 ::
    (ToTryData a, ToTryData b) =>
    (ListTry TryData -> Try (a, b)) ->
    (TryList TryData -> (TryData, TryData))
liftList12 f x =
    let y = x >>= f in (toTryData $ fst <$> y, toTryData $ snd <$> y)

-- | Vectorize a unary function
vec1 :: (Id -> DataTry -> TryData) -> Id -> DataTry -> TryData
vec1 f i (DListT xs) = liftList (tryMap (vec1 f) i) xs
vec1 f i x = f i x

-- | Vectorize a binary function with padding
vec2Pad ::
    (Id -> DataTry -> DataTry -> TryData) ->
    Id ->
    DataTry ->
    DataTry ->
    TryData
vec2Pad f i (DListT xs) (DListT ys) = liftList2 (zipWithPad (vec2Pad f) i) xs ys
vec2Pad f i (DListT xs) y = liftList (tryMap1 (vec2Pad f) i y) xs
vec2Pad f i x (DListT ys) = liftList (tryMap2 (vec2Pad f) i x) ys
vec2Pad f i x y = f i x y

-- | Vectorize a binary function with failure on mismatched lengths
vec2Fail ::
    (Id -> DataTry -> DataTry -> TryData) ->
    Id ->
    DataTry ->
    DataTry ->
    TryData
vec2Fail f i (DListT xs) (DListT ys) =
    liftList2 (zipWithFail (vec2Fail f) i) xs ys
vec2Fail f i (DListT xs) y = liftList (tryMap1 (vec2Fail f) i y) xs
vec2Fail f i x (DListT ys) = liftList (tryMap2 (vec2Fail f) i x) ys
vec2Fail f i x y = f i x y

-- | Vectorize a binary function with outer product
vec2Outer ::
    (Id -> DataTry -> DataTry -> TryData) ->
    Id ->
    DataTry ->
    DataTry ->
    TryData
vec2Outer f i (DListT xs) (DListT ys) =
    liftList2 (tryOuter (vec2Outer f) i) xs ys
vec2Outer f i (DListT xs) y = liftList (tryMap1 (vec2Outer f) i y) xs
vec2Outer f i x (DListT ys) = liftList (tryMap2 (vec2Outer f) i x) ys
vec2Outer f i x y = f i x y

-- | Vectorize the first argument of a binary function
vec2Arg1 ::
    (Id -> DataTry -> DataTry -> TryData) ->
    Id ->
    DataTry ->
    DataTry ->
    TryData
vec2Arg1 f i (DListT xs) y = liftList (tryMap1 (vec2Arg1 f) i y) xs
vec2Arg1 f i x y = f i x y

-- | Vectorize the second argument of a binary function
vec2Arg2 ::
    (Id -> DataTry -> DataTry -> TryData) ->
    Id ->
    DataTry ->
    DataTry ->
    TryData
vec2Arg2 f i x (DListT ys) = liftList (tryMap2 (vec2Arg2 f) i x) ys
vec2Arg2 f i x y = f i x y

-- | A helper class for checking for equality
class TryEq a where
    tryEq :: a -> a -> Try Bool
    tryNe :: a -> a -> Try Bool
    tryNe x y = not <$> tryEq x y

instance TryEq Integer where
    tryEq x y = Val $ x == y

instance TryEq Rational where
    tryEq x y = Val $ x == y

instance TryEq Char where
    tryEq x y = Val $ x == y

instance (Eq a) => TryEq (Det a) where
    tryEq (Det x) (Det y) = Val $ x == y

instance TryEq Data where
    tryEq x y = Val $ x == y

instance (TryEq a) => TryEq (Try a) where
    tryEq = liftJoinM2 tryEq

instance (TryEq a) => TryEq (ListTry a) where
    tryEq Nil Nil = Val True
    tryEq (Cons x xs) (Cons y ys) =
        tryEq x y >>= \b -> if b then tryEq xs ys else Val False
    tryEq _ _ = Val False

instance TryEq DataTry where
    tryEq (DNumT x) (DNumT y) = tryEq x y
    tryEq (DStringT x) (DStringT y) = tryEq x y
    tryEq (DListT x) (DListT y) = tryEq x y
    tryEq _ _ = Val False

-- | A helper class for checking for ordering
class TryOrd a where
    tryCmp :: a -> a -> Try Ordering
    tryLe :: a -> a -> Try Bool
    tryLe x y = (<= EQ) <$> tryCmp x y
    tryLt :: a -> a -> Try Bool
    tryLt x y = (< EQ) <$> tryCmp x y
    tryGe :: a -> a -> Try Bool
    tryGe x y = (>= EQ) <$> tryCmp x y
    tryGt :: a -> a -> Try Bool
    tryGt x y = (> EQ) <$> tryCmp x y
    tryMin :: a -> a -> Try a
    tryMin x y = tryCmp x y <&> \o -> if o == LT then x else y
    tryMax :: a -> a -> Try a
    tryMax x y = tryCmp x y <&> \o -> if o == GT then x else y

instance TryOrd Integer where
    tryCmp x y = Val $ compare x y

instance TryOrd Rational where
    tryCmp x y = Val $ compare x y

instance TryOrd Char where
    tryCmp x y = Val $ compare x y

instance (Ord a) => TryOrd (Det a) where
    tryCmp (Det x) (Det y) = Val $ compare x y

instance (TryOrd a) => TryOrd (Try a) where
    tryCmp = liftJoinM2 tryCmp

instance (TryOrd a) => TryOrd (ListTry a) where
    tryCmp Nil Nil = Val EQ
    tryCmp Nil _ = Val LT
    tryCmp _ Nil = Val GT
    tryCmp (Cons x xs) (Cons y ys) =
        tryCmp x y >>= \o -> if o == EQ then tryCmp xs ys else return o

instance TryOrd DataTry where
    tryCmp (DNumT x) (DNumT y) = tryCmp x y
    tryCmp (DStringT x) (DStringT y) = tryCmp x y
    tryCmp (DListT x) (DListT y) = tryCmp x y
    tryCmp (DNumT _) _ = Val LT
    tryCmp _ (DNumT _) = Val GT
    tryCmp (DStringT _) _ = Val LT
    tryCmp _ (DStringT _) = Val GT
