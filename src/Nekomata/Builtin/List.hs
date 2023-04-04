module Nekomata.Builtin.List where

import Control.Arrow (first, second)
import Control.Monad (join, liftM2, (>=>))
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Nekomata.Builtin.Basic (dup)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

nonempty' :: Function
nonempty' = predicate nonempty''
  where
    nonempty'' _ (DListT x) = nonempty_ <$> x
    nonempty'' _ (DStringT x) = nonempty_ <$> x
    nonempty'' _ _ = Fail
    nonempty_ :: ListTry a -> Bool
    nonempty_ (Cons _ _) = True
    nonempty_ Nil = False

anyOf' :: Function
anyOf' = unary anyOf''
  where
    anyOf'' i (DStringT xs) = Val . DStringT $ xs >>= anyOf i <&> singleton
    anyOf'' i (DListT xs) = join (xs >>= anyOf i)
    anyOf'' _ _ = Fail

emptyList :: Function
emptyList = constant . Val . DListT $ Val Nil

singleton' :: Function
singleton' = unary . const $ toTryData . singleton

pair :: Function
pair = binary pair'
  where
    pair' _ x y = toTryData [x, y]

removeFail :: Function
removeFail = unary removeFail'
  where
    removeFail' _ (DListT xs) = Val . DListT $ xs >>= filterTry
    removeFail' _ x = Val x

length' :: Function
length' = unary length''
  where
    length'' _ (DStringT xs) = liftString length_ xs
    length'' _ (DListT xs) = liftList length_ xs
    length'' _ _ = toTryData (1 :: Integer)
    length_ :: ListTry a -> Try Integer
    length_ Nil = Val 0
    length_ (Cons _ xs) = xs >>= length_ <&> (+ 1)

lengthIs :: Function
lengthIs = binary lengthIs'
  where
    lengthIs' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . (`lengthIs_` x)) y) xs
    lengthIs' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`lengthIs_` x) y) xs
    lengthIs' _ _ _ = Fail
    lengthIs_ :: Integer -> ListTry a -> TryList a
    lengthIs_ 0 Nil = Val Nil
    lengthIs_ _ Nil = Fail
    lengthIs_ n _ | n <= 0 = Fail
    lengthIs_ n (Cons x xs) = Cons x . lengthIs_ (n - 1) <$> xs

range0 :: Function
range0 = unaryVec range0'
  where
    range0' _ (DNumT x) = liftNum range0_ x
    range0' _ _ = Fail
    range0_ :: Rational -> [Integer]
    range0_ x = enumFromTo 0 . floor $ x - 1

range1 :: Function
range1 = unaryVec range1'
  where
    range1' _ (DNumT x) = liftNum range1_ x
    range1' _ _ = Fail
    range1_ :: Rational -> [Integer]
    range1_ = enumFromTo 1 . floor

nth :: Function
nth = binaryVecArg2 nth'
  where
    nth' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (`nth_` x) y) xs
    nth' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`nth_` x) y) xs
    nth' _ _ _ = Fail
    nth_ :: Integer -> ListTry a -> Try a
    nth_ 0 (Cons x _) = Val x
    nth_ n (Cons _ xs) | n > 0 = xs >>= nth_ (n - 1)
    nth_ _ _ = Fail

head' :: Function
head' = unary head''
  where
    head'' _ (DStringT xs) = liftString head_ xs
    head'' _ (DListT xs) = liftList head_ xs
    head'' _ _ = Fail
    head_ :: ListTry a -> Try a
    head_ Nil = Fail
    head_ (Cons x _) = Val x

tail' :: Function
tail' = unary tail''
  where
    tail'' _ (DStringT xs) = liftString (AsString . tail_) xs
    tail'' _ (DListT xs) = liftList tail_ xs
    tail'' _ _ = Fail
    tail_ :: ListTry a -> TryList a
    tail_ Nil = Fail
    tail_ (Cons _ xs) = xs

cons :: Function
cons = binary cons'
  where
    cons' _ (DListT xs) y = liftList (Cons (Val y) . Val) xs
    cons' _ x y = liftList (Cons (Val y) . Val) (Val . singleton $ Val x)

uncons :: Function
uncons = unary2 uncons'
  where
    uncons' _ (DStringT xs) =
        liftString12 (uncons_ >=> \(ys, y) -> Val (AsString ys, y)) xs
    uncons' _ (DListT xs) = liftList12 uncons_ xs
    uncons' _ _ = (Fail, Fail)
    uncons_ :: ListTry a -> Try (TryList a, a)
    uncons_ Nil = Fail
    uncons_ (Cons x xs) = Val (xs, x)

last' :: Function
last' = unary last''
  where
    last'' _ (DStringT xs) = liftString last_ xs
    last'' _ (DListT xs) = liftList last_ xs
    last'' _ _ = Fail
    last_ :: ListTry a -> Try (Maybe a)
    last_ Nil = Val Nothing
    last_ (Cons x xs) = xs >>= last_ >>= Val . Just . fromMaybe x

init' :: Function
init' = unary init''
  where
    init'' _ (DStringT xs) = liftString (AsString . init_) xs
    init'' _ (DListT xs) = liftList init_ xs
    init'' _ _ = Fail
    init_ :: ListTry a -> Try (Maybe (ListTry a))
    init_ Nil = Val Nothing
    init_ (Cons x xs) = xs >>= init_ >>= Val . Just . maybe Nil (Cons x . Val)

snoc :: Function
snoc = singleton' .* join'

unsnoc :: Function
unsnoc = unary2 unsnoc'
  where
    unsnoc' _ (DStringT xs) =
        liftString12 (unsnoc'' >=> \(ys, y) -> Val (AsString ys, y)) xs
    unsnoc' _ (DListT xs) = liftList12 unsnoc'' xs
    unsnoc' _ _ = (Fail, Fail)
    unsnoc'' = fmap unzipMaybe . unsnoc_
    unsnoc_ :: ListTry a -> Try (Maybe (ListTry a, a))
    unsnoc_ Nil = Val Nothing
    unsnoc_ (Cons x xs) =
        xs
            >>= unsnoc_
            >>= Val
                . Just
                . maybe (Nil, x) (\(ys, y) -> (Cons x (Val ys), y))
    unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
    unzipMaybe Nothing = (Nothing, Nothing)
    unzipMaybe (Just (a, b)) = (Just a, Just b)

cons0 :: Function
cons0 = constant (0 :: Integer) .* cons

reverse' :: Function
reverse' = unary reverse''

reverse'' :: Id -> DataTry -> TryData
reverse'' _ (DStringT xs) = liftString (AsString . reverse_ Nil) xs
reverse'' _ (DListT xs) = liftList (reverse_ Nil) xs
reverse'' _ _ = Fail

reverse_ :: ListTry a -> ListTry a -> TryList a
reverse_ ys Nil = Val ys
reverse_ ys (Cons x xs) = xs >>= reverse_ (Cons x (Val ys))

prefix :: Function
prefix = unary prefix'
  where
    prefix' i (DStringT xs) = liftString (AsString . prefix_ i) xs
    prefix' i (DListT xs) = liftList (prefix_ i) xs
    prefix' _ _ = Fail
    prefix_ :: Id -> ListTry a -> TryList a
    prefix_ _ Nil = Val Nil
    prefix_ i (Cons x xs) =
        Choice (leftId i) (Val Nil) (Val . Cons x $ xs >>= prefix_ (rightId i))

suffix :: Function
suffix = unary suffix'
  where
    suffix' i (DStringT xs) = liftString (AsString . suffix_ i) xs
    suffix' i (DListT xs) = liftList (suffix_ i) xs
    suffix' _ _ = Fail
    suffix_ :: Id -> ListTry a -> TryList a
    suffix_ _ Nil = Val Nil
    suffix_ i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix_ (rightId i))

take' :: Function
take' = binaryVecArg2 take''
  where
    take'' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . (`take_` x)) y) xs
    take'' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`take_` x) y) xs
    take'' _ _ _ = Fail
    take_ :: Integer -> ListTry a -> TryList a
    take_ 0 _ = Val Nil
    take_ n (Cons x xs) | n > 0 = Cons x . take_ (n - 1) <$> xs
    take_ _ _ = Fail

subset :: Function
subset = unary subset'
  where
    subset' i (DStringT xs) =
        liftString (AsString . subset_ i) xs
    subset' i (DListT xs) = liftList (subset_ i) xs
    subset' _ _ = Fail
    subset_ :: Id -> ListTry a -> TryList a
    subset_ i xs = Choice (leftId i) (Val Nil) (nonemptySubset (rightId i) xs)
    nonemptySubset _ Nil = Fail
    nonemptySubset i (Cons x xs) =
        Choice (leftId i) (Val $ singleton x) $
            xs >>= nonemptySubset (leftId (rightId i)) >>= \ys ->
                Choice (rightId (rightId i)) (Val ys) (Val . Cons x $ Val ys)

subsequence :: Function
subsequence = unary subsequence'
  where
    subsequence' i (DStringT xs) = liftString (AsString . subsequence_ i) xs
    subsequence' i (DListT xs) = liftList (subsequence_ i) xs
    subsequence' _ _ = Fail
    subsequence_ :: Id -> ListTry a -> TryList a
    subsequence_ i xs =
        Choice
            (leftId i)
            (Val Nil)
            (prefix' (leftId (rightId i)) xs >>= suffix' (rightId (rightId i)))
    prefix' _ Nil = Fail
    prefix' i (Cons x xs) =
        Choice
            (leftId i)
            (Val $ singleton x)
            (Val . Cons x $ xs >>= prefix' (rightId i))
    suffix' _ Nil = Fail
    suffix' i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix' (rightId i))

join' :: Function
join' = binary join''

join'' :: Id -> DataTry -> DataTry -> TryData
join'' _ (DStringT xs) (DStringT ys) = liftString2 (AsString .: join_) xs ys
join'' i x@(DStringT _) y =
    join'' i x (DStringT (fromList . map Det . show <$> toTry y))
join'' i x y@(DStringT _) =
    join'' i (DStringT (fromList . map Det . show <$> toTry x)) y
join'' _ (DListT xs) (DListT ys) = liftList2 join_ xs ys
join'' _ _ _ = Fail

join_ :: ListTry a -> ListTry a -> TryList a
join_ Nil ys = Val ys
join_ (Cons x xs) ys = Val . Cons x $ liftJoinM2 join_ xs (Val ys)

split :: Function
split = unary2 split'
  where
    split' i (DStringT xs) =
        liftString12 (split_ i >=> \(x, y) -> Val (AsString x, AsString y)) xs
    split' i (DListT xs) = liftList12 (split_ i) xs
    split' _ _ = (Fail, Fail)
    split_ :: Id -> ListTry a -> Try (ListTry a, ListTry a)
    split_ _ Nil = Val (Nil, Nil)
    split_ i s@(Cons x xs) =
        Choice
            (leftId i)
            (Val (Nil, s))
            (xs >>= split_ (rightId i) <&> first (Cons x . Val))

minimum' :: Function
minimum' = unary minimum''
  where
    minimum'' i (DListT xs) = liftList (tryFoldl1 (const tryMin) i) xs
    minimum'' _ _ = Fail

maximum' :: Function
maximum' = unary maximum''
  where
    maximum'' i (DListT xs) = liftList (tryFoldl1 (const tryMax) i) xs
    maximum'' _ _ = Fail

concat' :: Function
concat' = unary concat''
  where
    concat'' i (DListT xs) = xs >>= concat_ i
    concat'' _ _ = Fail
    concat_ _ Nil = Val . DListT $ Val Nil
    concat_ i (Cons x xs) = liftJoinM2 (tryFoldl join'' i) x xs

unconcat :: Function
unconcat = unary unconcat'
  where
    unconcat' i (DStringT xs) =
        liftString (fmap (fmap AsString) . unconcat_ i) xs
    unconcat' i (DListT xs) = liftList (unconcat_ i) xs
    unconcat' _ _ = Fail
    unconcat_ :: Id -> ListTry a -> TryList (TryList a)
    unconcat_ _ Nil = Val Nil
    unconcat_ i (Cons x xs) =
        Choice
            (leftId i)
            (Val $ Cons (Val $ singleton x) (xs >>= unconcat_ (rightId i)))
            (xs >>= unconcat_ (rightId i) >>= prependToFirst x)
    prependToFirst _ Nil = Fail
    prependToFirst x (Cons y ys) = Val $ Cons (Val $ Cons x y) ys

nub :: Function
nub = unary nub'
  where
    nub' i (DStringT xs) = liftString (AsString . nub_ i) xs
    nub' i (DListT xs) = liftList (nub_ i) xs
    nub' _ _ = Fail
    nub_ :: (TryEq a) => Id -> ListTry a -> ListTry a
    nub_ _ Nil = Nil
    nub_ i (Cons x xs) =
        Cons x $
            xs
                >>= tryFilter (const $ tryNe x) (leftId i)
                <&> nub_ (rightId i)

sort :: Function
sort = unary sort'
  where
    sort' i (DStringT xs) = liftString (AsString . sort_ i) xs
    sort' i (DListT xs) = liftList (sort_ i) xs
    sort' _ _ = Fail
    sort_ :: (TryOrd a) => Id -> ListTry a -> TryList a
    sort_ _ xs = mergeLists (Val . singleton <$> xs)
    mergeLists :: (TryOrd a) => ListTry (TryList a) -> TryList a
    mergeLists Nil = Val Nil
    mergeLists (Cons x xs) = xs >>= mergeLists' x
    mergeLists' :: (TryOrd a) => TryList a -> ListTry (TryList a) -> TryList a
    mergeLists' x Nil = x
    mergeLists' x (Cons y ys) =
        ys >>= mergePairs >>= mergeLists' (liftJoinM2 merge x y)
    mergePairs :: (TryOrd a) => ListTry (TryList a) -> TryList (TryList a)
    mergePairs Nil = Val Nil
    mergePairs (Cons x xs) = mergePairs' x <$> xs
    mergePairs' x Nil = singleton x
    mergePairs' x (Cons y ys) = Cons (liftJoinM2 merge x y) (ys >>= mergePairs)
    merge :: (TryOrd a) => ListTry a -> ListTry a -> TryList a
    merge Nil ys = Val ys
    merge xs Nil = Val xs
    merge (Cons x xs) (Cons y ys) =
        tryLe x y <&> \b ->
            if b
                then Cons x (liftJoinM2 merge xs (Val $ Cons y ys))
                else Cons y (liftJoinM2 merge (Val $ Cons x xs) ys)

permutation :: Function
permutation = unary permutation'
  where
    permutation' i (DStringT xs) = liftString (AsString . permutation_ i) xs
    permutation' i (DListT xs) = liftList (permutation_ i) xs
    permutation' _ _ = Fail
    permutation_ :: Id -> ListTry a -> TryList a
    permutation_ _ Nil = Val Nil
    permutation_ i xs =
        extract' (leftId i) xs >>= \(x, xs') ->
            Val $ Cons x (xs' >>= permutation_ (rightId i))
    extract' :: Id -> ListTry a -> Try (a, TryList a)
    extract' _ Nil = Fail
    extract' i (Cons x xs) =
        Choice
            (leftId i)
            (Val (x, xs))
            (xs >>= extract' (rightId i) <&> second (Val . Cons x))

allEqual :: Function
allEqual = unary allEqual'
  where
    allEqual' i (DStringT xs) = liftString (tryFoldl1 tryEq' i . fmap Val) xs
    allEqual' i (DListT xs) = liftList (tryFoldl1 tryEq' i) xs
    allEqual' _ _ = Fail
    tryEq' :: (TryEq a) => Id -> a -> a -> Try a
    tryEq' _ x y = tryEq x y >>= \b -> if b then Val x else Fail

free :: Function
free = predicate2 free'
  where
    free' i x@(DListT xs) y = liftM2 (&&) (tryNe x y) (xs >>= free'' i y)
    free' _ x y = tryNe x y
    free'' :: Id -> DataTry -> ListTry TryData -> Try Bool
    free'' _ _ Nil = Val True
    free'' i y (Cons x xs) =
        liftM2
            (&&)
            (x >>= flip (free' (leftId i)) y)
            (xs >>= free'' (rightId i) y)

enumerate :: Function
enumerate = dup .* length' .* range0

rotate :: Function
rotate = binaryVecArg2 rotate'
  where
    rotate' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . (`rotate_` x)) y) xs
    rotate' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`rotate_` x) y) xs
    rotate' _ _ _ = Fail
    rotate_ :: Integer -> ListTry a -> TryList a
    rotate_ _ Nil = Val Nil
    rotate_ n s@(Cons x xs)
        | n > 0 =
            xs >>= (`join_` singleton x) >>= rotate_ (n - 1)
        | n < 0 = reverse_ Nil s >>= rotate_ (-n) >>= reverse_ Nil
        | otherwise = Val s

transpose :: Function
transpose = unary transpose'
  where
    transpose' i (DListT xs) = liftList (transpose'' . tryMap asList i) xs
    transpose' _ _ = Fail
    asList _ (DListT xs) = xs
    asList _ _ = Fail
    transpose'' :: ListTry (TryList a) -> TryList (TryList a)
    transpose'' Nil = Val Nil
    transpose'' (Cons x xs) = liftJoinM2 transpose_ x xs
    transpose_ xs Nil = Val $ Val . singleton <$> xs
    transpose_ xs ys = transpose'' ys >>= zipWithCons xs
    zipWithCons Nil Nil = Val Nil
    zipWithCons (Cons x xs) (Cons y ys) =
        Val $ Cons (Val $ Cons x y) (liftJoinM2 zipWithCons xs ys)
    zipWithCons _ _ = Fail
