{-# LANGUAGE LambdaCase #-}

module Nekomata.Builtin.List where

import Control.Arrow (first)
import Control.Monad (liftM2)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Nekomata.Builtin.Basic (allValues, dup, eq)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

range0_ :: Rational -> [Integer]
range0_ x = enumFromTo 0 $ ceiling x - 1

isNonempty :: Function
isNonempty = predicate isNonempty'
  where
    isNonempty' _ (DListT x) = isNonempty_ <$> x
    isNonempty' _ _ = Fail

isNonempty_ :: ListTry a -> Bool
isNonempty_ (Cons _ _) = True
isNonempty_ Nil = False

isLong :: Function
isLong = predicate isLong'
  where
    isLong' _ (DListT x) = x >>= isLong_
    isLong' _ _ = Fail
    isLong_ :: ListTry a -> Try Bool
    isLong_ (Cons _ xs) = isNonempty_ <$> xs
    isLong_ Nil = Val False

anyOf' :: Function
anyOf' = unary anyOf''
  where
    anyOf'' i (DNumT x) = liftNum (anyOf i . fromList . range0_) x
    anyOf'' i (DListT xs) = liftList (anyOf i) xs
    anyOf'' _ _ = Fail

emptyList :: Function
emptyList = constant . Val . DListT $ Val Nil

singleton' :: Function
singleton' = unary . const $ Val . singleton_

singleton_ :: DataTry -> DataTry
singleton_ = DListT . Val . singleton . Val

orSingleton :: DataTry -> TryList TryData
orSingleton (DListT xs) = xs
orSingleton x = Val . singleton $ Val x

unsingleton :: Function
unsingleton = unary unsingleton'
  where
    unsingleton' _ (DListT xs) = liftList unsingleton_ xs
    unsingleton' _ _ = Fail
    unsingleton_ :: ListTry a -> Try a
    unsingleton_ (Cons x xs) =
        xs >>= \case
            Nil -> Val x
            _ -> Fail
    unsingleton_ _ = Fail

pair :: Function
pair = binary pair'
  where
    pair' _ x y = toTryData [x, y]

unpair :: Function
unpair = unary2 unpair'
  where
    unpair' _ (DListT xs) = liftList12 unpair_ xs
    unpair' _ _ = (Fail, Fail)
    unpair_ :: ListTry a -> Try (a, a)
    unpair_ (Cons x xs) =
        xs >>= \case
            Nil -> Fail
            Cons y ys ->
                ys >>= \case
                    Nil -> Val (x, y)
                    _ -> Fail
    unpair_ _ = Fail

removeFail :: Function
removeFail = unary removeFail'
  where
    removeFail' _ (DListT xs) = Val . DListT $ xs >>= filterTry
    removeFail' _ x = Val x

length' :: Function
length' = unary length''
  where
    length'' _ (DListT xs) = liftList length_ xs
    length'' _ _ = Fail

length_ :: ListTry a -> Try Integer
length_ Nil = Val 0
length_ (Cons _ xs) = xs >>= length_ <&> (+ 1)

lengthIs :: Function
lengthIs = binary lengthIs'
  where
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

range1 :: Function
range1 = unaryVec range1'
  where
    range1' _ (DNumT x) = liftNum range1_ x
    range1' _ _ = Fail
    range1_ :: Rational -> [Integer]
    range1_ = enumFromTo 1 . floor

interval :: Function
interval = binaryVecFail interval'
  where
    interval' _ (DNumT x) (DNumT y) = liftNum2 interval_ x y
    interval' _ (DCharT x) (DCharT y) = liftChar2 enumFromTo x y
    interval' _ _ _ = Fail
    interval_ :: Rational -> Rational -> [Integer]
    interval_ x y = enumFromTo (ceiling x) (floor y)

nth :: Function
nth = binaryVecArg2 nth'
  where
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
    head'' _ (DListT xs) = liftList head_ xs
    head'' _ _ = Fail
    head_ :: ListTry a -> Try a
    head_ Nil = Fail
    head_ (Cons x _) = Val x

tail' :: Function
tail' = unary tail''
  where
    tail'' _ (DListT xs) = liftList tail_ xs
    tail'' _ _ = Fail
    tail_ :: ListTry a -> TryList a
    tail_ Nil = Fail
    tail_ (Cons _ xs) = xs

cons :: Function
cons = Function (Arity 2 1) $ \_ (x :+ y :+ s) -> cons' y x :+ s
  where
    cons' x y = toTryData $ Cons y (x >>= orSingleton)

uncons :: Function
uncons = unary2 uncons'
  where
    uncons' _ (DListT xs) = liftList12 uncons_ xs
    uncons' _ _ = (Fail, Fail)
    uncons_ :: ListTry a -> Try (TryList a, a)
    uncons_ Nil = Fail
    uncons_ (Cons x xs) = Val (xs, x)

last' :: Function
last' = unary last''
  where
    last'' _ (DListT xs) = liftList last_ xs
    last'' _ _ = Fail
    last_ :: ListTry a -> Try (Maybe a)
    last_ Nil = Val Nothing
    last_ (Cons x xs) = xs >>= last_ >>= Val . Just . fromMaybe x

init' :: Function
init' = unary init''
  where
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
                . maybe (Nil, x) (first (Cons x . Val))
    unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
    unzipMaybe Nothing = (Nothing, Nothing)
    unzipMaybe (Just (a, b)) = (Just a, Just b)

cons0 :: Function
cons0 = constant (0 :: Integer) .* cons

reverse' :: Function
reverse' = unary $ const reverse''

reverse'' :: DataTry -> TryData
reverse'' (DNumT x) = liftNum (reverse_ Nil . fromList . range0_) x
reverse'' (DListT xs) = liftList (reverse_ Nil) xs
reverse'' _ = Fail

reverse_ :: ListTry a -> ListTry a -> TryList a
reverse_ ys Nil = Val ys
reverse_ ys (Cons x xs) = xs >>= reverse_ (Cons x (Val ys))

prefix :: Function
prefix = unary prefix'
  where
    prefix' i (DNumT x) = liftNum (prefix_ i . fromList . range0_) x
    prefix' i (DListT xs) = liftList (prefix_ i) xs
    prefix' _ _ = Fail
    prefix_ :: Id -> ListTry a -> TryList a
    prefix_ _ Nil = Val Nil
    prefix_ i (Cons x xs) =
        Choice (leftId i) (Val Nil) (Val . Cons x $ xs >>= prefix_ (rightId i))

suffix :: Function
suffix = unary suffix'
  where
    suffix' i (DNumT x) = liftNum (suffix_ i . fromList . range0_) x
    suffix' i (DListT xs) = liftList (suffix_ i) xs
    suffix' _ _ = Fail
    suffix_ :: Id -> ListTry a -> TryList a
    suffix_ _ Nil = Val Nil
    suffix_ i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix_ (rightId i))

take' :: Function
take' = binaryVecArg2 take''
  where
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
    subset' i (DNumT x) = liftNum (subset_ i . fromList . range0_) x
    subset' i (DListT xs) = liftList (subset_ i) xs
    subset' _ _ = Fail
    subset_ :: Id -> ListTry a -> TryList a
    subset_ i xs = Choice (leftId i) (Val Nil) (nonemptySubset (rightId i) xs)
    nonemptySubset _ Nil = Fail
    nonemptySubset i (Cons x xs) =
        Choice (leftId i) (Val $ singleton x) $
            xs
                >>= nonemptySubset (leftId (rightId i))
                >>= \ys ->
                    Choice (rightId (rightId i)) (Val ys) (Val . Cons x $ Val ys)

subsequence :: Function
subsequence = unary subsequence'
  where
    subsequence' i (DNumT x) = liftNum (subsequence_ i . fromList . range0_) x
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
join' = Function (Arity 2 1) $ \_ (x :+ y :+ s) -> join'' y x :+ s

join'' :: TryData -> TryData -> TryData
join'' x y = toTryData $ x >>= orSingleton >>= join_ (y >>= orSingleton)

join_ :: TryList a -> ListTry a -> TryList a
join_ ys Nil = ys
join_ ys (Cons x xs) = Val . Cons x $ xs >>= join_ ys

split :: Function
split = unary2 split'
  where
    split' i (DNumT x) = liftNum12 (split_ i . fromList . range0_) x
    split' i (DListT xs) = liftList12 (split_ i) xs
    split' _ _ = (Fail, Fail)
    split_ :: Id -> ListTry a -> Try (ListTry a, ListTry a)
    split_ _ Nil = Val (Nil, Nil)
    split_ i s@(Cons x xs) =
        Choice
            (leftId i)
            (Val (Nil, s))
            (xs >>= split_ (rightId i) <&> first (Cons x . Val))

replicate' :: Function
replicate' = binaryVecArg2 replicate''

replicate'' :: Id -> DataTry -> DataTry -> TryData
replicate'' _ x (DNumT y) = liftInt (replicate_ x) y
replicate'' _ _ _ = Fail

replicate_ :: a -> Integer -> TryList a
replicate_ _ 0 = Val Nil
replicate_ x n | n > 0 = Val $ Cons x (replicate_ x (n - 1))
replicate_ _ _ = Fail

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

minMax :: Function
minMax = unary2 minMax'
  where
    minMax' i (DListT xs) = liftList12 (minMax_ i) xs
    minMax' _ _ = (Fail, Fail)
    minMax_ i xs =
        Val (tryFoldl1 (const tryMin) i xs, tryFoldl1 (const tryMax) i xs)

concat' :: Function
concat' = unary concat''
  where
    concat'' _ (DListT xs) = liftList concat_ xs
    concat'' _ _ = Fail

concat_ :: ListTry TryData -> TryList TryData
concat_ Nil = Val Nil
concat_ (Cons x xs) = x >>= orSingleton >>= join_ (xs >>= concat_)

unconcat :: Function
unconcat = unary unconcat'
  where
    unconcat' i (DNumT x) = liftNum (unconcat_ i . fromList . range0_) x
    unconcat' i (DListT xs) = liftList (unconcat_ i) xs
    unconcat' _ _ = Fail
    unconcat_ :: Id -> ListTry a -> TryList (TryList a)
    unconcat_ _ Nil = Val Nil
    unconcat_ i (Cons x xs) =
        let xs' = xs >>= unconcat_ (rightId i)
         in Choice
                (leftId i)
                (Val $ Cons (Val $ singleton x) xs')
                ( xs' >>= \case
                    Nil -> Fail
                    Cons y ys -> Val $ Cons (Val $ Cons x y) ys
                )

nub :: Function
nub = unary nub'
  where
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
    sort' _ (DListT xs) = liftList sort_ xs
    sort' _ _ = Fail

sort_ :: (TryOrd a) => ListTry a -> TryList a
sort_ s = mergeLists (Val . singleton <$> s)
  where
    mergeLists :: (TryOrd a) => ListTry (TryList a) -> TryList a
    mergeLists Nil = Val Nil
    mergeLists (Cons x xs) = xs >>= mergeLists' x
    mergeLists' :: (TryOrd a) => TryList a -> ListTry (TryList a) -> TryList a
    mergeLists' x Nil = x
    mergeLists' x (Cons y ys) =
        ys >>= mergePairs >>= mergeLists' (liftJoinM2 merge x y)
    mergePairs :: (TryOrd a) => ListTry (TryList a) -> TryList (TryList a)
    mergePairs Nil = Val Nil
    mergePairs (Cons x xs) =
        xs <&> \case
            Nil -> singleton x
            Cons y ys -> Cons (liftJoinM2 merge x y) (ys >>= mergePairs)
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
    permutation' i (DNumT x) = liftNum (permutation_ i . fromList . range0_) x
    permutation' i (DListT xs) = liftList (permutation_ i) xs
    permutation' _ _ = Fail
    permutation_ :: Id -> ListTry a -> TryList a
    permutation_ _ Nil = Val Nil
    permutation_ i xs =
        extract_ (leftId i) xs <&> \(xs', x) ->
            Cons x (xs' >>= permutation_ (rightId i))

extract :: Function
extract = unary2 extract'
  where
    extract' i (DNumT x) = liftNum12 (extract_ i . fromList . range0_) x
    extract' i (DListT xs) = liftList12 (extract_ i) xs
    extract' _ _ = (Fail, Fail)

extract_ :: Id -> ListTry a -> Try (TryList a, a)
extract_ _ Nil = Fail
extract_ i (Cons x xs) =
    Choice
        (leftId i)
        (Val (xs, x))
        (xs >>= extract_ (rightId i) <&> first (Val . Cons x))

allEqual :: Function
allEqual = unary allEqual'
  where
    allEqual' i (DListT xs) = liftList (tryFoldl1 tryEq' i) xs
    allEqual' _ _ = Fail
    tryEq' :: (TryEq a) => Id -> a -> a -> Try a
    tryEq' _ x y = tryEq x y >>= \b -> if b then Val x else Fail

isUnique :: Function
isUnique = dup .* nub .* eq

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
    rotate' _ (DNumT x) (DNumT y) =
        liftNum (\x' -> liftInt (flip rotate_ . fromList $ range0_ x') y) x
    rotate' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`rotate_` x) y) xs
    rotate' _ _ _ = Fail
    rotate_ :: Integer -> ListTry a -> TryList a
    rotate_ _ Nil = Val Nil
    rotate_ n s@(Cons x xs)
        | n > 0 =
            xs >>= join_ (Val $ singleton x) >>= rotate_ (n - 1)
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

setPartition :: Function
setPartition = unary setPartition'
  where
    setPartition' i (DNumT x) =
        liftNum (setPartition_ i . fromList . range0_) x
    setPartition' i (DListT xs) = liftList (setPartition_ i) xs
    setPartition' _ _ = Fail
    setPartition_ :: Id -> ListTry a -> TryList (TryList a)
    setPartition_ _ Nil = Val Nil
    setPartition_ i (Cons x xs) =
        xs >>= setPartition_ (leftId i) >>= insert (rightId i) x
    insert :: Id -> a -> ListTry (TryList a) -> TryList (TryList a)
    insert _ x Nil = Val . singleton . Val $ singleton x
    insert i x (Cons y ys) =
        Choice
            (leftId i)
            (Val $ Cons (Val $ Cons x y) ys)
            (Val . Cons y $ ys >>= insert (rightId i) x)

setMinus :: Function
setMinus = binary setMinus'
  where
    setMinus' _ x y = liftList2 setMinus_ (orSingleton x) (orSingleton y)

setMinus_ :: (TryEq a) => ListTry a -> ListTry a -> TryList a
setMinus_ xs Nil = Val xs
setMinus_ xs (Cons y ys) = liftJoinM2 setMinus_ (delete y xs) ys

delete :: (TryEq a) => a -> ListTry a -> TryList a
delete _ Nil = Val Nil
delete x (Cons y ys) =
    tryEq x y
        >>= \b -> if b then ys else Val $ Cons y (ys >>= delete x)

index :: Function
index = binary index'
  where
    index' i (DListT xs) y = liftList (index_ i 0 (Val y)) xs
    index' _ _ _ = Fail
    index_ :: (TryEq a) => Id -> Integer -> a -> ListTry a -> Try Integer
    index_ _ _ _ Nil = Fail
    index_ i n y (Cons x xs) =
        let ns = xs >>= index_ (rightId i) (n + 1) y
         in tryEq x y >>= \b -> if b then Choice (leftId i) (Val n) ns else ns

count :: Function
count = binary count'
  where
    count' _ (DListT xs) y = liftList (count_ y) xs
    count' _ _ _ = Fail
    count_ :: (TryEq a) => a -> ListTry (Try a) -> Try Integer
    count_ _ Nil = Val 0
    count_ y (Cons x xs) =
        let n = xs >>= count_ y
         in x >>= tryEq y >>= \b -> if b then (+ 1) <$> n else n

tally :: Function
tally = unary2 tally'
  where
    tally' i (DListT xs) = liftList12 (tally_ i) xs
    tally' _ _ = (Fail, Fail)
    tally_ ::
        (TryEq a) => Id -> ListTry (Try a) -> Try (ListTry a, ListTry Integer)
    tally_ i xs = unzipF <$> tryFoldl insertCount i Nil xs
    insertCount _ Nil x = Val $ singleton (x, 1)
    insertCount i (Cons (y, n) ys) x =
        tryEq x y
            <&> \b ->
                if b
                    then Cons (y, n + 1) ys
                    else Cons (y, n) $ ys >>= flip (insertCount i) x

tryElem :: (TryEq a) => a -> ListTry a -> Try Bool
tryElem _ Nil = Val False
tryElem x (Cons y ys) =
    tryEq x y >>= \b -> if b then Val True else ys >>= tryElem x

intersect :: Function
intersect = binary intersect'
  where
    intersect' _ x y = liftList2 intersect_ (orSingleton x) (orSingleton y)
    intersect_ :: (TryEq a) => ListTry a -> ListTry a -> TryList a
    intersect_ _ Nil = Val Nil
    intersect_ xs (Cons y ys) =
        let ys' = liftJoinM2 intersect_ (delete y xs) ys
         in tryElem y xs >>= \b -> if b then Val $ Cons y ys' else ys'

union :: Function
union = binary union'
  where
    union' _ x y = liftList2 union_ (orSingleton x) (orSingleton y)
    union_ :: (TryEq a) => ListTry a -> ListTry a -> TryList a
    union_ xs ys = join_ (setMinus_ ys xs) xs

spanEq :: (TryEq a) => a -> ListTry a -> Try (ListTry a, ListTry a)
spanEq _ Nil = Val (Nil, Nil)
spanEq x s@(Cons y ys) =
    tryEq x y
        >>= \b ->
            if b
                then ys >>= spanEq x <&> first (Cons x . Val)
                else Val (Nil, s)

chunks :: Function
chunks = unary chunks'
  where
    chunks' _ (DListT xs) = liftList chunks_ xs
    chunks' _ _ = Fail
    chunks_ :: (TryEq a) => ListTry a -> TryList (TryList a)
    chunks_ Nil = Val Nil
    chunks_ (Cons x xs) =
        xs
            >>= spanEq x
            <&> \(ys, zs) -> Cons (Val $ Cons x (Val ys)) (chunks_ zs)

rle :: Function
rle = unary2 rle'
  where
    rle' _ (DListT xs) = liftList12 rle'' xs
    rle' _ _ = (Fail, Fail)
    rle'' ::
        (TryEq a) =>
        ListTry (Try a) ->
        Try (ListTry (Try a), ListTry (Try Integer))
    rle'' xs = unzipF <$> rle_ xs
    rle_ :: (TryEq a) => ListTry (Try a) -> TryList (Try a, Try Integer)
    rle_ Nil = Val Nil
    rle_ (Cons x xs) =
        xs
            >>= spanEq x
            <&> \(ys, zs) ->
                Cons (x, (+ 1) <$> length_ ys) (rle_ zs)

unrle :: Function
unrle = binary unrle'
  where
    unrle' i (DListT xs) (DListT ys) = liftList2 (unrle_ i) xs ys
    unrle' _ _ _ = Fail
    unrle_ :: Id -> ListTry TryData -> ListTry TryData -> TryList TryData
    unrle_ i xs ys =
        zipWithFail replicate'' (leftId i) xs ys >>= concat_

slices :: Function
slices = binaryVecArg2 slices'
  where
    slices' _ (DNumT x) (DNumT y) =
        liftNum (\x' -> liftInt (flip slices_ . fromList $ range0_ x') y) x
    slices' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (`slices_` x) y) xs
    slices' _ _ _ = Fail
    slices_ :: Integer -> ListTry a -> TryList (TryList a)
    slices_ n _ | n <= 0 = Fail
    slices_ n xs =
        splitAt' n xs <&> \case
            (Nil, _) -> Nil
            (ys, Nil) -> singleton $ Val ys
            (ys, zs) -> Cons (Val ys) (slices_ n zs)
    splitAt' :: Integer -> ListTry a -> Try (ListTry a, ListTry a)
    splitAt' 0 xs = Val (Nil, xs)
    splitAt' n (Cons x xs) = xs >>= splitAt' (n - 1) <&> first (Cons x . Val)
    splitAt' _ Nil = Val (Nil, Nil)

uninterleave :: Function
uninterleave = unary2 uninterleave'
  where
    uninterleave' _ (DNumT x) =
        liftNum12 (Val . uninterleave_ . fromList . range0_) x
    uninterleave' _ (DListT xs) = liftList12 (Val . uninterleave_) xs
    uninterleave' _ _ = (Fail, Fail)
    uninterleave_ :: ListTry a -> (TryList a, TryList a)
    uninterleave_ Nil = (Val Nil, Val Nil)
    uninterleave_ (Cons x xs) =
        let xs' = uninterleave_ <$> xs
         in (Val . Cons x $ xs' >>= snd, xs' >>= fst)

interleave :: Function
interleave = binary interleave'
  where
    interleave' _ (DListT xs) (DListT ys) = liftList2 interleave_ xs ys
    interleave' _ _ _ = Fail
    interleave_ :: ListTry a -> ListTry a -> TryList a
    interleave_ Nil Nil = Val Nil
    interleave_ Nil _ = Fail
    interleave_ (Cons x xs) ys = Val $ Cons x (xs >>= interleave_ ys)

minimumBy :: Function
minimumBy = binary minimumBy'
  where
    minimumBy' i (DListT xs) (DListT ys) = liftList2 (minimumBy_ i) xs ys
    minimumBy' _ _ _ = Fail
    minimumBy_ ::
        (TryOrd b) => Id -> ListTry (Try a) -> ListTry (Try b) -> Try a
    minimumBy_ i xs ys =
        zipWithFail (\_ x y -> Val (OrdBy y $ Val x)) (leftId i) xs ys
            >>= tryFoldl1 tryMinBy (rightId i)
            >>= ordVal

maximumBy :: Function
maximumBy = binary maximumBy'
  where
    maximumBy' i (DListT xs) (DListT ys) = liftList2 (maximumBy_ i) xs ys
    maximumBy' _ _ _ = Fail
    maximumBy_ ::
        (TryOrd b) => Id -> ListTry (Try a) -> ListTry (Try b) -> Try a
    maximumBy_ i xs ys =
        zipWithFail (\_ x y -> Val (OrdBy y $ Val x)) (leftId i) xs ys
            >>= tryFoldl1 tryMaxBy (rightId i)
            >>= ordVal

shortest :: Function
shortest = unary shortest'
  where
    shortest' i (DListT xs) = liftList (shortest_ i) xs
    shortest' _ _ = Fail
    shortest_ :: Id -> ListTry TryData -> TryData
    shortest_ i xs =
        tryFoldl1 tryMinBy i (xs <&> (\x -> Val $ OrdBy (x >>= length'') x))
            >>= ordVal
    length'' (DListT xs) = xs >>= length_
    length'' _ = Fail

longest :: Function
longest = unary longest'
  where
    longest' i (DListT xs) = liftList (longest_ i) xs
    longest' _ _ = Fail
    longest_ :: Id -> ListTry TryData -> TryData
    longest_ i xs =
        tryFoldl1 tryMaxBy i (xs <&> (\x -> Val $ OrdBy (x >>= length'') x))
            >>= ordVal
    length'' (DListT xs) = xs >>= length_
    length'' _ = Fail

tuple :: Function
tuple = binaryVecArg2 tuple'
  where
    tuple' i (DNumT x) (DNumT y) =
        liftNum (\x' -> liftInt (flip (tuple_ i) . fromList $ range0_ x') y) x
    tuple' i (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (flip (tuple_ i) x) y) xs
    tuple' _ _ _ = Fail
    tuple_ :: Id -> Integer -> ListTry a -> TryList a
    tuple_ i n xs
        | n == 0 = Val Nil
        | n > 0 =
            anyOf (leftId i) xs <&> \x -> Cons x (tuple_ (rightId i) (n - 1) xs)
        | otherwise = Fail

bifurcate :: Function
bifurcate = dup .* reverse'

flatten :: Function
flatten = unary flatten'
  where
    flatten' i (DListT xs) = DListT . concat_ . tryMap flatten' i <$> xs
    flatten' _ x = Val $ singleton_ x

pad :: Function
pad = unary pad'
  where
    pad' i x = padShape (leftId i) x >>= padTo (rightId i) x
    padShape :: Id -> DataTry -> TryList (Try Integer)
    padShape i (DListT xs) =
        Val
            . Cons (xs >>= length_)
            $ xs
                >>= tryFoldl tryMaxList (rightId i) Nil
                    . tryMap padShape (leftId i)
    padShape _ _ = Val Nil
    tryMaxList _ Nil ys = Val ys
    tryMaxList _ xs Nil = Val xs
    tryMaxList i (Cons x xs) (Cons y ys) =
        Val . Cons (liftJoinM2 tryMax x y) $ liftJoinM2 (tryMaxList i) xs ys
    padTo :: Id -> DataTry -> ListTry (Try Integer) -> TryData
    padTo _ x Nil = Val x
    padTo i (DListT xs) (Cons y ys) =
        DListT
            . Val
            . tryMap (\i' xs' -> ys >>= padTo i' xs') i
            <$> liftJoinM2 (padList zero) y xs
    padTo i x ys = padTo i (singleton_ x) ys
    zero = Val . DNumT . Val $ Det 0
    padList :: a -> Integer -> ListTry a -> TryList a
    padList _ 0 _ = Val Nil
    padList x n Nil = replicate_ x n
    padList x n (Cons y ys) = Val . Cons y $ ys >>= padList x (n - 1)

ordering :: Function
ordering = unary ordering'
  where
    ordering' i (DListT xs) = liftList (ordering_ i) xs
    ordering' _ _ = Fail
    ordering_ :: Id -> ListTry TryData -> TryList (Try Integer)
    ordering_ i xs =
        length_ xs
            >>= zipWithFail (\_ x y -> Val (OrdBy x y)) i xs
                . (\n -> Val <$> fromList [0 .. n - 1])
            >>= sort_
            <&> fmap (fmap ordVal)

minValue :: Function
minValue = allValues .* minimum'

maxValue :: Function
maxValue = allValues .* maximum'

elem' :: Function
elem' = binary elem''
  where
    elem'' _ x (DListT ys) = liftList (elem_ x) ys
    elem'' _ _ _ = Fail
    elem_ :: (TryEq a) => a -> ListTry (Try a) -> Try a
    elem_ _ Nil = Fail
    elem_ x (Cons y ys) =
        y >>= tryEq x >>= \b -> if b then Val x else ys >>= elem_ x

filterBy :: Function
filterBy = binary filterBy'
  where
    filterBy' i (DListT xs) (DListT ys) = liftList2 (filterBy_ i) xs ys
    filterBy' _ _ _ = Fail
    filterBy_ i xs ys =
        zipWithFail (\_ x y -> normalForm (Val y) >> Val x) i xs ys
            >>= filterTry
