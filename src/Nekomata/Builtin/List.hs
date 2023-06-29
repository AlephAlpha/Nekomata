{-# LANGUAGE LambdaCase #-}

module Nekomata.Builtin.List where

import Control.Arrow (first, (***))
import Control.Monad (liftM2)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Nekomata.Builtin.Basic (dup, eq)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

range0_ :: Rational -> [Integer]
range0_ x = enumFromTo 0 . floor $ x - 1

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
    anyOf'' i (DNumT x) = liftNum (anyOf i . fromList . range0_) x
    anyOf'' i (DStringT xs) = liftString (anyOf i) xs
    anyOf'' i (DListT xs) = liftList (anyOf i) xs

emptyList :: Function
emptyList = constant . Val . DListT $ Val Nil

singleton' :: Function
singleton' = unary . const $ toTryData . singleton

unsingleton :: Function
unsingleton = unary unsingleton'
  where
    unsingleton' _ (DStringT xs) = liftString unsingleton_ xs
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
    unpair' _ (DStringT xs) = liftString12 unpair_ xs
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
    length'' _ (DStringT xs) = liftString length_ xs
    length'' _ (DListT xs) = liftList length_ xs
    length'' _ _ = Fail

length_ :: ListTry a -> Try Integer
length_ Nil = Val 0
length_ (Cons _ xs) = xs >>= length_ <&> (+ 1)

lengthIs :: Function
lengthIs = binary lengthIs'
  where
    lengthIs' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . flip lengthIs_ x) y) xs
    lengthIs' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (flip lengthIs_ x) y) xs
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

nth :: Function
nth = binaryVecArg2 nth'
  where
    nth' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (flip nth_ x) y) xs
    nth' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (flip nth_ x) y) xs
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
        liftString12 (fmap (first AsString) . uncons_) xs
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
        liftString12 (fmap (first AsString) . unsnoc'') xs
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
reverse'' (DStringT xs) = liftString (AsString . reverse_ Nil) xs
reverse'' (DListT xs) = liftList (reverse_ Nil) xs

reverse_ :: ListTry a -> ListTry a -> TryList a
reverse_ ys Nil = Val ys
reverse_ ys (Cons x xs) = xs >>= reverse_ (Cons x (Val ys))

prefix :: Function
prefix = unary prefix'
  where
    prefix' i (DNumT x) = liftNum (prefix_ i . fromList . range0_) x
    prefix' i (DStringT xs) = liftString (AsString . prefix_ i) xs
    prefix' i (DListT xs) = liftList (prefix_ i) xs
    prefix_ :: Id -> ListTry a -> TryList a
    prefix_ _ Nil = Val Nil
    prefix_ i (Cons x xs) =
        Choice (leftId i) (Val Nil) (Val . Cons x $ xs >>= prefix_ (rightId i))

suffix :: Function
suffix = unary suffix'
  where
    suffix' i (DNumT x) = liftNum (suffix_ i . fromList . range0_) x
    suffix' i (DStringT xs) = liftString (AsString . suffix_ i) xs
    suffix' i (DListT xs) = liftList (suffix_ i) xs
    suffix_ :: Id -> ListTry a -> TryList a
    suffix_ _ Nil = Val Nil
    suffix_ i s@(Cons _ xs) =
        Choice (leftId i) (Val s) (xs >>= suffix_ (rightId i))

take' :: Function
take' = binaryVecArg2 take''
  where
    take'' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . flip take_ x) y) xs
    take'' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (flip take_ x) y) xs
    take'' _ _ _ = Fail
    take_ :: Integer -> ListTry a -> TryList a
    take_ 0 _ = Val Nil
    take_ n (Cons x xs) | n > 0 = Cons x . take_ (n - 1) <$> xs
    take_ _ _ = Fail

subset :: Function
subset = unary subset'
  where
    subset' i (DNumT x) = liftNum (subset_ i . fromList . range0_) x
    subset' i (DStringT xs) =
        liftString (AsString . subset_ i) xs
    subset' i (DListT xs) = liftList (subset_ i) xs
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
    subsequence' i (DNumT x) = liftNum (subsequence_ i . fromList . range0_) x
    subsequence' i (DStringT xs) = liftString (AsString . subsequence_ i) xs
    subsequence' i (DListT xs) = liftList (subsequence_ i) xs
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
join' = binary (const join'')

join'' :: DataTry -> DataTry -> TryData
join'' (DStringT xs) (DStringT ys) = liftString2 (AsString .: join_) xs ys
join'' x@(DStringT _) y = join'' x (DStringT (fromValue . show <$> toTry y))
join'' x y@(DStringT _) = join'' (DStringT (fromValue . show <$> toTry x)) y
join'' (DListT xs) (DListT ys) = liftList2 join_ xs ys
join'' _ _ = Fail

join_ :: ListTry a -> ListTry a -> TryList a
join_ Nil ys = Val ys
join_ (Cons x xs) ys = Val . Cons x $ liftJoinM2 join_ xs (Val ys)

split :: Function
split = unary2 split'
  where
    split' i (DNumT x) = liftNum12 (split_ i . fromList . range0_) x
    split' i (DStringT xs) =
        liftString12 (fmap (AsString *** AsString) . split_ i) xs
    split' i (DListT xs) = liftList12 (split_ i) xs
    split_ :: Id -> ListTry a -> Try (ListTry a, ListTry a)
    split_ _ Nil = Val (Nil, Nil)
    split_ i s@(Cons x xs) =
        Choice
            (leftId i)
            (Val (Nil, s))
            (xs >>= split_ (rightId i) <&> first (Cons x . Val))

replicate' :: Function
replicate' = binaryVecArg2 replicate''
  where
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

concat' :: Function
concat' = unary concat''
  where
    concat'' i (DListT xs) = xs >>= concat_ i
    concat'' _ _ = Fail
    concat_ _ Nil = Val . DListT $ Val Nil
    concat_ i (Cons x xs) = liftJoinM2 (tryFoldl (const join'') i) x xs

unconcat :: Function
unconcat = unary unconcat'
  where
    unconcat' i (DNumT x) = liftNum (unconcat_ i . fromList . range0_) x
    unconcat' i (DStringT xs) =
        liftString (fmap (fmap AsString) . unconcat_ i) xs
    unconcat' i (DListT xs) = liftList (unconcat_ i) xs
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
    permutation' i (DStringT xs) = liftString (AsString . permutation_ i) xs
    permutation' i (DListT xs) = liftList (permutation_ i) xs
    permutation_ :: Id -> ListTry a -> TryList a
    permutation_ _ Nil = Val Nil
    permutation_ i xs =
        extract_ (leftId i) xs >>= \(xs', x) ->
            Val $ Cons x (xs' >>= permutation_ (rightId i))

extract :: Function
extract = unary2 extract'
  where
    extract' i (DNumT x) = liftNum12 (extract_ i . fromList . range0_) x
    extract' i (DStringT xs) =
        liftString12 (fmap (first AsString) . extract_ i) xs
    extract' i (DListT xs) = liftList12 (extract_ i) xs

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
    allEqual' i (DStringT xs) = liftString (tryFoldl1 tryEq' i . fmap Val) xs
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
    rotate' _ (DStringT xs) (DNumT y) =
        liftString (\x -> liftInt (AsString . flip rotate_ x) y) xs
    rotate' _ (DListT xs) (DNumT y) =
        liftList (\x -> liftInt (flip rotate_ x) y) xs
    rotate' _ _ _ = Fail
    rotate_ :: Integer -> ListTry a -> TryList a
    rotate_ _ Nil = Val Nil
    rotate_ n s@(Cons x xs)
        | n > 0 =
            xs >>= flip join_ (singleton x) >>= rotate_ (n - 1)
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
    setPartition' i (DStringT xs) =
        liftString (fmap (fmap AsString) . setPartition_ i) xs
    setPartition' i (DListT xs) = liftList (setPartition_ i) xs
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
    setMinus' _ (DStringT xs) (DStringT ys) =
        liftString2 (AsString .: setMinus_) xs ys
    setMinus' _ (DListT xs) (DListT ys) = liftList2 setMinus_ xs ys
    setMinus' _ _ _ = Fail
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

tally :: Function
tally = unary2 tally'
  where
    tally' i (DStringT xs) = liftString12 (tally_ i . fmap Val) xs
    tally' i (DListT xs) = liftList12 (tally_ i) xs
    tally' _ _ = (Fail, Fail)
    tally_ ::
        (TryEq a) => Id -> ListTry (Try a) -> Try (ListTry a, ListTry Integer)
    tally_ i xs = unzip' <$> tryFoldl insertCount i Nil xs
    unzip' :: ListTry (a, b) -> (ListTry a, ListTry b)
    unzip' Nil = (Nil, Nil)
    unzip' (Cons x xs) =
        let ys = unzip' <$> xs
         in (Cons (fst x) (fst <$> ys), Cons (snd x) (snd <$> ys))
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
    intersect' _ (DStringT xs) (DStringT ys) =
        liftString2 (AsString .: intersect_) xs ys
    intersect' _ (DListT xs) (DListT ys) = liftList2 intersect_ xs ys
    intersect' _ _ _ = Fail
    intersect_ :: (TryEq a) => ListTry a -> ListTry a -> TryList a
    intersect_ Nil _ = Val Nil
    intersect_ (Cons x xs) ys =
        tryElem x ys >>= \b ->
            if b
                then Val $ Cons x (xs >>= flip intersect_ ys)
                else xs >>= flip intersect_ ys

union :: Function
union = binary union'
  where
    union' _ (DStringT xs) (DStringT ys) =
        liftString2 (AsString .: union_) xs ys
    union' _ (DListT xs) (DListT ys) = liftList2 union_ xs ys
    union' _ _ _ = Fail
    union_ :: (TryEq a) => ListTry a -> ListTry a -> TryList a
    union_ Nil xs = Val xs
    union_ (Cons x xs) ys =
        tryElem x ys >>= \b ->
            if b
                then xs >>= flip union_ ys
                else Val $ Cons x (xs >>= flip union_ ys)

chunks :: Function
chunks = unary chunks'
  where
    chunks' _ (DStringT xs) = liftString chunks_ xs
    chunks' _ (DListT xs) = liftList chunks_ xs
    chunks' _ _ = Fail
    chunks_ :: (TryEq a) => ListTry a -> TryList (TryList a)
    chunks_ Nil = Val Nil
    chunks_ (Cons x xs) =
        xs
            >>= spanEq x
            <&> \(ys, zs) -> Cons (Val $ Cons x (Val ys)) (chunks_ zs)
    spanEq :: (TryEq a) => a -> ListTry a -> Try (ListTry a, ListTry a)
    spanEq _ Nil = Val (Nil, Nil)
    spanEq x s@(Cons y ys) =
        tryEq x y
            >>= \b ->
                if b
                    then ys >>= spanEq x <&> first (Cons x . Val)
                    else Val (Nil, s)

deinterleave :: Function
deinterleave = unary2 deinterleave'
  where
    deinterleave' _ (DNumT x) = liftNum12 (deinterleave_ . fromList . range0_) x
    deinterleave' _ (DStringT xs) =
        liftString12 (fmap (AsString *** AsString) . deinterleave_) xs
    deinterleave' _ (DListT xs) = liftList12 deinterleave_ xs
    deinterleave_ :: ListTry a -> Try (ListTry a, ListTry a)
    deinterleave_ Nil = Val (Nil, Nil)
    deinterleave_ (Cons x xs) =
        xs >>= deinterleave_ <&> first (Cons x . Val) . swap

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
    length'' (DStringT xs) = xs >>= length_
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
    length'' (DStringT xs) = xs >>= length_
    length'' (DListT xs) = xs >>= length_
    length'' _ = Fail
