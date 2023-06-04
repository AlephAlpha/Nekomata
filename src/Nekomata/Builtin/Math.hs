module Nekomata.Builtin.Math where

import Control.Arrow (second, (***))
import Control.Monad (liftM2)
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Ratio (denominator, numerator, (%))
import Math.NumberTheory.Primes (Prime (unPrime), factorise, nextPrime)
import Math.NumberTheory.Primes.Counting (primeCount)
import Math.NumberTheory.Primes.Testing (isCertifiedPrime)
import Math.NumberTheory.Roots (exactSquareRoot)
import Nekomata.Builtin.Basic (dup, swap)
import Nekomata.Builtin.List (length', reverse'')
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

nonzero :: Function
nonzero = predicateVec nonzero'
  where
    nonzero' _ (DNumT x) = (/= 0) . unDet <$> x
    nonzero' _ _ = Fail

isPositive :: Function
isPositive = predicateVec isPositive'
  where
    isPositive' _ (DNumT x) = (> 0) . unDet <$> x
    isPositive' _ _ = Fail

less :: Function
less = predicateVec2 less'
  where
    less' _ (DNumT x) (DNumT y) = tryLt x y
    less' _ (DStringT x) (DStringT y) = tryLt x y
    less' _ _ _ = Fail

lessEq :: Function
lessEq = predicateVec2 lessEq'
  where
    lessEq' _ (DNumT x) (DNumT y) = tryLe x y
    lessEq' _ (DStringT x) (DStringT y) = tryLe x y
    lessEq' _ _ _ = Fail

greater :: Function
greater = predicateVec2 greater'
  where
    greater' _ (DNumT x) (DNumT y) = tryGt x y
    greater' _ (DStringT x) (DStringT y) = tryGt x y
    greater' _ _ _ = Fail

greaterEq :: Function
greaterEq = predicateVec2 greaterEq'
  where
    greaterEq' _ (DNumT x) (DNumT y) = tryGe x y
    greaterEq' _ (DStringT x) (DStringT y) = tryGe x y
    greaterEq' _ _ _ = Fail

neg1 :: Function
neg1 = constant (-1 :: Integer)

ten :: Function
ten = constant (10 :: Integer)

octet :: Function
octet = constant (256 :: Integer)

neg :: Function
neg = unaryVec neg'
  where
    neg' _ (DNumT x) = liftNum negate x
    neg' _ _ = Fail

abs' :: Function
abs' = unaryVec abs''
  where
    abs'' _ (DNumT x) = liftNum abs x
    abs'' _ _ = Fail

increment :: Function
increment = unaryVec increment'
  where
    increment' _ (DNumT x) = liftNum (+ 1) x
    increment' _ _ = Fail

decrement :: Function
decrement = unaryVec decrement'
  where
    decrement' _ (DNumT x) = liftNum (subtract 1) x
    decrement' _ _ = Fail

logicalNot :: Function
logicalNot = unaryVec logicalNot'
  where
    logicalNot' _ (DNumT x) = liftNum logicalNot_ x
    logicalNot' _ _ = Fail
    logicalNot_ :: Rational -> Rational
    logicalNot_ 0 = 1
    logicalNot_ _ = 0

sign :: Function
sign = unaryVec sign'
  where
    sign' _ (DNumT x) = liftNum signum x
    sign' _ _ = Fail

add :: Function
add = binary add'

add' :: Id -> DataTry -> DataTry -> TryData
add' = vec2Pad add''
  where
    add'' _ (DNumT x) (DNumT y) = liftNum2 (+) x y
    add'' _ _ _ = Fail

sub :: Function
sub = binary sub'

sub' :: Id -> DataTry -> DataTry -> TryData
sub' i x y = neg' (leftId i) y >>= add' (rightId i) x
  where
    neg' = vec1 neg''
    neg'' _ (DNumT x') = liftNum negate x'
    neg'' _ _ = Fail

mul :: Function
mul = binary mul'

mul' :: Id -> DataTry -> DataTry -> TryData
mul' = vec2Fail mul''
  where
    mul'' _ (DNumT x) (DNumT y) = liftNum2 (*) x y
    mul'' _ _ _ = Fail

div' :: Function
div' = binaryVecFail div''
  where
    div'' _ (DNumT x) (DNumT y) = liftNum2 div_ x y
    div'' _ _ _ = Fail
    div_ _ 0 = Fail
    div_ x y = Val $ x / y

divInt :: Function
divInt = binaryVecFail divInt'
  where
    divInt' _ (DNumT x) (DNumT y) = liftNum2 div_ x y
    divInt' _ _ _ = Fail
    div_ :: Rational -> Rational -> Try Rational
    div_ _ 0 = Fail
    div_ x y = Val . fromInteger . floor $ x / y

mod' :: Function
mod' = binaryVecFail mod''
  where
    mod'' _ (DNumT x) (DNumT y) = liftNum2 mod_ x y
    mod'' _ _ _ = Fail
    mod_ _ 0 = Fail
    mod_ x y = Val $ x - y * fromInteger (floor $ x / y)

divExact :: Function
divExact = binaryVecFail divExact'
  where
    divExact' _ (DNumT x) (DNumT y) = liftNum2 divExact_ x y
    divExact' _ _ _ = Fail
    divExact_ _ 0 = Fail
    divExact_ x y =
        let q = x / y in if denominator q == 1 then Val $ numerator q else Fail

half :: Function
half = unaryVec half'
  where
    half' _ (DNumT x) = liftInt half_ x
    half' _ _ = Fail
    half_ x = if even x then Val $ x `div` 2 else Fail

pow :: Function
pow = binaryVecFail pow'
  where
    pow' _ (DNumT x) (DNumT y) = liftNum2 pow'' y x
    pow' _ _ _ = Fail
    pow'' x y = toTryInt y <&> pow_ x
    pow_ x y | y >= 0 = Val $ x ^ y
    pow_ 0 _ = Fail
    pow_ x y = Val $ 1 / (x ^ (-y))

recip' :: Function
recip' = constant (1 :: Integer) .* swap .* div'

numerator' :: Function
numerator' = unaryVec numerator''
  where
    numerator'' _ (DNumT x) = liftNum numerator x
    numerator'' _ _ = Fail

denominator' :: Function
denominator' = unaryVec denominator''
  where
    denominator'' _ (DNumT x) = liftNum denominator x
    denominator'' _ _ = Fail

min' :: Function
min' = binaryVecPad min''
  where
    min'' _ (DNumT x) (DNumT y) = liftNum2 tryMin x y
    min'' _ (DStringT x) (DStringT y) = liftString2 (AsString .: tryMin) x y
    min'' _ _ _ = Fail

max' :: Function
max' = binaryVecPad max''
  where
    max'' _ (DNumT x) (DNumT y) = liftInt2 tryMax x y
    max'' _ (DStringT x) (DStringT y) = liftString2 (AsString .: tryMax) x y
    max'' _ _ _ = Fail

ceil :: Function
ceil = unaryVec ceil'
  where
    ceil' _ (DNumT x) = liftNum (ceiling :: Rational -> Integer) x
    ceil' _ _ = Fail

floor' :: Function
floor' = unaryVec floor''
  where
    floor'' _ (DNumT x) = liftNum (floor :: Rational -> Integer) x
    floor'' _ _ = Fail

natural :: Function
natural = nullary $
    \i -> toTryData <$> anyOf i $ fromList [0 :: Integer ..]

integer :: Function
integer = nullary $
    \i -> toTryData <$> anyOf i $ fromList integers
  where
    integers = (0 :: Integer) : [y | x <- [1 ..], y <- [x, -x]]

sum' :: Function
sum' = unary sum''
  where
    sum'' i (DListT xs) =
        liftList
            (tryFoldl add' i . DNumT . Val $ Det 0)
            xs
    sum'' _ _ = Fail

product' :: Function
product' = unary product''
  where
    product'' i (DListT xs) =
        liftList
            (tryFoldl mul' i . DNumT . Val $ Det 1)
            xs
    product'' _ _ = Fail

dot :: Function
dot = mul .* sum'

convolve :: Function
convolve = binary convolve'
  where
    convolve' i (DListT xs) (DListT ys) = liftList2 (convolve_ i) xs ys
    convolve' _ _ _ = Fail
    convolve_ :: Id -> ListTry TryData -> ListTry TryData -> ListTry TryData
    convolve_ _ Nil _ = Nil
    convolve_ _ _ Nil = Nil
    convolve_ i xs (Cons y ys) =
        zipWithPad
            add'
            (leftId i)
            (tryMap (\i' x -> y >>= mul' i' x) (leftId (rightId i)) xs)
            ( Cons
                (toTryData (0 :: Integer))
                (convolve_ (rightId (rightId i)) xs <$> ys)
            )

mean :: Function
mean = dup .* sum' .* swap .* length' .* div'

fromBase :: Function
fromBase = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) (DNumT b) =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) b) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldl (mulAdd b) i (DNumT . Val $ Det 0)
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) x >>= add' (rightId i) y

fromBaseRev :: Function
fromBaseRev = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) (DNumT b) =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) b) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldr (mulAdd b) i (DNumT . Val $ Det 0)
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) y >>= add' (rightId i) x

toBaseRev :: Function
toBaseRev = binaryVecOuter toBaseRev'

toBaseRev' :: Id -> DataTry -> DataTry -> TryData
toBaseRev' _ (DNumT x) (DNumT b) = liftInt2 toBaseRev_ x b
toBaseRev' _ _ _ = Fail

toBaseRev_ :: Integer -> Integer -> TryList Integer
toBaseRev_ _ b | b < 1 = Fail
toBaseRev_ x b | x < 0 = toBaseRev_ (-x) b
toBaseRev_ x 1 = Val . fromList $ replicate (fromIntegral x) 1
toBaseRev_ 0 _ = Val Nil
toBaseRev_ x b = Val $ Cons (x `mod` b) (toBaseRev_ (x `div` b) b)

toBase :: Function
toBase = binaryVecOuter toBase'
  where
    toBase' i x y = toBaseRev' (leftId i) x y >>= reverse'' (rightId i)

toBase2Rev :: Function
toBase2Rev = constant (2 :: Integer) .* toBaseRev

cumsum :: Function
cumsum = unary cumsum'
  where
    cumsum' i (DListT xs) = liftList (tryScanl1 add' i) xs
    cumsum' _ _ = Fail

delta :: Function
delta = unary delta'
  where
    delta' i (DListT xs) = liftList (delta_ i) xs
    delta' _ _ = Fail
    delta_ _ Nil = Fail
    delta_ i s@(Cons _ xs) = xs >>= flip (zipWithTrunc sub' i) s

binomial :: Function
binomial = binaryVecFail binomial'
  where
    binomial' _ (DNumT n) (DNumT k) = liftNum2 binomial'' n k
    binomial' _ _ _ = Fail
    binomial'' n k = binomial_ n <$> toTryInt k
    binomial_ n k =
        product [n + 1 - fromInteger i | i <- [1 .. k]]
            / fromInteger (product [1 .. k])

factorial :: Function
factorial = unaryVec factorial'
  where
    factorial' _ (DNumT x) = liftInt factorial_ x
    factorial' _ _ = Fail
    factorial_ x
        | x < 0 = Fail
        | otherwise = Val $ product [1 .. x]

isPrime' :: Function
isPrime' = predicateVec isPrime''
  where
    isPrime'' _ (DNumT x) = isCertifiedPrime <$> toTryInt' x
    isPrime'' _ _ = Fail

prime :: Function
prime = nullary $
    \i ->
        toTryData
            <$> anyOf i . fromList
            $ map unPrime [nextPrime (1 :: Integer) ..]

primePi :: Function
primePi = unaryVec primePi'
  where
    primePi' _ (DNumT x) = liftInt primePi_ x
    primePi' _ _ = Fail
    primePi_ x = Val $ primeCount x

factor :: Function
factor = unary2Vec factor'
  where
    factor' _ (DNumT x) = liftNum12 factor_ x
    factor' _ _ = (Fail, Fail)
    factor_ 0 = Fail
    factor_ x =
        Val . unzip $
            merge (factorInt $ numerator x) (factorInt $ denominator x)
    factorInt = sort . map (unPrime *** toInteger) . factorise
    merge [] ys = map (second negate) ys
    merge xs [] = xs
    merge xs@((p, n) : xs') ys@((q, m) : ys')
        | p == q = (p, n - m) : merge xs' ys'
        | p < q = (p, n) : merge xs' ys
        | otherwise = (q, m) : merge xs ys'

gcd' :: Function
gcd' = binaryVecFail gcd''
  where
    gcd'' _ (DNumT x) (DNumT y) = liftNum2 gcd_ x y
    gcd'' _ _ _ = Fail
    gcd_ x y =
        Val $
            gcd (numerator x) (numerator y)
                % lcm (denominator x) (denominator y)

lcm' :: Function
lcm' = binaryVecFail lcm''
  where
    lcm'' _ (DNumT x) (DNumT y) = liftNum2 lcm_ x y
    lcm'' _ _ _ = Fail
    lcm_ x y =
        Val $
            lcm (numerator x) (numerator y)
                % gcd (denominator x) (denominator y)

intPartition :: Function
intPartition = unaryVec intPartition'
  where
    intPartition' i (DNumT x) = liftInt (intPartition_ i 1) x
    intPartition' _ _ = Fail
    intPartition_ :: Id -> Integer -> Integer -> Try [Integer]
    intPartition_ _ _ 0 = Val []
    intPartition_ i x y
        | x > y = Fail
        | otherwise = do
            x' <- anyOf' (leftId i) [x .. y]
            p <- intPartition_ (rightId i) x' (y - x')
            return $ x' : p

sqrt' :: Function
sqrt' = unaryVec sqrt''
  where
    sqrt'' _ (DNumT x) = liftNum sqrt_ x
    sqrt'' _ _ = Fail
    sqrt_ x =
        liftM2
            (%)
            (exactSquareRoot (numerator x))
            (exactSquareRoot (denominator x))