module Nekomata.Builtin.Math where

import Control.Arrow (second, (***))
import Control.Monad (liftM2)
import Data.Bits (popCount, xor, (.&.), (.|.))
import Data.Functor ((<&>))
import Data.List (sort)
import Data.Ratio (denominator, numerator, (%))
import Math.NumberTheory.ArithmeticFunctions (divisorsList)
import Math.NumberTheory.Primes (Prime (unPrime), factorise, nextPrime)
import Math.NumberTheory.Primes.Counting (primeCount)
import Math.NumberTheory.Primes.Testing (isCertifiedPrime)
import Math.NumberTheory.Roots (exactSquareRoot)
import Nekomata.Builtin.Basic (dup, swap)
import Nekomata.Builtin.List (flatten, length', reverse'')
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

isNonzero :: Function
isNonzero = predicateVec isNonzero'
  where
    isNonzero' _ x = (/= 0) . unDet <$> toTryNum x

isPositive :: Function
isPositive = predicateVec isPositive'
  where
    isPositive' _ x = (> 0) . unDet <$> toTryNum x

isNonnegative :: Function
isNonnegative = predicateVec isNonnegative'
  where
    isNonnegative' _ x = (>= 0) . unDet <$> toTryNum x

isZero :: Function
isZero = predicateVec isZero'
  where
    isZero' _ x = (== 0) . unDet <$> toTryNum x

isBig :: Function
isBig = predicateVec isBig'
  where
    isBig' _ x = (> 1) . abs . unDet <$> toTryNum x

isSmall :: Function
isSmall = predicateVec isSmall'
  where
    isSmall' _ x = (<= 1) . abs . unDet <$> toTryNum x

less :: Function
less = predicateVec2 less'
  where
    less' _ x y = tryLt (toTryNum x) (toTryNum y)

lessEq :: Function
lessEq = predicateVec2 lessEq'
  where
    lessEq' _ x y = tryLe (toTryNum x) (toTryNum y)

greater :: Function
greater = predicateVec2 greater'
  where
    greater' _ x y = tryGt (toTryNum x) (toTryNum y)

greaterEq :: Function
greaterEq = predicateVec2 greaterEq'
  where
    greaterEq' _ x y = tryGe (toTryNum x) (toTryNum y)

neg1 :: Function
neg1 = constant (-1 :: Integer)

ten :: Function
ten = constant (10 :: Integer)

octet :: Function
octet = constant (256 :: Integer)

neg :: Function
neg = unaryNum $ const negate

abs' :: Function
abs' = unaryNum $ const abs

increment :: Function
increment = unaryNum $ const (+ 1)

decrement :: Function
decrement = unaryNum $ const (subtract 1)

logicalNot :: Function
logicalNot = unaryNum $ const logicalNot_
  where
    logicalNot_ :: Rational -> Rational
    logicalNot_ 0 = 1
    logicalNot_ _ = 0

sign :: Function
sign = unaryNum $ const signum

add :: Function
add = binary add'

add' :: Id -> DataTry -> DataTry -> TryData
add' = vec2Pad add''
  where
    add'' _ x y = liftNum2 (+) (toTryNum x) (toTryNum y)

sub :: Function
sub = binary sub'

sub' :: Id -> DataTry -> DataTry -> TryData
sub' i x y = neg' (leftId i) y >>= add' (rightId i) x
  where
    neg' = vec1 neg''
    neg'' _ x' = liftNum negate (toTryNum x')

absDiff :: Function
absDiff = sub .* abs'

mul :: Function
mul = binary mul'

mul' :: Id -> DataTry -> DataTry -> TryData
mul' = vec2Fail mul''
  where
    mul'' _ x y = liftNum2 (*) (toTryNum x) (toTryNum y)

div' :: Function
div' = binaryNumFail $ const div_
  where
    div_ _ 0 = Fail
    div_ x y = Val $ x / y

divInt :: Function
divInt = binaryNumFail $ const divInt_
  where
    divInt_ :: Rational -> Rational -> Try Rational
    divInt_ _ 0 = Fail
    divInt_ x y = Val . fromInteger . floor $ x / y

mod' :: Function
mod' = binaryNumFail $ const mod_
  where
    mod_ _ 0 = Fail
    mod_ x y = Val $ x - y * fromInteger (floor $ x / y)

divExact :: Function
divExact = binaryNumFail $ const divExact_
  where
    divExact_ _ 0 = Fail
    divExact_ x y =
        let q = x / y in if denominator q == 1 then Val $ numerator q else Fail

divMod' :: Function
divMod' = binary2NumFail $ const divMod_
  where
    divMod_ _ 0 = Fail
    divMod_ x y =
        let q = fromInteger . floor $ x / y
         in Val (q, x - y * q)

half :: Function
half = unaryInt $ const half'
  where
    half' x = if even x then Val $ x `div` 2 else Fail

pow :: Function
pow = binaryNumFail $ const pow''
  where
    pow'' x y = toTryInt x <&> pow_ y
    pow_ x y | y >= 0 = Val $ x ^ y
    pow_ 0 _ = Fail
    pow_ x y = Val $ 1 / (x ^ (-y))

recip' :: Function
recip' = constant (1 :: Integer) .* swap .* div'

mul2 :: Function
mul2 = constant (2 :: Integer) .* mul

div2 :: Function
div2 = constant (2 :: Integer) .* div'

mod2 :: Function
mod2 = constant (2 :: Integer) .* mod'

powOf2 :: Function
powOf2 = constant (2 :: Integer) .* pow

numerator' :: Function
numerator' = unaryNum $ const numerator

denominator' :: Function
denominator' = unaryNum $ const denominator

min' :: Function
min' = binaryVecPad min''
  where
    min'' _ (DNumT x) (DNumT y) = liftNum2 tryMin x y
    min'' _ (DCharT x) (DCharT y) = liftChar2 tryMin x y
    min'' _ _ _ = Fail

max' :: Function
max' = binaryVecPad max''
  where
    max'' _ (DNumT x) (DNumT y) = liftInt2 tryMax x y
    max'' _ (DCharT x) (DCharT y) = liftChar2 tryMax x y
    max'' _ _ _ = Fail

ceil :: Function
ceil = unaryNum $ const (ceiling :: Rational -> Integer)

floor' :: Function
floor' = unaryNum $ const (floor :: Rational -> Integer)

natural :: Function
natural = nullary $
    \i -> toTryData <$> anyOf i $ fromList [0 :: Integer ..]

integer :: Function
integer = nullary $
    \i -> toTryData <$> anyOf i $ fromList integers
  where
    integers = (0 :: Integer) : [y | x <- [1 ..], y <- [x, -x]]

zero :: DataTry
zero = DNumT . Val $ Det 0

one :: DataTry
one = DNumT . Val $ Det 1

sum' :: Function
sum' = unary sum''

sum'' :: Id -> DataTry -> TryData
sum'' i (DListT xs) = liftList (tryFoldl add' i zero) xs
sum'' _ _ = Fail

product' :: Function
product' = unary product''
  where
    product'' i (DListT xs) = liftList (tryFoldl mul' i one) xs
    product'' _ _ = Fail

dot :: Function
dot = mul .* sum'

convolve :: Function
convolve = binary convolve'
  where
    convolve' i (DListT xs) (DListT ys) = liftList2 (convolve_ i) xs ys
    convolve' i x y = mul' i x y
    convolve_ :: Id -> ListTry TryData -> ListTry TryData -> ListTry TryData
    convolve_ _ Nil _ = Nil
    convolve_ _ _ Nil = Nil
    convolve_ i xs (Cons y ys) =
        zipWithPad
            add'
            (leftId i)
            (tryMap (\i' x -> y >>= convolve' i' x) (leftId (rightId i)) xs)
            ( Cons
                (toTryData (0 :: Integer))
                (convolve_ (rightId (rightId i)) xs <$> ys)
            )

mean :: Function
mean = dup .* sum' .* swap .* length' .* div'

fromBase :: Function
fromBase = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) b =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) (toTryNum b)) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldl (mulAdd b) i zero
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) x >>= add' (rightId i) y

fromBaseRev :: Function
fromBaseRev = binaryVecArg2 fromBase'
  where
    fromBase' i (DListT xs) b =
        liftList (\x -> liftNum (\b' -> fromBase_ i b' x) (toTryNum b)) xs
    fromBase' _ _ _ = Fail
    fromBase_ i b = tryFoldr (mulAdd b) i zero
    mulAdd b i x y =
        toTryData b >>= mul' (leftId i) y >>= add' (rightId i) x

toBaseRev :: Function
toBaseRev = binaryVecOuter toBaseRev'

toBaseRev' :: Id -> DataTry -> DataTry -> TryData
toBaseRev' _ x b = liftInt2 toBaseRev_ (toTryNum x) (toTryNum b)

toBaseRev_ :: Integer -> Integer -> TryList Integer
toBaseRev_ _ b | b < 1 = Fail
toBaseRev_ x b | x < 0 = toBaseRev_ (-x) b
toBaseRev_ x 1 = Val . fromList $ replicate (fromIntegral x) 1
toBaseRev_ 0 _ = Val Nil
toBaseRev_ x b = Val $ Cons (x `mod` b) (toBaseRev_ (x `div` b) b)

toBase :: Function
toBase = binaryVecOuter toBase'
  where
    toBase' i x y = toBaseRev' i x y >>= reverse''

binary' :: Function
binary' = constant (2 :: Integer) .* toBaseRev

fromBinary :: Function
fromBinary = constant (2 :: Integer) .* fromBaseRev

digits :: Function
digits = constant (10 :: Integer) .* toBase

fromDigits :: Function
fromDigits = constant (10 :: Integer) .* fromBase

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
    delta_ i s@(Cons _ xs) = xs >>= flip (Val .: zipWithTrunc sub' i) s

binomial :: Function
binomial = binaryNumFail $ const binomial'
  where
    binomial' n k = binomial_ n <$> toTryInt k
    binomial_ n k
        | k < 0 = 0
        | otherwise =
            product [n + 1 - fromInteger i | i <- [1 .. k]]
                / fromInteger (product [1 .. k])

factorial :: Function
factorial = unaryNum $ const factorial_
  where
    factorial_ x
        | x < 0 = Fail
        | otherwise = Val $ product [1 .. x]

isPrime' :: Function
isPrime' = predicateVec isPrime''
  where
    isPrime'' _ x = isCertifiedPrime <$> toTryInt' (toTryNum x)

prime :: Function
prime = nullary $
    \i ->
        toTryData
            <$> anyOf i
                . fromList
            $ map unPrime [nextPrime (1 :: Integer) ..]

primePi :: Function
primePi = unaryNum $ const primePi_
  where
    primePi_ = Val . primeCount . floor

factor :: Function
factor = unary2Num $ const factor_
  where
    factor_ 0 = Fail
    factor_ x =
        Val
            . unzip
            $ merge (factorInt $ numerator x) (factorInt $ denominator x)
    factorInt = sort . map (unPrime *** toInteger) . factorise
    merge [] ys = map (second negate) ys
    merge xs [] = xs
    merge xs@((p, n) : xs') ys@((q, m) : ys')
        | p == q = (p, n - m) : merge xs' ys'
        | p < q = (p, n) : merge xs' ys
        | otherwise = (q, -m) : merge xs ys'

gcd' :: Function
gcd' = binaryNumFail $ const gcd_
  where
    gcd_ x y =
        Val $
            gcd (numerator x) (numerator y)
                % lcm (denominator x) (denominator y)

lcm' :: Function
lcm' = binaryNumFail $ const lcm_
  where
    lcm_ x y =
        Val $
            lcm (numerator x) (numerator y)
                % gcd (denominator x) (denominator y)

divisors :: Function
divisors = unaryInt $ const divisors_
  where
    divisors_ 0 = Fail
    divisors_ x = toTryData $ divisorsList x

intPartition :: Function
intPartition = unaryInt $ intPartition_ 1
  where
    intPartition_ :: Integer -> Id -> Integer -> Try [Integer]
    intPartition_ _ _ 0 = Val []
    intPartition_ x i y
        | x > y = Fail
        | otherwise = do
            x' <- anyOf' (leftId i) [x .. y]
            p <- intPartition_ x' (rightId i) (y - x')
            return $ x' : p

sqrt' :: Function
sqrt' = unaryNum $ const sqrt_
  where
    sqrt_ x =
        liftM2
            (%)
            (exactSquareRoot (numerator x))
            (exactSquareRoot (denominator x))

unitVec2 :: Function
unitVec2 =
    nullary $
        \i ->
            Choice
                i
                (toTryData ([0, 1] :: [Integer]))
                (toTryData ([1, 0] :: [Integer]))

orNeg :: Function
orNeg = unaryNum orNeg_
  where
    orNeg_ _ 0 = Val 0
    orNeg_ i x = Choice i (Val x) (Val $ -x)

bitAnd :: Function
bitAnd = binaryIntFail $ const (.&.)

bitOr :: Function
bitOr = binaryIntPad $ const (.|.)

bitXor :: Function
bitXor = binaryIntPad $ const xor

popCount' :: Function
popCount' = unaryInt $ const (toInteger . popCount)

histogram :: Function
histogram = flatten .* powOf2 .* binary' .* sum'

sumEach :: Function
sumEach = unary sumEach'
  where
    sumEach' i (DListT xs) = liftList (tryMap sum'' i) xs
    sumEach' _ _ = Fail
