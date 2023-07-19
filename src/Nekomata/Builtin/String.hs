{-# LANGUAGE LambdaCase #-}

module Nekomata.Builtin.String where

import Control.Monad ((>=>))
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Nekomata.CodePage (codePage)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser.Data
import Text.Parsec (parse)

charToInt :: Function
charToInt = unaryVec charToInt'
  where
    charToInt' _ (DStringT x) =
        liftString (fmap (fmap toInteger . flip elemIndex codePage)) x
    charToInt' _ _ = Fail

intToChar :: Function
intToChar = unary intToChar'
  where
    intToChar' _ (DNumT x) = liftInt intToChar_ x
    intToChar' _ (DListT xs) = liftList (AsString . intsToString) xs
    intToChar' _ _ = Fail
    intsToString :: ListTry TryData -> TryList Char
    intsToString Nil = Val Nil
    intsToString (Cons x xs) =
        x >>= \case
            DNumT x' ->
                toTryInt' x' >>= intToChar_ <&> flip Cons (xs >>= intsToString)
            _ -> Fail
    intToChar_ x
        | x >= 0 && x < 255 = Val $ codePage !! fromIntegral x
        | otherwise = Fail

read' :: Function
read' = unaryVec read''
  where
    read'' _ (DStringT x) = liftString ((toTry . fmap Det) >=> read_) x
    read'' _ _ = Fail
    read_ = either (const Fail) Val . parse parseData ""

show' :: Function
show' = unary show''
  where
    show'' :: Id -> DataTry -> TryData
    show'' _ x = Val $ DStringT (fromValue . show <$> toTry x)
