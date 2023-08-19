module Nekomata.Builtin.String where

import Control.Monad ((>=>))
import Data.List (elemIndex)
import Nekomata.CodePage (codePage)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser.Data
import Text.Parsec (eof, parse)

charToInt :: Function
charToInt = unaryVec charToInt'
  where
    charToInt' _ (DCharT x) =
        liftChar (fmap toInteger . flip elemIndex codePage) x
    charToInt' _ _ = Fail

intToChar :: Function
intToChar = unaryVec intToChar'
  where
    intToChar' _ (DNumT x) = liftInt intToChar_ x
    intToChar' _ _ = Fail
    intToChar_ x
        | x >= 0 && x < 255 = Val $ codePage !! fromIntegral x
        | otherwise = Fail

read' :: Function
read' = unary read''
  where
    read'' _ (DCharT x) = liftChar (\c -> read_ [c]) x
    read'' _ x = toTryData $ (asString >=> read_) <$> toTry x
    read_ = either (const Nothing) Just . parse (parseData <* eof) ""

show' :: Function
show' = unary show''
  where
    show'' :: Id -> DataTry -> TryData
    show'' _ x = toTryData $ show <$> toTry x
