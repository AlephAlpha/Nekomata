module Nekomata.Builtin.String where

import Control.Monad ((>=>))
import Nekomata.CodePage (charToInt, intToChar)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser.Data
import Text.Parsec (eof, parse)

charToInt' :: Function
charToInt' = unaryVec charToInt''
  where
    charToInt'' _ (DCharT x) = liftChar (charToInt :: Char -> Maybe Integer) x
    charToInt'' _ _ = Fail

intToChar' :: Function
intToChar' = unaryVec intToChar''
  where
    intToChar'' _ (DNumT x) = liftInt intToChar x
    intToChar'' _ _ = Fail

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
