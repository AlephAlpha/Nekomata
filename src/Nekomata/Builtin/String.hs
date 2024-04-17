module Nekomata.Builtin.String where

import Control.Monad ((>=>))
import Data.Word (Word8)
import Nekomata.CodePage (byteToChar)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser.Data
import Text.Parsec (eof, parse)

charToInt' :: Function
charToInt' = unaryVec charToInt''
  where
    charToInt'' _ (DCharT x) = liftChar charToInt_ x
    charToInt'' _ (DNumT x) = liftInt Just x
    charToInt'' _ _ = Fail
    charToInt_ :: Word8 -> Integer
    charToInt_ = fromIntegral

intToChar' :: Function
intToChar' = unaryVec intToChar''
  where
    intToChar'' _ (DNumT x) = liftInt intToChar_ x
    intToChar'' _ (DCharT x) = liftChar Just x
    intToChar'' _ _ = Fail
    intToChar_ :: Integer -> Maybe Word8
    intToChar_ x
        | x >= 0 && x < 255 = Just $ fromIntegral x
        | otherwise = Nothing

read' :: Function
read' = unary read''
  where
    read'' _ (DCharT x) = liftChar (\c -> read_ [byteToChar c]) x
    read'' _ x = toTryData $ (asString >=> read_) <$> toTry x
    read_ = either (const Nothing) Just . parse (parseData <* eof) ""

show' :: Function
show' = unary show''
  where
    show'' :: Id -> DataTry -> TryData
    show'' _ x = toTryData $ show <$> toTry x
