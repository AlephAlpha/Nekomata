module Nekomata.Builtin.String where

import Data.List (elemIndex)
import Nekomata.CodePage (codePage)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

bytes :: Function
bytes = unaryVec bytes'
  where
    bytes' _ (DStringT x) =
        liftString (fmap (fmap toInteger . flip elemIndex codePage)) x
    bytes' _ _ = Fail
