module Nekomata.Builtin.Basic where

import Control.Arrow (second)
import Nekomata.Data
import Nekomata.Function
import Nekomata.NonDet

choice :: Function
choice = Function (Arity 2 1) $ \i (x :+ y :+ s) -> Choice i y x :+ s

fail' :: Function
fail' = constant (Fail :: TryData)

allValues :: Function
allValues = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> (ds, toTryData . fromList $ values ds x)) :+ s

oneValue :: Function
oneValue = Function (Arity 1 1) $
    \_ (x :+ s) ->
        Cut (\ds -> maybe (ds, Fail) (second toTryData) $ firstValue ds x) :+ s

countValues' :: Function
countValues' = Function (Arity 1 1) $
    \_ (x :+ s) -> Cut (\ds -> (ds, toTryData $ countValues ds x)) :+ s

normalForm' :: Function
normalForm' = Function (Arity 1 1) $ \_ (x :+ s) -> normalForm x :+ s

if' :: Function
if' = choice .* oneValue

drop' :: Function
drop' = Function (Arity 1 0) $ \_ (_ :+ s) -> s

dup :: Function
dup = Function (Arity 1 2) $ \_ (x :+ s) -> x :+ x :+ s

swap :: Function
swap = Function (Arity 2 2) $ \_ (x :+ y :+ s) -> y :+ x :+ s

eq :: Function
eq = predicate2 $ \_ x y -> tryEq x y

ne :: Function
ne = predicate2 $ \_ x y -> tryNe x y
