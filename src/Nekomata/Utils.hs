module Nekomata.Utils where

import Control.Monad (join, liftM2)

-- | A helper function to lift a binary function to a monad
liftJoinM2 :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftJoinM2 f x y = join $ liftM2 f x y

-- | A helper function to map two levels of functor
fmap2 :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
fmap2 = fmap . fmap
