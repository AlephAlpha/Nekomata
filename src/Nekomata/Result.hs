module Nekomata.Result (
    Result (..),
    showResult,
    truncate',
) where

import Data.Maybe (fromMaybe)

-- | The result of a Nekomata evaluation, to be shown to the user
data Result
    = -- | All results, possibly truncated
      All Bool [String]
    | -- | The first result
      First (Maybe String)
    | -- | The number of results
      Count Integer
    | -- | Whether there are any results
      Check Bool
    deriving (Eq)

instance Show Result where
    show (All truncated xs) = unwords xs ++ if truncated then " ..." else ""
    show (First x) = fromMaybe "" x
    show (Count n) = show n
    show (Check b) = show b

-- | Show a Nekomata result separated by newlines
showResult :: Result -> [String]
showResult (All truncated xs) = xs ++ ["..." | truncated]
showResult (First x) = [fromMaybe "" x]
showResult (Count n) = [show n]
showResult (Check b) = [show b]

-- | Truncate a list of strings to a given length
truncate' :: Int -> [String] -> Result
truncate' n xs =
    let (ys, zs) = splitAt n xs in All (not $ null zs) ys