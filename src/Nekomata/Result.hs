module Nekomata.Result (
    Result (..),
    all_,
    first_,
    nothing_,
    showResult,
    truncate',
    truncate_,
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

-- | All results, not truncated
all_ :: [String] -> Result
all_ = All False

-- | First few results
truncate_ :: [String] -> Result
truncate_ = All True

-- | The first result
first_ :: String -> Result
first_ = First . Just

-- | No results
nothing_ :: Result
nothing_ = First Nothing