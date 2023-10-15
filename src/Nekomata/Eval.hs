module Nekomata.Eval where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import Nekomata.CodePage (CodePageError, checkCodePage)
import Nekomata.Data (Data (..), TryData, asString)
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser
import Nekomata.Particle (ParticleArityError)
import Nekomata.Program
import Text.Parsec (ParseError, parse)

-- | An error that can occur during parsing and evaluation
data NekomataError
    = ParseError ParseError
    | CodePageError CodePageError
    | ParticleArityError ParticleArityError
    deriving (Eq)

instance Show NekomataError where
    show (ParseError e) = show e
    show (CodePageError e) = show e
    show (ParticleArityError e) = show e

-- | Compile a Nekomata program string into a function
compile :: String -> Either NekomataError Function
compile =
    left CodePageError
        . checkCodePage
        >=> left ParseError
        . parse parseProgram ""
        >=> left ParticleArityError
        . compileProgram

-- | Nekomata's runtime state
data Runtime = Runtime {choiceId :: Id, stack :: Stack}

-- | Initialize Nekomata's runtime state with a list of input values
initRuntime :: [Data] -> Runtime
initRuntime = Runtime initId . initStack . map fromValue

readInput :: String -> Either NekomataError [Data]
readInput = left ParseError . parse parseInput ""

-- | Run a Nekomata function with the given runtime state
runFunction :: Function -> Runtime -> (Runtime, TryData)
runFunction f (Runtime id' s) =
    let s' = apply f (leftId id') s
     in (Runtime (rightId id') s', top s')

-- | Nekomata's evaluation mode
data Mode = AllValues | FirstValue | CountValues | CheckExistence
    deriving (Eq, Ord, Show)

-- | Show a Nekomata value
showData :: Data -> String
showData x = fromMaybe (show x) $ asString x

-- | Get all results of a Nekomata evaluation as a list of strings
allResults :: TryData -> [String]
allResults = map showData . values initDecisions

-- | Get the first result of a Nekomata evaluation as a string
firstResult :: TryData -> Maybe String
firstResult = fmap showData . values initDecisions

-- | Count the number of results of a Nekomata evaluation
countResults :: TryData -> Integer
countResults = countValues initDecisions

-- | Check if a Nekomata evaluation has any results
checkResult :: TryData -> Bool
checkResult = hasValue initDecisions

-- | The result of a Nekomata evaluation, to be shown to the user
data Result
    = All Bool [String]
    | First (Maybe String)
    | Count Integer
    | Check Bool
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

-- | Get the result of a Nekomata evaluation according to the mode
toResult :: Mode -> Maybe Int -> TryData -> Result
toResult AllValues Nothing = All False . allResults
toResult AllValues (Just n) = truncate' n . allResults
toResult FirstValue _ = First . firstResult
toResult CountValues _ = Count . countResults
toResult CheckExistence _ = Check . checkResult

-- | Evaluate a Nekomata program according to the mode
eval :: Mode -> Maybe Int -> Function -> String -> Either NekomataError Result
eval mode limit fun input =
    toResult mode limit
        . snd
        . runFunction fun
        . initRuntime
        <$> readInput input
