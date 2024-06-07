module Nekomata.Eval (
    Arity (..),
    Data (..),
    Function,
    NekomataData,
    NekomataError,
    Mode (..),
    Result (..),
    Runtime,
    allResults,
    checkResult,
    compile,
    countResults,
    eval,
    firstResult,
    getArity,
    initRuntime,
    readInput,
    runFunction,
    showResult,
    toResult,
) where

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
import Nekomata.Result
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
data Runtime = Runtime Id Stack

-- | Initialize Nekomata's runtime state with a list of input values
initRuntime :: [Data] -> Runtime
initRuntime = Runtime initId . initStack . map fromValue

-- | Read a Nekomata input string into a list of values
readInput :: String -> Either NekomataError [Data]
readInput =
    left CodePageError
        . checkCodePage
        >=> left ParseError
            . parse parseInput ""

-- | Nekomata's non-deterministic evaluation result
newtype NekomataData = NekomataData {fromNekomataData :: TryData}

-- | Run a Nekomata function with the given runtime state
runFunction :: Function -> Runtime -> (Runtime, NekomataData)
runFunction f (Runtime id' s) =
    let s' = apply f (leftId id') s
     in (Runtime (rightId id') s', NekomataData $ top s')

-- | Nekomata's evaluation mode
data Mode
    = -- | Show all results
      AllValues
    | -- | Show the first result
      FirstValue
    | -- | Count the number of results
      CountValues
    | -- | Check if there are any results
      CheckExistence
    deriving (Eq, Ord, Show)

-- | Show a Nekomata value
showData :: Data -> String
showData x = fromMaybe (show x) $ asString x

-- | Get all results of a Nekomata evaluation as a list of strings
allResults :: NekomataData -> [String]
allResults = map showData . values initDecisions . fromNekomataData

-- | Get the first result of a Nekomata evaluation as a string
firstResult :: NekomataData -> Maybe String
firstResult = fmap showData . values initDecisions . fromNekomataData

-- | Count the number of results of a Nekomata evaluation
countResults :: NekomataData -> Integer
countResults = countValues initDecisions . fromNekomataData

-- | Check if a Nekomata evaluation has any results
checkResult :: NekomataData -> Bool
checkResult = hasValue initDecisions . fromNekomataData

-- | Get the result of a Nekomata evaluation according to the mode
toResult :: Mode -> Maybe Int -> NekomataData -> Result
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
