module Nekomata.Eval where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Data.Maybe (fromMaybe)
import Nekomata.CodePage (CodePageError, checkCodePage)
import Nekomata.Data (Data (..), TryData)
import Nekomata.Function
import Nekomata.NonDet
import Nekomata.Parser
import Nekomata.Particle (ParticleArityError)
import Nekomata.Program
import Text.Parsec

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
    left CodePageError . checkCodePage
        >=> left ParseError . parse parseProgram ""
        >=> left ParticleArityError . compileProgram

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
showData (DString s) = s
showData x = show x

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

-- | Show the result of a Nekomata evaluation according to the mode
showResult :: Mode -> TryData -> String
showResult AllValues = unlines . allResults
showResult FirstValue = fromMaybe "" . firstResult
showResult CountValues = show . countResults
showResult CheckExistence = show . checkResult

-- | Evaluate a Nekomata program string according to the mode
eval :: Mode -> String -> String -> Either NekomataError String
eval mode code input = do
    f <- compile code
    inputData <- readInput input
    return . showResult mode . snd $ runFunction f (initRuntime inputData)

-- | Get the information of a built-in function or particle
