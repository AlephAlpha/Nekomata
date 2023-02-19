module Nekomata.Eval where

import Control.Arrow (left)
import Control.Monad ((>=>))
import Nekomata.CodePage (CodePageError, checkCodePage)
import Nekomata.Data (Data, TryData)
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

-- | Show the result of a Nekomata evaluation according to the mode
showResult :: Mode -> TryData -> String
showResult AllValues = unlines . map show . values initDecisions
showResult FirstValue = maybe "" show . values initDecisions
showResult CountValues = show . countValues initDecisions
showResult CheckExistence = show . hasValue initDecisions

-- | Get all results of a Nekomata evaluation as a list of strings
allResults :: TryData -> [String]
allResults = map show . values initDecisions

-- | Evaluate a Nekomata program string according to the mode
eval :: Mode -> String -> String -> Either NekomataError String
eval mode code input = do
    f <- compile code
    inputData <- readInput input
    return . showResult mode . snd $ runFunction f (initRuntime inputData)

-- | Get the information of a built-in function or particle
