module Nekomata.Eval where

import Control.Monad ((>=>))
import Nekomata.CodePage (CodePageError, checkCodePage)
import Nekomata.Data
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

instance Show NekomataError where
    show (ParseError e) = show e
    show (CodePageError e) = show e
    show (ParticleArityError e) = show e

-- | Compile a Nekomata program string into a function
compile :: String -> Either NekomataError Function
compile =
    either (Left . CodePageError) Right . checkCodePage
        >=> either (Left . ParseError) Right . parse parseProgram ""
        >=> either (Left . ParticleArityError) Right . compileProgram

-- | Nekomata's runtime state
data Runtime = Runtime {choiceId :: Id, stack :: Stack}

-- | Initialize Nekomata's runtime state with a list of input values
initRuntime :: [Data] -> Runtime
initRuntime = Runtime initId . initStack . map fromValue

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
