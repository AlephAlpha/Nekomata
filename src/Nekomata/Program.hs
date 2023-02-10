module Nekomata.Program where

import Nekomata.Builtin
import Nekomata.Data
import Nekomata.Function
import Nekomata.Particle hiding (name)
import qualified Nekomata.Particle as Particle

-- | A term in a Nekomata program
data Term
    = ELit Data
    | EFunc Builtin
    | EPart BuiltinParticle Term
    | EBlock Program

instance Show Term where
    show (ELit d) = show d
    show (EFunc f) = '\\' : name f
    show (EPart p t) = show t ++ " " ++ '\\' : Particle.name p
    show (EBlock ts) = show ts

-- | A Nekomata program consists of a list of terms
newtype Program = Program {unProgram :: [Term]}

instance Show Program where
    show (Program ts) = "{" ++ unwords (map show ts) ++ "}"

-- | Compile a term into a function
compileTerm :: Term -> Either String Function
compileTerm (ELit d) = Right $ constant d
compileTerm (EFunc f) = Right $ func f
compileTerm (EPart p t) = compileTerm t >>= applyParticle p
compileTerm (EBlock ts) = compileProgram ts

-- | Compile a program into a function
compileProgram :: Program -> Either String Function
compileProgram (Program ts) = foldl compose identity <$> mapM compileTerm ts
