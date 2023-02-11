module Nekomata.Program where

import Nekomata.Builtin
import Nekomata.Data
import Nekomata.Function
import Nekomata.Particle

-- | A term in a Nekomata program
data Term
    = TLit Data
    | TFunc Builtin
    | TPart BuiltinParticle Term
    | TBlock Program

instance Show Term where
    show (TLit d) = show d
    show (TFunc f) = show f
    show (TPart p t) = show p ++ " " ++ show t
    show (TBlock ts) = show ts

-- | A Nekomata program consists of a list of terms
newtype Program = Program {unProgram :: [Term]}

instance Show Program where
    show (Program ts) = "{ " ++ unwords (map show ts) ++ " }"

-- | Compile a term into a function
compileTerm :: Term -> Either ParticleArityError Function
compileTerm (TLit d) = Right $ constant d
compileTerm (TFunc f) = Right $ func f
compileTerm (TPart p t) = compileTerm t >>= applyParticle p
compileTerm (TBlock ts) = compileProgram ts

-- | Compile a program into a function
compileProgram :: Program -> Either ParticleArityError Function
compileProgram (Program ts) = foldl compose identity <$> mapM compileTerm ts
