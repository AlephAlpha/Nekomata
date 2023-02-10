module Nekomata.Particle where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Function

{- | A particle is a higher-order function that modifies a function

Its behavior may depend on the arity of the function it is applied to,
or may fail if the arity is not suitable
-}
newtype Particle = Particle {runParticle :: Function -> Maybe Function}

-- | A builtin particle in Nekomata
data BuiltinParticle = BuiltinParticle
    { name :: String
    , particle :: Particle
    , help :: String
    }

-- | The list of all builtin particles
builtinParticles :: [BuiltinParticle]
builtinParticles =
    [ BuiltinParticle
        "apply2"
        apply2
        "(1 -> n) -> (2 -> 2 * n): Apply a function to two values."
    , BuiltinParticle
        "nonPop"
        nonPop
        "(m -> n) -> (0 -> n): Apply a function without popping the stack."
    , BuiltinParticle
        "dip"
        dip
        "(m -> n) -> (m + 1 -> n + 1): \
        \Pop the top value of the stack, apply a function to the rest, \
        \and push the popped value back."
    ]

-- | The map of all builtin particles
builtinParticleMap :: Map String BuiltinParticle
builtinParticleMap = Map.fromList $ map (\b -> (name b, b)) builtinParticles

applyParticle :: BuiltinParticle -> Function -> Either String Function
applyParticle p f = maybe (Left message) Right $ runParticle (particle p) f
  where
    message =
        "Cannot apply particle "
            ++ name p
            ++ " to function with arity "
            ++ show (arity f)

apply2 :: Particle
apply2 = Particle apply2'
  where
    apply2' (Function (Arity 1 n) f) =
        Just . Function (Arity 2 (2 * n)) $
            \i (x :+ y :+ s) ->
                prepend (takeStack n $ f i (x :+ s)) $ f i (y :+ s)
    apply2' _ = Nothing

nonPop :: Particle
nonPop = Particle nonPop'
  where
    nonPop' (Function (Arity _ n) f) =
        Just . Function (Arity 0 n) $
            \i s -> prepend (takeStack n $ f i s) s

dip :: Particle
dip = Particle dip'
  where
    dip' (Function (Arity m n) f) =
        Just . Function (Arity (m + 1) (n + 1)) $
            \i (x :+ s) -> x :+ f i s
