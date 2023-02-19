module Nekomata.Particle (
    Particle (..),
    BuiltinParticle (..),
    builtinParticles,
    builtinParticleMap,
    builtinParticleShortMap,
    ParticleNotFoundError (..),
    ParticleArityError (..),
    applyParticle,
    info,
    infoByName,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Data
import Nekomata.Function hiding (arity)
import qualified Nekomata.Function as Function
import Nekomata.NonDet

{- | A particle is a higher-order function that modifies a function

Its behavior may depend on the arity of the function it is applied to,
or may fail if the arity is not suitable
-}
newtype Particle = Particle {runParticle :: Function -> Maybe Function}

-- | A builtin particle in Nekomata
data BuiltinParticle = BuiltinParticle
    { name :: String
    , short :: Char
    , particle :: Particle
    , arity :: String
    , help :: String
    }

instance Show BuiltinParticle where
    show b = "\\" ++ name b

-- | Get the info string for a builtin particle
info :: BuiltinParticle -> String
info b =
    show b
        ++ " ('"
        ++ [short b]
        ++ "', "
        ++ arity b
        ++ "):\n"
        ++ help b

-- | Get the info string for a builtin particle by name
infoByName :: String -> Maybe String
infoByName name' =
    info <$> case name' of
        [short'] -> Map.lookup short' builtinParticleShortMap
        '\\' : name'' -> Map.lookup name'' builtinParticleMap
        _ -> Map.lookup name' builtinParticleMap

-- | The list of all builtin particles
builtinParticles :: [BuiltinParticle]
builtinParticles =
    [ BuiltinParticle
        "apply2"
        'ᵃ'
        apply2
        "(m -> n) -> (m + 1 -> 2 * n)"
        "Apply a function to the top two values of the stack."
    , BuiltinParticle
        "nonPop"
        'ᵒ'
        nonPop
        "(m -> n) -> (0 -> n)"
        "Apply a function without popping the stack."
    , BuiltinParticle
        "dip"
        'ᵈ'
        dip
        "(m -> n) -> (m + 1 -> n + 1)"
        "Pop the top value of the stack, apply a function to the rest, \
        \and push the popped value back."
    , BuiltinParticle
        "dupDip"
        'ᵉ'
        dupDip
        "(m -> n) -> (m -> n + 1)"
        "Duplicate the top value of the stack, pop the top value, \
        \apply a function to the rest, and push the popped value back."
    , BuiltinParticle
        "map"
        'ᵐ'
        map'
        "(m -> 1) -> (m -> 1)"
        "Apply a function to each value in a list, \
        \or to each character in a string, \
        \or to each integer from 1 to an integer."
    , BuiltinParticle
        "predicate"
        'ᵖ'
        predicate'
        "(m -> n) -> (1 -> 1)"
        "Apply a function without pushing or popping the stack, \
        \but replace the top value with Fail if the function fails."
    , BuiltinParticle
        "repeatNonDet"
        'ʳ'
        repeatNonDet
        "(1 -> 1) -> (1 -> 1)"
        "Apply a function to the top value of the stack zero or more times. \
        \The function must take a single argument and return a single value."
    ]

-- | The map of from names to builtin particles
builtinParticleMap :: Map String BuiltinParticle
builtinParticleMap = Map.fromList $ map (\b -> (name b, b)) builtinParticles

-- | The map of from short names to builtin particles
builtinParticleShortMap :: Map Char BuiltinParticle
builtinParticleShortMap =
    Map.fromList $ map (\b -> (short b, b)) builtinParticles

-- | An error that occurs when a builtin particle is not found
data ParticleNotFoundError
    = ParticleNotFound String
    | ParticleShortNotFound Char
    deriving (Eq)

instance Show ParticleNotFoundError where
    show (ParticleNotFound name') =
        "cannot find particle with full name \"\\" ++ name' ++ "\""
    show (ParticleShortNotFound short') =
        "cannot find particle with short name '" ++ [short'] ++ "'"

{- | An error that occurs when applying a particle to a function
with the wrong arity
-}
data ParticleArityError = ParticleArityError
    { particleName :: String
    , particleShort :: Char
    , particleArity :: String
    , functionArity :: String
    }
    deriving (Eq)

instance Show ParticleArityError where
    show (ParticleArityError name' short' arity1 arity2) =
        "Cannot apply particle "
            ++ name'
            ++ " ('"
            ++ [short']
            ++ "') with arity "
            ++ arity1
            ++ " to function with arity "
            ++ arity2
            ++ "."

-- | Apply a builtin particle to a function
applyParticle ::
    BuiltinParticle ->
    Function ->
    Either ParticleArityError Function
applyParticle p f = maybe (Left message) Right $ runParticle (particle p) f
  where
    message =
        ParticleArityError
            { particleName = show p
            , particleShort = short p
            , particleArity = arity p
            , functionArity = show $ Function.arity f
            }

apply2 :: Particle
apply2 = Particle apply2'
  where
    apply2' (Function (Arity m n) f) =
        Just . Function (Arity (m + 1) (2 * n)) $
            \i (x :+ y :+ s) ->
                prepend (takeStack n $ f i (x :+ s)) $ f i (y :+ s)

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

dupDip :: Particle
dupDip = Particle dupDip'
  where
    dupDip' (Function (Arity m n) f) =
        Just . Function (Arity m (n + 1)) $
            \i (x :+ s) -> x :+ f i (x :+ s)

map' :: Particle
map' = Particle map''
  where
    map'' (Function (Arity m 1) f) =
        Just . Function (Arity m 1) $
            \i (x :+ s) ->
                let f' i' x' = top $ f i' (pure x' :+ s)
                 in (x >>= liftList (tryMap f' i) . toDListT)
                        :+ dropStack (m - 1) s
    map'' _ = Nothing
    toDListT (DListT xs) = xs
    toDListT (DIntT x) = fromList . map toTryData . enumFromTo 1 <$> toTry x
    toDListT (DStringT xs) = fmap (Val . DStringT . Val . singleton) <$> xs

predicate' :: Particle
predicate' = Particle predicate''
  where
    predicate'' (Function (Arity _ _) f) =
        Just . Function (Arity 1 1) $
            \i (x :+ s) ->
                let (x' :+ _) = f i (x :+ s)
                    x'' = Cut $ \ds -> toTryData . maybe Fail Val $ values ds x'
                 in (x'' >> x) :+ s

repeatNonDet :: Particle
repeatNonDet = Particle repeatNonDet'
  where
    repeatNonDet' (Function (Arity 1 1) f) =
        Just . Function (Arity 1 1) $
            \i (x :+ s) ->
                (x >>= repeatNonDet'' i (\i' x' -> top $ f i' (Val x' :+ s)))
                    :+ s
    repeatNonDet' _ = Nothing
    repeatNonDet'' i f x =
        Choice
            (leftId i)
            (Val x)
            ( f (leftId (rightId i)) x
                >>= repeatNonDet'' (rightId (rightId i)) f
            )
