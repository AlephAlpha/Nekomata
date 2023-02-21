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
        "noPop"
        'ˣ'
        noPop
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
        "(m -> 1) -> (m -> 1) where m > 0"
        "Apply a function to each value in a list.\n\
        \If the input is a string, apply the function to each character.\n\
        \If the input is an integer, apply the function to each integer \
        \from 0 to the input minus 1."
    , BuiltinParticle
        "zipWith"
        'ᶻ'
        zipWith'
        "(m -> 1) -> (m -> 1) where m > 1"
        "Zip two lists and apply a function to each pair of values.\n\
        \If one of the input is a string, apply the function to each \
        \character.\n\
        \If one of the input is an integer, apply the function to each \
        \integer from 0 to the input minus 1."
    , BuiltinParticle
        "outer"
        'ᵒ'
        outer
        "(m -> 1) -> (m -> 1) where m > 1"
        "Apply a function to every possible pair of values in two lists \
        \and return a list of lists.\n\
        \If one of the input is a string, apply the function to each \
        \character.\n\
        \If one of the input is an integer, apply the function to each \
        \integer from 0 to the input minus 1."
    , BuiltinParticle
        "predicate"
        'ᵖ'
        predicate'
        "(m -> n) -> (1 -> 1)"
        "Apply a function without pushing or popping the stack, \
        \but replace the top value with Fail if the function fails."
    , BuiltinParticle
        "repeatNonDet"
        'ⁿ'
        repeatNonDet
        "(n -> n) -> (n -> n)"
        "Apply a function zero or more times non-deterministically, \
        \until the top value of the stack is Fail.\n\
        \This is different from `while` in that it returns \
        \the intermediate results."
    , BuiltinParticle
        "while"
        'ʷ'
        while
        "(n -> n) -> (n -> n)"
        "Apply a function zero or more times, \
        \until the top value of the stack is Fail.\n\
        \This is different from `repeatNonDet` in that it does not \
        \return the intermediate results."
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
                prepend
                    (takeStack n $ f (leftId i) (x :+ s))
                    (f (rightId i) (y :+ s))

noPop :: Particle
noPop = Particle noPop'
  where
    noPop' (Function (Arity _ n) f) =
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
    map'' (Function (Arity m 1) f) | m > 0 =
        Just . Function (Arity m 1) $
            \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (x >>= liftList (tryMap f' i) . toTryList)
                        :+ dropStack (m - 1) s
    map'' _ = Nothing

zipWith' :: Particle
zipWith' = Particle zipWith''
  where
    zipWith'' (Function (Arity m 1) f) | m > 1 =
        Just . Function (Arity m 1) $
            \i (x :+ y :+ s) ->
                let f' i' x' y' = top $ f i' (Val x' :+ Val y' :+ s)
                    f'' x' y' =
                        liftList2
                            (zipWithFail f' i)
                            (toTryList x')
                            (toTryList y')
                 in liftJoinM2 f'' x y :+ dropStack (m - 2) s
    zipWith'' _ = Nothing

outer :: Particle
outer = Particle outer'
  where
    outer' (Function (Arity m 1) f) | m > 1 =
        Just . Function (Arity m 1) $
            \i (x :+ y :+ s) ->
                let f' i' x' y' = top $ f i' (Val x' :+ Val y' :+ s)
                    f'' x' y' =
                        liftList2
                            (tryOuter f' i)
                            (toTryList x')
                            (toTryList y')
                 in liftJoinM2 f'' x y :+ dropStack (m - 2) s
    outer' _ = Nothing

predicate' :: Particle
predicate' = Particle predicate''
  where
    predicate'' (Function (Arity _ _) f) =
        Just . Function (Arity 1 1) $
            \i (x :+ s) ->
                ( normalForm x
                    >>= predicate_ (\x' -> top (f i (Val x' :+ s)))
                )
                    :+ s
    predicate_ f x = Cut $ \ds -> if hasValue ds (f x) then Val x else Fail

repeatNonDet :: Particle
repeatNonDet = Particle repeatNonDet'
  where
    repeatNonDet' (Function (Arity m n) f) | m == n =
        Just . Function (Arity m n) $
            \i s ->
                prepend
                    (takeStack n . tryStack $ repeatNonDet'' i f s)
                    (dropStack m s)
    repeatNonDet' _ = Nothing
    repeatNonDet'' i f s =
        Choice
            (leftId i)
            (Val s)
            ( Val (f (leftId (rightId i)) s)
                >>= (\s' -> top s' >> Val s')
                >>= repeatNonDet'' (rightId (rightId i)) f
            )

while :: Particle
while = Particle while'
  where
    while' (Function (Arity m n) f) | m == n =
        Just . Function (Arity m n) $
            \i s ->
                prepend
                    (takeStack n . tryStack $ while'' i f s)
                    (dropStack m s)
    while' _ = Nothing
    while'' :: Id -> (Id -> Stack -> Stack) -> Stack -> Try Stack
    while'' i f s =
        Cut $ \ds ->
            let s' = f (leftId i) s
             in if hasValue ds (top s')
                    then normalForm (top s') >> Val s' >>= while'' (rightId i) f
                    else Val s
