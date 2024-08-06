{-# LANGUAGE LambdaCase #-}

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
    infoMarkdown,
    infoByName,
) where

import Data.Functor (($>), (<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Builtin.List (concat', unconcat)
import Nekomata.Data
import Nekomata.Function hiding (arity)
import qualified Nekomata.Function as Function
import Nekomata.NonDet
import Nekomata.Result

{- | A particle is a higher-order function that modifies a function

Its behavior may depend on the arity of the function it is applied to,
or may fail if the arity is not suitable
-}
newtype Particle = Particle {runParticle :: Function -> Maybe Function}

-- | A builtin particle in Nekomata
data BuiltinParticle = BuiltinParticle
    { name :: String
    -- ^ The name of the particle
    , short :: Char
    -- ^ The short name of the particle
    , particle :: Particle
    -- ^ The particle itself
    , arity :: String
    -- ^ The arity of the particle
    , help :: String
    -- ^ The help message for the particle
    , examples :: [(String, Result)]
    -- ^ Some examples for the particle
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
        ++ if null (examples b)
            then ""
            else
                "\nExamples:\n"
                    ++ unlines
                        [ "  "
                            ++ example
                            ++ " -> "
                            ++ if result == all_ []
                                then "Fail"
                                else show result
                        | (example, result) <- examples b
                        ]

-- | Get the info string for a builtin particle in Markdown format
infoMarkdown :: BuiltinParticle -> String
infoMarkdown b =
    "### `"
        ++ name b
        ++ "` (`"
        ++ [short b]
        ++ "`, `"
        ++ arity b
        ++ "`)\n\n"
        ++ concatMap (++ "\n\n") (lines (help b))
        ++ if null (examples b)
            then ""
            else
                "__Examples__:\n\n"
                    ++ unlines
                        [ "- `"
                            ++ example
                            ++ "` → "
                            ++ if result == all_ []
                                then "Fail"
                                else "`" ++ show result ++ "`"
                        | (example, result) <- examples b
                        ]
                    ++ "\n"

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
        "onBoth"
        'ᵃ'
        onBoth
        "(0 -> n) -> (0 -> 2 * n) \
        \or (m -> n) -> (m + 1 -> 2 * n) where m > 0"
        "Apply a function to the top two values of the stack.\n\
        \If the function takes no argument, simply apply it twice."
        [("1 2 ᵃ{1+} Ð", all_ ["[2,3]"])]
    , BuiltinParticle
        "noPop"
        'ˣ'
        noPop
        "(m -> n) -> (m -> m + n)"
        "Apply a function without popping the stack."
        [("1 ˣ{1+} Ð", all_ ["[1,2]"])]
    , BuiltinParticle
        "dip"
        'ᵈ'
        dip
        "(m -> n) -> (m + 1 -> n + 1)"
        "Pop the top value of the stack, apply a function to the rest, \
        \and push the popped value back."
        [("1 2 ᵈ{1+} Ð", all_ ["[2,2]"])]
    , BuiltinParticle
        "dupDip"
        'ᵉ'
        dupDip
        "(m -> n) -> (m -> n + 1)"
        "Apply a function to the stack, \
        \and then push the original top value back."
        [("1 ᵈ{1+} Ð", all_ ["[2,1]"])]
    , BuiltinParticle
        "dupDip2"
        'ᵋ'
        dupDip2
        "(m -> n) -> (m -> n + 2)"
        "Apply a function to the stack, \
        \and then push the original top two values back."
        [("1 2 ᵋ{+} ÐÐ", all_ ["[3,[1,2]]"])]
    , BuiltinParticle
        "map"
        'ᵐ'
        map'
        "(0 -> 1) -> (1 -> 1) \
        \or (m -> 1) -> (m -> 1) where m > 0"
        "Apply a function to each value in a list.\n\
        \If the input is an number, apply the function to each integer \
        \from 0 to the input minus 1.\n\
        \If the function takes no argument, return a list of n copies \
        \of the result of the function, where n is the length of the input."
        []
    , BuiltinParticle
        "mapWith"
        'ᵚ'
        mapWith
        "(1 -> 1) -> (2 -> 1) \
        \or (m -> 1) -> (m -> 1) where m > 1"
        "Map a binary function over its first argument.\n\
        \If the function is unary, return a list of n copies of the \
        \result of applying the function to the second argument, where \
        \n is the length of the first argument."
        []
    , BuiltinParticle
        "zipWith"
        'ᶻ'
        zipWith'
        "(m -> 1) -> (m -> 1) where m > 1"
        "Zip two lists and apply a function to each pair of values.\n\
        \Fail if the lists have different lengths.\n\
        \If one of the input is an number, apply the function to each \
        \integer from 0 to the input minus 1."
        []
    , BuiltinParticle
        "zipWithTrunc"
        'ᶾ'
        zipWithTrunc'
        "(m -> 1) -> (m -> 1) where m > 1"
        "Zip two lists and apply a function to each pair of values.\n\
        \If the lists have different lengths, truncate the longer list \
        \to the length of the shorter list.\n\
        \If one of the input is an number, apply the function to each \
        \integer from 0 to the input minus 1."
        []
    , BuiltinParticle
        "outer"
        'ᵒ'
        outer
        "(m -> 1) -> (m -> 1) where m > 1"
        "Apply a function to every possible pair of values in two lists \
        \and return a list of lists.\n\
        \If one of the input is an number, apply the function to each \
        \integer from 0 to the input minus 1."
        []
    , BuiltinParticle
        "concatMap"
        'ʲ'
        concatMap'
        "(0 -> 1) -> (1 -> 1) \
        \or (m -> 1) -> (m -> 1) where m > 0"
        "Map a function over a list and concatenate the results.\n\
        \See the documentation for `concat` and `map`."
        []
    , BuiltinParticle
        "unconcatMap"
        'ᶣ'
        unconcatMap'
        "(0 -> 1) -> (1 -> 1) \
        \or (m -> 1) -> (m -> 1) where m > 0"
        "Unconcatenate a list, and then map a function over the results.\n\
        \See the documentation for `unconcat` and `map`."
        []
    , BuiltinParticle
        "predicate"
        'ᵖ'
        predicate'
        "(m -> n) -> (1 -> 1)"
        "Check if a function would succeed without actually applying it.\n\
        \If the function fails, replace the top value with Fail.\n\
        \Otherwise, do nothing."
        []
    , BuiltinParticle
        "predicateNot"
        'ᵗ'
        predicateNot'
        "(m -> n) -> (1 -> 1)"
        "Check if a function would fail without actually applying it.\n\
        \If the function does not fail, replace the top value with Fail.\n\
        \Otherwise, do nothing."
        []
    , BuiltinParticle
        "filter"
        'ᶠ'
        filter'
        "(m -> n) -> (1 -> 1)"
        "For each value in a list, check if a function would succeed \
        \without actually applying it, and remove the value if it fails.\n\
        \If the input is an number, convert it to a list of integers \
        \from 0 to the input minus 1 before filtering."
        []
    , BuiltinParticle
        "orApply"
        'ᶜ'
        orApply
        "(n -> n) -> (n -> n)"
        "Apply a function zero or one time non-deterministically."
        []
    , BuiltinParticle
        "iterate"
        'ᶦ'
        iterate'
        "(n -> n) -> (n -> n)"
        "Apply a function zero or more times non-deterministically, \
        \until the top value of the stack is Fail.\n\
        \This is different from `while` in that it returns \
        \the intermediate results."
        []
    , BuiltinParticle
        "nTimes"
        'ᵑ'
        nTimes
        "(n -> n) -> (n + 1 -> n)"
        "Take an integer from the top of the stack, \
        \and apply a function that many times."
        []
    , BuiltinParticle
        "while"
        'ʷ'
        while
        "(n -> n) -> (n -> n)"
        "Apply a function zero or more times, \
        \until the top value of the stack is Fail.\n\
        \This is different from `iterate` in that it does not \
        \return the intermediate results."
        []
    , BuiltinParticle
        "lengthWhile"
        'ˡ'
        lengthWhile
        "(n -> n) -> (n -> 1)"
        "Apply a function zero or more times, \
        \until the top value of the stack is Fail, \
        \and return the number of times the function was applied."
        []
    , BuiltinParticle
        "fixedPoint"
        'ʸ'
        fixedPoint
        "(n -> n) -> (n -> n)"
        "Apply a function zero or more times, \
        \until the top value of the stack no longer changes."
        []
    , BuiltinParticle
        "firstInt"
        'ᵏ'
        firstInt
        "(m -> n) -> (0 -> 1)"
        "Find the smallest non-negative integer for which a function \
        \does not fail, and return it."
        []
    , BuiltinParticle
        "fold1"
        'ʳ'
        fold1
        "(m -> 1) -> (m - 1 -> 1) where m > 1"
        "Apply a function to the first two values of a list, \
        \then apply it to the result and the third value, \
        \and so on until the end of the list.\n\
        \If the input is an number, convert it to a list of integers \
        \from 0 to the input minus 1 before folding."
        []
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
    = -- | Cannot find a particle with the given full name
      ParticleNotFound String
    | -- | Cannot find a particle with the given short name
      ParticleShortNotFound Char
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
    -- ^ The name of the particle
    , particleShort :: Char
    -- ^ The short name of the particle
    , particleArity :: String
    -- ^ The arity of the particle
    , functionArity :: String
    -- ^ The arity of the function applied to
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

onBoth :: Particle
onBoth = Particle onBoth'
  where
    onBoth' (Function (Arity 0 n) f) =
        Just
            . Function (Arity 0 (2 * n))
            $ \i s ->
                prepend
                    (takeStack n $ f (leftId i) s)
                    (f (rightId i) s)
    onBoth' (Function (Arity m n) f) =
        Just
            . Function (Arity (m + 1) (2 * n))
            $ \i (x :+ y :+ s) ->
                prepend
                    (takeStack n $ f (leftId i) (x :+ s))
                    (f (rightId i) (y :+ s))

noPop :: Particle
noPop = Particle noPop'
  where
    noPop' (Function (Arity m n) f) =
        Just
            . Function (Arity m (m + n))
            $ \i s -> prepend (takeStack n $ f i s) s

dip :: Particle
dip = Particle dip'
  where
    dip' (Function (Arity m n) f) =
        Just
            . Function (Arity (m + 1) (n + 1))
            $ \i (x :+ s) -> x :+ f i s

dupDip :: Particle
dupDip = Particle dupDip'
  where
    dupDip' (Function (Arity m n) f) =
        Just
            . Function (Arity m (n + 1))
            $ \i (x :+ s) -> x :+ f i (x :+ s)

dupDip2 :: Particle
dupDip2 = Particle dupDip2'
  where
    dupDip2' (Function (Arity m n) f) =
        Just
            . Function (Arity m (n + 2))
            $ \i (x :+ y :+ s) -> x :+ y :+ f i (x :+ y :+ s)

map' :: Particle
map' = Particle map''
  where
    map'' (Function (Arity 0 1) f) =
        Just
            . Function (Arity 1 1)
            $ \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (x >>= liftList (tryMap f' i) . toTryList) :+ s
    map'' (Function (Arity m 1) f) | m > 0 =
        Just
            . Function (Arity m 1)
            $ \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (x >>= liftList (tryMap f' i) . toTryList)
                        :+ dropStack (m - 1) s
    map'' _ = Nothing

mapWith :: Particle
mapWith = Particle mapWith'
  where
    mapWith' (Function (Arity 1 1) f) =
        Just
            . Function (Arity 2 1)
            $ \i (x :+ y :+ s) ->
                let f' i' y' = top $ f i' (x :+ Val y' :+ s)
                 in (y >>= liftList (tryMap f' i) . toTryList) :+ s
    mapWith' (Function (Arity m 1) f) | m > 1 =
        Just
            . Function (Arity m 1)
            $ \i (x :+ y :+ s) ->
                let f' i' y' = top $ f i' (x :+ Val y' :+ s)
                 in (y >>= liftList (tryMap f' i) . toTryList)
                        :+ dropStack (m - 2) s
    mapWith' _ = Nothing

zipWith' :: Particle
zipWith' = Particle zipWith''
  where
    zipWith'' (Function (Arity m 1) f) | m > 1 =
        Just
            . Function (Arity m 1)
            $ \i (x :+ y :+ s) ->
                let f' i' x' y' = top $ f i' (Val x' :+ Val y' :+ s)
                    f'' x' y' =
                        liftList2
                            (zipWithFail f' i)
                            (toTryList x')
                            (toTryList y')
                 in liftJoinM2 f'' x y :+ dropStack (m - 2) s
    zipWith'' _ = Nothing

zipWithTrunc' :: Particle
zipWithTrunc' = Particle zipWithTrunc''
  where
    zipWithTrunc'' (Function (Arity m 1) f) | m > 1 =
        Just
            . Function (Arity m 1)
            $ \i (x :+ y :+ s) ->
                let f' i' x' y' = top $ f i' (Val x' :+ Val y' :+ s)
                    f'' x' y' =
                        liftList2
                            (zipWithTrunc f' i)
                            (toTryList x')
                            (toTryList y')
                 in liftJoinM2 f'' x y :+ dropStack (m - 2) s
    zipWithTrunc'' _ = Nothing

outer :: Particle
outer = Particle outer'
  where
    outer' (Function (Arity m 1) f) | m > 1 =
        Just
            . Function (Arity m 1)
            $ \i (x :+ y :+ s) ->
                let f' i' x' y' = top $ f i' (Val x' :+ Val y' :+ s)
                    f'' x' y' =
                        liftList2
                            (tryOuter f' i)
                            (toTryList x')
                            (toTryList y')
                 in liftJoinM2 f'' x y :+ dropStack (m - 2) s
    outer' _ = Nothing

concatMap' :: Particle
concatMap' = Particle $ fmap (.* concat') . runParticle map'

unconcatMap' :: Particle
unconcatMap' = Particle $ fmap (unconcat .*) . runParticle map'

predicate' :: Particle
predicate' = Particle predicate''
  where
    predicate'' (Function (Arity _ _) f) =
        Just
            . Function (Arity 1 1)
            $ \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (normalForm x >>= (\x' -> normalForm (f' i x') $> x')) :+ s

predicateNot' :: Particle
predicateNot' = Particle predicateNot''
  where
    predicateNot'' (Function (Arity _ _) f) =
        Just
            . Function (Arity 1 1)
            $ \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (normalForm x >>= predicateNot_ (f' i)) :+ s
    predicateNot_ f x =
        Cut $ \ds -> (ds, if hasValue ds (f x) then Fail else Val x)

filter' :: Particle
filter' = Particle filter''
  where
    filter'' (Function (Arity _ _) f) =
        Just
            . Function (Arity 1 1)
            $ \i (x :+ s) ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                    f'' i' x' = normalForm (f' i' x') $> x'
                 in (x >>= liftList (filterTry . tryMap f'' i) . toTryList)
                        :+ s

orApply :: Particle
orApply = Particle orApply'
  where
    orApply' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity m n)
            $ \i s ->
                prepend
                    (takeStack n . tryStack $ orApply_ i f s)
                    (dropStack m s)
    orApply' _ = Nothing
    orApply_ i f s = Choice (leftId i) (Val s) (Val (f (rightId i) s))

iterate' :: Particle
iterate' = Particle iterate''
  where
    iterate'' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity m n)
            $ \i s ->
                prepend
                    (takeStack n . tryStack $ iterate_ i f s)
                    (dropStack m s)
    iterate'' _ = Nothing
    iterate_ i f s =
        Choice
            (leftId i)
            (Val s)
            ( let s' = f (leftId (rightId i)) s
               in normalForm (top s') $> s' >>= iterate_ (rightId (rightId i)) f
            )

nTimes :: Particle
nTimes = Particle nTimes'
  where
    nTimes' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity (m + 1) n)
            $ \i (x :+ s) ->
                prepend
                    (takeStack n . tryStack $ x >>= nTimes'' i f s)
                    (dropStack m s)
    nTimes' _ = Nothing
    nTimes'' i f s (DNumT x) = toTryInt' x >>= nTimes_ i f s
    nTimes'' _ _ _ _ = Fail
    nTimes_ _ _ s 0 = Val s
    nTimes_ _ _ _ x | x < 0 = Fail
    nTimes_ i f s x = f (leftId i) <$> nTimes_ (rightId i) f s (x - 1)

while :: Particle
while = Particle while'
  where
    while' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity m n)
            $ \i s ->
                prepend
                    (takeStack n . tryStack $ while'' i f s)
                    (dropStack m s)
    while' _ = Nothing
    while'' :: Id -> (Id -> Stack -> Stack) -> Stack -> Try Stack
    while'' i f s =
        Cut $ \ds ->
            ( ds
            , let s' = f (leftId i) s
                  t = normalForm (top s')
               in if hasValue ds t
                    then t $> s' >>= while'' (rightId i) f
                    else Val s
            )

lengthWhile :: Particle
lengthWhile = Particle lengthWhile'
  where
    lengthWhile' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity m 1)
            $ \i s -> toTryData (lengthWhile'' i f s) :+ dropStack m s
    lengthWhile' _ = Nothing
    lengthWhile'' :: Id -> (Id -> Stack -> Stack) -> Stack -> Try Integer
    lengthWhile'' i f s =
        Cut $ \ds ->
            ( ds
            , let s' = f (leftId i) s
                  t = normalForm (top s')
               in if hasValue ds t
                    then t $> s' >>= lengthWhile'' (rightId i) f <&> (+ 1)
                    else Val 0
            )

fixedPoint :: Particle
fixedPoint = Particle fixedPoint'
  where
    fixedPoint' (Function (Arity m n) f) | m == n =
        Just
            . Function (Arity m n)
            $ \i s ->
                prepend
                    (takeStack n . tryStack $ fixedPoint'' i f s)
                    (dropStack m s)
    fixedPoint' _ = Nothing
    fixedPoint'' :: Id -> (Id -> Stack -> Stack) -> Stack -> Try Stack
    fixedPoint'' i f s =
        let t = top s
            s' = f (leftId i) s
            t' = normalForm (top s')
         in tryEq t t' >>= \case
                True -> Val s
                False -> t' $> s' >>= fixedPoint'' (rightId i) f

firstInt :: Particle
firstInt = Particle firstInt'
  where
    firstInt' (Function (Arity _ _) f) =
        Just
            . Function (Arity 0 1)
            $ \i s ->
                let f' i' x' = top $ f i' (Val x' :+ s)
                 in (toTryData . Cut $ \ds -> (ds, firstInt'' ds i f' 0)) :+ s
    firstInt'' ::
        Decisions -> Id -> (Id -> DataTry -> TryData) -> Integer -> Try Integer
    firstInt'' ds i f x =
        let t = normalForm $ toTryData x >>= f (leftId i)
         in if hasValue ds t
                then Val x
                else firstInt'' ds (rightId i) f (x + 1)

fold1 :: Particle
fold1 = Particle fold1'
  where
    fold1' (Function (Arity m 1) f) | m > 1 =
        Just
            . Function (Arity (m - 1) 1)
            $ \i (x :+ s) ->
                let f' i' x' y' = top $ f i' (Val y' :+ Val x' :+ s)
                 in (x >>= liftList (tryFoldl1 f' i) . toTryList)
                        :+ dropStack (m - 2) s
    fold1' _ = Nothing
