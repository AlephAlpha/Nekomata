module Repl (runRepl) where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf)
import Data.Map.Strict (keys, (!))
import qualified Data.Map.Strict as Map
import Nekomata.Builtin
import Nekomata.Eval
import qualified Nekomata.Particle as Particle
import System.Console.Haskeline

data ReplState = ReplState
    { mode :: Mode
    , runtime :: Runtime
    }

initReplState :: ReplState
initReplState = ReplState AllValues (initRuntime [])

data ReplCommand
    = ReplQuit
    | ReplHelp
    | ReplShowMode
    | ReplMode Mode
    | ReplInput String
    | ReplInfo String
    | ReplEval String
    deriving (Eq, Show)

parseReplCommand :: String -> ReplCommand
parseReplCommand input =
    case words input of
        ("\\Q" : _) -> ReplQuit
        ("\\Quit" : _) -> ReplQuit
        ("\\H" : _) -> ReplHelp
        ("\\Help" : _) -> ReplHelp
        ("\\Mode" : "all" : _) -> ReplMode AllValues
        ("\\Mode" : "first" : _) -> ReplMode FirstValue
        ("\\Mode" : "count" : _) -> ReplMode CountValues
        ("\\Mode" : "exists" : _) -> ReplMode CheckExistence
        ("\\Mode" : _) -> ReplShowMode
        ("\\Input" : rest) -> ReplInput (unwords rest)
        ("\\Info" : rest) -> ReplInfo (unwords rest)
        _ -> ReplEval input

replCommandStrings :: [String]
replCommandStrings =
    [ "\\Q"
    , "\\Quit"
    , "\\H"
    , "\\Help"
    , "\\Mode"
    , "\\Mode all"
    , "\\Mode first"
    , "\\Mode count"
    , "\\Mode exists"
    , "\\Input"
    , "\\Info"
    ]

helpString :: String
helpString =
    unlines
        [ "Nekomata REPL"
        , ""
        , "Commands:"
        , "  \\Q             Quit"
        , "  \\Quit          Quit"
        , "  \\H             Show this help"
        , "  \\Help          Show this help"
        , "  \\Mode          Show current mode"
        , "  \\Mode all      Show all results"
        , "  \\Mode first    Show first result"
        , "  \\Mode count    Show number of results"
        , "  \\Mode exists   Show whether there are any results"
        , "  \\Input <data>  Reset the stack with the given input"
        , "  \\Info <name>   Show information about a builtin"
        ]

builtinInfo :: String -> Maybe String
builtinInfo name' = infoByName name' <|> Particle.infoByName name'

repl :: ReplState -> InputT IO ReplState
repl state = do
    input <- getInputLine ">>> "
    case parseReplCommand <$> input of
        Nothing -> return state
        Just ReplQuit -> return state
        Just ReplHelp -> do
            outputStrLn helpString
            repl state
        Just ReplShowMode -> do
            outputStrLn $ "Current mode: " ++ show (mode state)
            repl state
        Just (ReplMode mode') -> do
            outputStrLn $ "Current mode: " ++ show mode'
            repl state{mode = mode'}
        Just (ReplInput xs) -> do
            case readInput xs of
                Left e -> do
                    outputStrLn $ "Invalid input: " ++ show e
                    repl state
                Right input' -> repl state{runtime = initRuntime input'}
        Just (ReplInfo xs) -> do
            case builtinInfo xs of
                Nothing -> do
                    outputStrLn $ "Unknown builtin: " ++ xs
                    repl state
                Just info' -> do
                    outputStrLn info'
                    repl state
        Just (ReplEval code) -> do
            case compile code of
                Left e -> do
                    outputStrLn $ "Invalid code: " ++ show e
                    repl state
                Right function' -> do
                    let (rt, result) = runFunction function' (runtime state)
                    outputStrLn $ showResult (mode state) result
                    repl state{runtime = rt}

replCommandCompletion :: String -> [Completion]
replCommandCompletion prefix =
    [simpleCompletion s | s <- replCommandStrings, prefix `isPrefixOf` s]

builtinCompletion :: String -> [Completion]
builtinCompletion prefix = functionCompletions ++ particleCompletions
  where
    functionCompletions =
        [ completion prefix name' short'
        | name' <- Map.keys builtinMap
        , tail prefix `isPrefixOf` name'
        , let short' = [short $ builtinMap ! name']
        ]
    particleCompletions =
        [ completion prefix name' short'
        | name' <- keys Particle.builtinParticleMap
        , tail prefix `isPrefixOf` name'
        , let short' = [Particle.short $ Particle.builtinParticleMap ! name']
        ]
    completion prefix' name' short' =
        let name'' = '\\' : name'
            info' = name'' ++ " ('" ++ short' ++ "')"
         in if name'' == prefix'
                then Completion short' info' False
                else Completion name'' info' False

replCompletion :: Monad m => CompletionFunc m
replCompletion (s, _) = do
    let (word, rest) = break (== '\\') s
    case rest of
        [] -> return (rest, [])
        _ : rest' -> do
            let word' = '\\' : reverse word
            let builtin = builtinCompletion word'
            let replCommand =
                    if null rest'
                        then replCommandCompletion word'
                        else []
            return (rest', builtin ++ replCommand)

replSettings :: Monad m => Settings m
replSettings = Settings replCompletion Nothing True

runRepl :: IO ()
runRepl = do
    putStrLn "Nekomata REPL - type \\H for help"
    _ <- runInputT replSettings (repl initReplState)
    return ()
