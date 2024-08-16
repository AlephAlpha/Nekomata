module Repl (runRepl) where

import Control.Applicative ((<|>))
import Data.List (isPrefixOf)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Version (showVersion)
import Nekomata.Builtin (Builtin (short), builtinMap, infoByName)
import Nekomata.Eval
import qualified Nekomata.Particle as Particle
import Paths_Nekomata (version)
import System.Console.Haskeline
import Text.Read (readMaybe)

data ReplState = ReplState
    { mode :: Mode
    , runtime :: Runtime
    , limit :: Int
    }

initReplState :: ReplState
initReplState = ReplState AllValues (initRuntime []) 16

data ReplCommand
    = ReplQuit
    | ReplHelp
    | ReplShowMode
    | ReplMode Mode
    | ReplLimit String
    | ReplInput String
    | ReplInfo String
    | ReplEval String
    | ReplArity String
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
        ("\\Mode" : "last" : _) -> ReplMode LastValue
        ("\\Mode" : "count" : _) -> ReplMode CountValues
        ("\\Mode" : "exists" : _) -> ReplMode CheckExistence
        ("\\Mode" : _) -> ReplShowMode
        ("\\Limit" : rest) -> ReplLimit (unwords rest)
        ("\\Input" : rest) -> ReplInput (unwords rest)
        ("\\Info" : rest) -> ReplInfo (unwords rest)
        ("\\Arity" : rest) -> ReplArity (unwords rest)
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
    , "\\Mode last"
    , "\\Mode count"
    , "\\Mode exists"
    , "\\Limit"
    , "\\Input"
    , "\\Info"
    , "\\Arity"
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
        , "  \\Mode          Show current mode (default: all)"
        , "  \\Mode all      Show all results"
        , "  \\Mode first    Show the first result"
        , "  \\Mode last     Show the last result"
        , "  \\Mode count    Show the number of results"
        , "  \\Mode exists   Show whether there are any results"
        , "  \\Limit <n>     Set the max number of results to show (default: 16)"
        , "  \\Input <data>  Reset the stack with the given input"
        , "  \\Info <name>   Show information about a builtin"
        , "  \\Arity <code>  Show the arity of a function"
        , ""
        , "Press TAB to complete commands and builtins."
        , "Add a space after the full name of a builtin and press TAB \
          \to convert it to its short form."
        ]

builtinInfo :: String -> Maybe String
builtinInfo name' = infoByName name' <|> Particle.infoByName name'

repl :: ReplState -> InputT IO ReplState
repl state = handleInterrupt (outputStrLn "Cancelled." >> repl state) $ do
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
        Just (ReplLimit xs) -> do
            case readMaybe xs of
                Nothing -> do
                    outputStrLn $ "Invalid limit: " ++ xs
                    outputStrLn $ "Current limit: " ++ show (limit state)
                    repl state
                Just limit' -> do
                    outputStrLn $ "Current limit: " ++ show limit'
                    repl state{limit = limit'}
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
                    let result' =
                            toResult (mode state) (Just $ limit state) result
                    mapM_ outputStrLn $ showResult result'
                    repl state{runtime = rt}
        Just (ReplArity code) -> do
            case compile code of
                Left e -> do
                    outputStrLn $ "Invalid code: " ++ show e
                    repl state
                Right function' -> do
                    outputStrLn $ code ++ " : " ++ show (getArity function')
                    repl state

replCommandCompletion :: String -> [Completion]
replCommandCompletion prefix =
    [simpleCompletion s | s <- replCommandStrings, prefix `isPrefixOf` s]

completionFromMap :: Map String a -> (a -> Char) -> String -> [Completion]
completionFromMap m f prefix = completing ++ completed
  where
    completing =
        [ completing' name' short'
        | name' <- Map.keys m
        , drop 1 prefix `isPrefixOf` name'
        , let short' = [f $ m ! name']
        ]
    completed =
        if not (null prefix) && last prefix == ' '
            then case Map.lookup (drop 1 $ init prefix) m of
                Nothing -> []
                Just b -> [completed' [f b]]
            else []
    completing' name' short' =
        let name'' = '\\' : name'
            info' = name'' ++ " ('" ++ short' ++ "')"
         in Completion name'' info' True
    completed' short' = Completion short' short' False

builtinCompletion :: String -> [Completion]
builtinCompletion prefix =
    completionFromMap builtinMap short prefix
        ++ completionFromMap Particle.builtinParticleMap Particle.short prefix

replCompletion :: (Monad m) => CompletionFunc m
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

replSettings :: (Monad m) => Settings m
replSettings = Settings replCompletion (Just ".history") True

runRepl :: IO ()
runRepl = do
    putStrLn $ "Nekomata " ++ showVersion version ++ " REPL - type \\H for help"
    _ <- runInputT replSettings . withInterrupt $ repl initReplState
    return ()
