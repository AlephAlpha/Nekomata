module Repl (runRepl) where

import Nekomata.Eval
import System.Console.Haskeline

data ReplState = ReplState
    { mode :: Mode
    , runtime :: Runtime
    }

initReplState :: ReplState
initReplState = ReplState AllValues (initRuntime [])

helpString :: String
helpString =
    unlines
        [ "Nekomata REPL"
        , ""
        , "Commands:"
        , "  :q             Quit"
        , "  :quit          Quit"
        , "  :h             Show this help"
        , "  :help          Show this help"
        , "  :mode          Show current mode"
        , "  :mode all      Show all results"
        , "  :mode first    Show first result"
        , "  :mode count    Show number of results"
        , "  :mode exists   Show whether there are any results"
        , "  :input <data>  Reset the stack with the given input"
        , "  :info <name>   Show information about a builtin"
        ]

repl :: ReplState -> InputT IO ReplState
repl state = do
    input <- getInputLine ">>> "
    case input of
        Nothing -> return state
        Just ":q" -> return state
        Just ":quit" -> return state
        Just ":h" -> do
            outputStrLn helpString
            repl state
        Just ":help" -> do
            outputStrLn helpString
            repl state
        Just ":mode" -> do
            outputStrLn $ "Current mode: " ++ show (mode state)
            repl state
        Just ":mode all" -> repl state{mode = AllValues}
        Just ":mode first" -> repl state{mode = FirstValue}
        Just ":mode count" -> repl state{mode = CountValues}
        Just ":mode exists" -> repl state{mode = CheckExistence}
        Just (':' : 'm' : 'o' : 'd' : 'e' : ' ' : m) -> do
            outputStrLn $ "Unknown mode: " ++ m
            repl state
        Just (':' : 'i' : 'n' : 'p' : 'u' : 't' : ' ' : xs) -> do
            case readInput xs of
                Left e -> do
                    outputStrLn $ "Invalid input: " ++ show e
                    repl state
                Right input' -> repl state{runtime = initRuntime input'}
        Just (':' : 'i' : 'n' : 'f' : 'o' : ' ' : xs) -> do
            case builtinInfo xs of
                Nothing -> do
                    outputStrLn $ "Unknown builtin: " ++ xs
                    repl state
                Just info -> do
                    outputStrLn info
                    repl state
        Just code -> do
            case compile code of
                Left e -> do
                    outputStrLn $ "Invalid code: " ++ show e
                    repl state
                Right function' -> do
                    let (rt, result) = runFunction function' (runtime state)
                    outputStrLn $ showResult (mode state) result
                    repl state{runtime = rt}

runRepl :: IO ()
runRepl = do
    putStrLn "Nekomata REPL - type :h for help"
    _ <- runInputT defaultSettings (repl initReplState)
    return ()
