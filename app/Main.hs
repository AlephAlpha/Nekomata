module Main (main) where

import Control.Monad (forM_)
import qualified Data.ByteString as ByteString
import Data.Version (showVersion)
import Doc
import Nekomata.CodePage
import Nekomata.Eval
import Options.Applicative
import Paths_Nekomata (version)
import Repl
import System.Exit (die)

data Code = CodeArg String | CodeFile FilePath | CodeFileUtf8 FilePath

data Input = InputArg String | InputStdin | InputNone

optCode :: Parser Code
optCode =
    CodeArg
        <$> strOption
            ( long "code"
                <> short 'c'
                <> metavar "CODE"
                <> help "Code to run"
            )
            <|> CodeFile
        <$> strOption
            ( long "file"
                <> short 'f'
                <> metavar "FILE"
                <> help "File to run (custom encoding)"
            )
            <|> CodeFileUtf8
        <$> strOption
            ( long "utf8"
                <> short 'u'
                <> metavar "FILE"
                <> help "File to run (UTF-8)"
            )

optInput :: Parser Input
optInput =
    InputArg
        <$> strOption
            ( long "input"
                <> short 'i'
                <> metavar "INPUT"
                <> help "Input to pass to the program"
            )
            <|> flag'
                InputStdin
                ( long "stdin"
                    <> short 's'
                    <> help "Read input from stdin"
                )
            <|> pure InputNone

optMode :: Parser Mode
optMode =
    flag'
        FirstValue
        ( long "first"
            <> short '1'
            <> help "Show only the first value"
        )
        <|> flag'
            LastValue
            ( long "last"
                <> short 't'
                <> help "Show only the last value"
            )
        <|> flag'
            CountValues
            ( long "count"
                <> short 'n'
                <> help "Show the number of values"
            )
        <|> flag'
            CheckExistence
            ( long "exists"
                <> short 'e'
                <> help "Show whether the result has any value"
            )
        <|> pure AllValues

optMultiple :: Parser Bool
optMultiple =
    flag'
        True
        ( long "multiple"
            <> short 'm'
            <> help "Take multiple inputs separated by newlines"
        )
        <|> pure False

optLimit :: Parser (Maybe Int)
optLimit =
    optional
        ( option
            auto
            ( long "limit"
                <> short 'l'
                <> metavar "N"
                <> help "The maximum number of results to show"
            )
        )

data RunOnce = RunOnce
    { code :: Code
    , input :: Input
    , mode :: Mode
    , multiple :: Bool
    , limit :: Maybe Int
    }

optRunOnce :: Parser RunOnce
optRunOnce =
    RunOnce
        <$> optCode
        <*> optInput
        <*> optMode
        <*> optMultiple
        <*> optLimit

data Opts = Opts RunOnce | Repl | DocBuiltin | DocCodePage | Version

opts :: Parser Opts
opts =
    Opts
        <$> optRunOnce
            <|> flag' Repl (long "repl" <> short 'r' <> help "Run the REPL")
            <|> flag'
                DocBuiltin
                (long "doc" <> help "Generate documentation for builtins")
            <|> flag'
                DocCodePage
                (long "codepage" <> help "Generate documentation for code page")
            <|> flag'
                Version
                (long "version" <> short 'v' <> help "Show version")
            <|> pure Repl

optsInfo :: ParserInfo Opts
optsInfo =
    info
        (opts <**> helper)
        ( fullDesc
            <> progDesc
                ( "Nekomata is an experimental non-deterministic "
                    ++ "concatenative golfing language."
                )
            <> header
                ( "Nekomata "
                    ++ showVersion version
                    ++ " - a non-deterministic golfing language"
                )
        )

main :: IO ()
main = do
    opts' <- execParser optsInfo
    case opts' of
        Opts runOnce -> do
            code' <- case code runOnce of
                CodeArg code' -> return code'
                CodeFile file ->
                    fromBytes . ByteString.unpack <$> ByteString.readFile file
                CodeFileUtf8 file -> readFile file
            input' <- case input runOnce of
                InputArg input' -> return input'
                InputStdin -> getContents
                InputNone -> return $ if multiple runOnce then "\n" else ""
            fun <- case compile code' of
                Left err -> die $ "Invalid code: " ++ show err
                Right fun' -> return fun'
            if multiple runOnce
                then forM_ (lines input') $ \input'' ->
                    case eval (mode runOnce) (limit runOnce) fun input'' of
                        Left err -> die $ "Invalid input: " ++ show err
                        Right result ->
                            putStrLn $ input'' ++ " -> " ++ show result
                else case eval (mode runOnce) (limit runOnce) fun input' of
                    Left err -> die $ "Invalid input: " ++ show err
                    Right result -> mapM_ putStrLn $ showResult result
        Repl -> runRepl
        DocBuiltin -> putStrLn docBuiltins
        DocCodePage -> putStrLn docCodePage
        Version -> putStrLn $ "Nekomata " ++ showVersion version
