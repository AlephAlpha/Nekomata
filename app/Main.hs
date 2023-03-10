module Main (main) where

import qualified Data.ByteString as ByteString
import Doc
import Nekomata.CodePage
import Nekomata.Eval
import Options.Applicative
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

data RunOnce = RunOnce {code :: Code, input :: Input, mode :: Mode}

optRunOnce :: Parser RunOnce
optRunOnce = RunOnce <$> optCode <*> optInput <*> optMode

data Opts = Opts RunOnce | Repl | DocBuiltin | DocCodePage

opts :: Parser Opts
opts =
  Opts <$> optRunOnce
    <|> flag' Repl (long "repl" <> short 'r' <> help "Run the REPL")
    <|> flag'
      DocBuiltin
      (long "doc" <> help "Generate documentation for builtins")
    <|> flag'
      DocCodePage
      (long "codepage" <> help "Generate documentation for code page")

optsInfo :: ParserInfo Opts
optsInfo =
  info
    (opts <**> helper)
    ( fullDesc
        <> progDesc "Run a Nekomata program"
        <> header "Nekomata - a non-deterministic golfing language"
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
        InputNone -> return ""
      case eval (mode runOnce) code' input' of
        Left err -> die $ "Error: " ++ show err
        Right result -> putStrLn result
    Repl -> runRepl
    DocBuiltin -> putStrLn docBuiltins
    DocCodePage -> putStrLn docCodePage
