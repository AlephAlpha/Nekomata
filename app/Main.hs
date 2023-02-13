module Main where

import Nekomata.Eval
import Options.Applicative
import System.Exit (die)

data Code = CodeArg String | CodeFile FilePath

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
            <> help "File to run"
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

data Opts = Opts {code :: Code, input :: Input, mode :: Mode}

opts :: Parser Opts
opts = Opts <$> optCode <*> optInput <*> optMode

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
  code' <- case code opts' of
    CodeArg code' -> return code'
    CodeFile file -> readFile file
  input' <- case input opts' of
    InputArg input' -> return input'
    InputStdin -> getContents
    InputNone -> return ""
  case eval (mode opts') input' code' of
    Left err -> die $ "Error: " ++ show err
    Right result -> putStrLn result
