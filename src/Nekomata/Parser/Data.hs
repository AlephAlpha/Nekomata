module Nekomata.Parser.Data where

import Data.Ratio ((%))
import Nekomata.Data
import Text.Parsec
import Text.Parsec.String (Parser)

-- | Parse a positive integer
parsePositive :: Parser Integer
parsePositive = read <$> many1 digit <?> "positive integer"

-- | Parse an integer
parseInt :: Parser Integer
parseInt =
    do
        sign <- optionMaybe $ char '-'
        case sign of
            Just _ -> negate <$> parsePositive
            Nothing -> parsePositive
        <?> "integer"

-- | Parse a rational number
parseRational :: Parser Rational
parseRational =
    do
        num <- parseInt
        spaces
        _ <- oneOf "/\\"
        spaces
        den <- parsePositive
        return $ num % den
        <?> "rational number"

-- | Parse a rational number, but use '\' as a separator
parseRational' :: Parser Rational
parseRational' =
    do
        num <- parseInt
        spaces
        _ <- char '\\'
        spaces
        den <- parsePositive
        return $ num % den
        <?> "rational number"

-- | Parse a number literal
parseNum :: Parser Rational
parseNum = try parseRational <|> fromInteger <$> parseInt <?> "number"

{- | Parse a positive number literal,
where '\' is used as a separator for rational numbers
-}
parseNum' :: Parser Rational
parseNum' = try parseRational' <|> fromInteger <$> parsePositive <?> "number"

-- | Parse an escape character
parseEscape :: Parser Char
parseEscape = char '\\' >> oneOf "\"\\'"

-- | Parse a string literal
parseString :: Parser String
parseString =
    between
        (char '"')
        (char '"')
        (many $ try parseEscape <|> noneOf "\"")
        <?> "string"

-- | Parse a char literal
parseChar :: Parser Char
parseChar = between (char '\'') (optional $ char '\'') anyChar <?> "char"

-- | Parse a char literal, but without the right single quote
parseChar' :: Parser Char
parseChar' = char '\'' >> anyChar <?> "char"

-- | Parse a list of Nekomata data
parseList :: Parser [Data]
parseList =
    between
        (char '[' >> spaces)
        (spaces >> char ']')
        (parseData `sepBy` (spaces >> char ',' >> spaces))
        <?> "list"

-- | Parse a Nekomata data
parseData :: Parser Data
parseData =
    choice
        [ DNum <$> parseNum
        , DChar <$> parseChar
        , DList . map DChar <$> parseString
        , DList <$> parseList
        ]
        <?> "Nekomata data"

-- | Parse a Nekomata data literal in a program
parseData' :: Parser Data
parseData' =
    choice
        [ DNum <$> parseNum'
        , DChar <$> parseChar'
        , DList . map DChar <$> parseString
        , DList <$> parseList
        ]
        <?> "Nekomata data"

-- | Parse a Nekomata input
parseInput :: Parser [Data]
parseInput =
    parseData
        `endBy` (try (spaces >> char ',' >> spaces) <|> spaces)
        <* spaces
        <* eof
        <?> "Nekomata input"
