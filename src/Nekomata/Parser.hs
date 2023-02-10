module Nekomata.Parser where

import Nekomata.Data (Data (..))
import Text.Parsec

parseInt :: Parsec String () Integer
parseInt = read <$> many1 digit <?> "Integer"

parseEscape :: Parsec String () Char
parseEscape =
    char '\\' >> (\c -> read ("'\\" ++ [c] ++ "'")) <$> oneOf "'\"\\nrtbfv0"

parseString :: Parsec String () String
parseString =
    between
        (char '"')
        (char '"')
        (many $ try parseEscape <|> noneOf "\"")
        <?> "String"

parseList :: Parsec String () [Data]
parseList =
    between
        (char '[')
        (char ']')
        (parseData `sepBy` char ',')
        <?> "List"

parseData :: Parsec String () Data
parseData =
    choice
        [ DInt <$> parseInt
        , DString <$> parseString
        , DList <$> parseList
        ]
        <* spaces
        <?> "Data"
