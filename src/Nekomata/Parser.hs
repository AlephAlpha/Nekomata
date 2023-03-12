module Nekomata.Parser (
    parseData,
    parseBuiltin,
    parseInput,
    parseParticle,
    parseProgram,
) where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Nekomata.Builtin
import Nekomata.Data
import Nekomata.Particle
import Nekomata.Program
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
parseEscape = char '\\' >> oneOf "\"\\"

-- | Parse a string literal
parseString :: Parser String
parseString =
    between
        (char '"')
        (char '"')
        (many $ try parseEscape <|> noneOf "\"")
        <?> "string"

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
        , DString <$> parseString
        , DList <$> parseList
        ]
        <?> "Nekomata data"

-- | Parse a Nekomata data
parseData' :: Parser Data
parseData' =
    choice
        [ DNum <$> parseNum'
        , DString <$> parseString
        , DList <$> parseList
        ]
        <?> "Nekomata data"

-- | Parse a full name of a builtin function
parseBuiltinName :: Parser Builtin
parseBuiltinName =
    do
        name' <- char '\\' >> many1 alphaNum
        case Map.lookup name' builtinMap of
            Just b -> return b
            Nothing -> parserFail . show $ BuiltinNotFound name'
        <?> "builtin function full name"

-- | Parse a short name of a builtin function
parseBuiltinShort :: Parser Builtin
parseBuiltinShort =
    do
        short' <- oneOf $ Map.keys builtinShortMap
        case Map.lookup short' builtinShortMap of
            Just b -> return b
            Nothing -> parserFail . show $ BuiltinShortNotFound short'
        <?> "builtin function short name"

-- | Parse a builtin function
parseBuiltin :: Parser Builtin
parseBuiltin = parseBuiltinName <|> parseBuiltinShort <?> "builtin function"

-- | Parse a full name of a particle
parseParticleName :: Parser BuiltinParticle
parseParticleName =
    do
        name' <- char '\\' >> many1 alphaNum
        case Map.lookup name' builtinParticleMap of
            Just p -> return p
            Nothing -> parserFail . show $ ParticleNotFound name'
        <?> "particle full name"

-- | Parse a short name of a particle
parseParticleShort :: Parser BuiltinParticle
parseParticleShort =
    do
        short' <- oneOf $ Map.keys builtinParticleShortMap
        case Map.lookup short' builtinParticleShortMap of
            Just p -> return p
            Nothing -> parserFail . show $ ParticleShortNotFound short'
        <?> "particle short name"

-- | Parse a particle
parseParticle :: Parser BuiltinParticle
parseParticle = parseParticleName <|> parseParticleShort <?> "particle"

-- | Parse a term in a Nekomata program
parseTerm :: Parser Term
parseTerm =
    choice
        [ try $ TFunc <$> parseBuiltin
        , try $ TPart <$> parseParticle <* spaces <*> parseTerm
        , TLit <$> parseData'
        , TBlock
            <$> between
                (char '{' >> spaces)
                (void (char '}') <|> eof)
                parseBlock
        ]
        <?> "Nekomata term"

-- | Parse a Nekomata program
parseBlock :: Parser Program
parseBlock = Program <$> parseTerm `endBy` spaces <?> "Nekomata block"

-- | Parse a Nekomata program
parseProgram :: Parser Program
parseProgram = spaces >> parseBlock <* spaces <* eof <?> "Nekomata program"

parseInput :: Parser [Data]
parseInput =
    parseData
        `endBy` (try (spaces >> char ',' >> spaces) <|> try spaces)
        <* eof
        <?> "Nekomata input"
