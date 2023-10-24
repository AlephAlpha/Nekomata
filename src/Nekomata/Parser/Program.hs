module Nekomata.Parser.Program where

import Control.Monad (void)
import qualified Data.Map.Strict as Map
import Nekomata.Builtin
import Nekomata.Parser.Data
import Nekomata.Particle
import Nekomata.Program
import Text.Parsec
import Text.Parsec.String (Parser)

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

-- | Parse a Nekomata code block
parseBlock :: Parser Program
parseBlock = Program <$> parseTerm `endBy` spaces <?> "Nekomata block"

-- | Parse a Nekomata program
parseProgram :: Parser Program
parseProgram = spaces >> parseBlock <* spaces <* eof <?> "Nekomata program"
