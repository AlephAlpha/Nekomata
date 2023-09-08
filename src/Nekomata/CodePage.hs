module Nekomata.CodePage where

import Control.Monad (zipWithM)
import Data.List (elemIndex)
import Data.Word (Word8)

{- | Nekomata's custom code page:
256 characters, each representing a single byte.
-}
codePage :: String
codePage =
    "¡¢£¤¥¦§¬±×÷‼←→↔↕"
        ++ "¿∆∏∑∕∙√½∩∫≈≠≡µ≤≥"
        ++ " !\"#$%&'()*+,-./"
        ++ "0123456789:;<=>?"
        ++ "@ABCDEFGHIJKLMNO"
        ++ "PQRSTUVWXYZ[\\]^_"
        ++ "`abcdefghijklmno"
        ++ "pqrstuvwxyz{|}~\n"
        ++ "ᵃᶜᵈᵉᵋᶠᶦᵏˡᵐᵚᵑᵒᵖʳᵗ"
        ++ "ʷˣᶻᶾ������������"
        ++ "ÄƂÇĈĎÐƊËĜĢĨĬĻṀṂŇ"
        ++ "ÖØƆƤṖŘŞŢŤŬŽÞ����"
        ++ "äƃƀçĉđḍɗĕƒĝïĭįṁṃ"
        ++ "ňŋṇɔƥŗřşŧũůžþ���"
        ++ "����������������"
        ++ "����������������"

-- | Convert a character to a number.
charToInt :: (Num a) => Char -> Maybe a
charToInt = fmap fromIntegral . (`elemIndex` codePage)

-- | Convert an integer to a character.
intToChar :: (Integral a) => a -> Maybe Char
intToChar x
    | x >= 0 && x < 255 = Just $ codePage !! fromIntegral x
    | otherwise = Nothing

-- A Markdown table of the code page
codePageMarkdown :: String
codePageMarkdown =
    "| |"
        ++ concat ["**_" ++ d : "**|" | d <- hexDigits]
        ++ "\n|-|"
        ++ concat (replicate 16 "-|")
        ++ "\n"
        ++ unlines
            [ "|**"
                ++ d
                : "_**|"
                    ++ concat
                        [ '`' : escape c ++ "`|"
                        | j <- [0 .. 15]
                        , let c = codePage !! (i * 16 + j)
                        ]
            | (i, d) <- zip [0 ..] hexDigits
            ]
  where
    hexDigits = "0123456789ABCDEF"
    escape '\n' = "\\n"
    escape '|' = "\\|"
    escape '`' = "` ` `"
    escape c = [c]

-- | An error that occurs when a character is not in Nekomata's code page.
data CodePageError = CodePageError {char :: Char, pos :: Int} deriving (Eq)

instance Show CodePageError where
    show (CodePageError c i) =
        "Character '"
            ++ [c]
            ++ "' at position "
            ++ show i
            ++ " is not in Nekomata's code page."

-- | Convert a list of bytes to a string.
fromBytes :: [Word8] -> String
fromBytes = map $ (codePage !!) . fromIntegral

-- | Convert a string to a list of bytes.
toBytes :: String -> Either CodePageError [Word8]
toBytes = zipWithM toByte [0 ..]
  where
    toByte i x = maybe (Left $ CodePageError x i) Right $ charToInt x

-- | Check if a string is in Nekomata's code page.
checkCodePage :: String -> Either CodePageError String
checkCodePage = zipWithM checkCodePage' [0 ..]
  where
    checkCodePage' i x =
        if x `elem` codePage || x == '�'
            then Right x
            else Left $ CodePageError x i
