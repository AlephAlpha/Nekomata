module Nekomata.CodePage where

import Control.Monad (zipWithM)
import Data.List (elemIndex)
import Data.Word (Word8)

{- | Nekomata's custom code page:
256 characters, each representing a single byte.
-}
codePage :: String
codePage =
    "�÷×∣≠∀∃←→\n↔⇄≤≥⊥⊤"
        ++ "¬±ℕℤ∑∏∙∅��������"
        ++ " !\"#$%&'()*+,-./"
        ++ "0123456789:;<=>?"
        ++ "@ABCDEFGHIJKLMNO"
        ++ "PQRSTUVWXYZ[\\]^_"
        ++ "`abcdefghijklmno"
        ++ "pqrstuvwxyz{|}~�"
        ++ "�ᵃᵇᶜᵈᵉᶠᵍʰⁱʲᵏˡᵐⁿᵒ"
        ++ "ᵖ𐞥ʳˢᵗᵘᵛʷˣʸᶻ�����"
        ++ "����������������"
        ++ "����������������"
        ++ "����������������"
        ++ "����������������"
        ++ "����������������"
        ++ "����������������"

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
    toByte i x =
        maybe
            (Left $ CodePageError x i)
            (Right . fromIntegral)
            $ elemIndex x codePage

-- | Check if a string is in Nekomata's code page.
checkCodePage :: String -> Either CodePageError String
checkCodePage = zipWithM checkCodePage' [0 ..]
  where
    checkCodePage' i x =
        if x `elem` codePage || x == '�'
            then Right x
            else Left $ CodePageError x i
