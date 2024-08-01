module Nekomata.Builtin (
    Builtin (..),
    BuiltinNotFoundError (..),
    builtins,
    builtinMap,
    builtinShortMap,
    info,
    infoMarkdown,
    infoByName,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Nekomata.Builtin.Basic
import Nekomata.Builtin.List
import Nekomata.Builtin.Math
import Nekomata.Builtin.String
import Nekomata.Function
import Nekomata.Result

-- | A builtin function in Nekomata
data Builtin = Builtin
    { name :: String
    -- ^ The name of the builtin function
    , short :: Char
    -- ^ The short name of the builtin function
    , func :: Function
    -- ^ The function itself
    , help :: String
    -- ^ The help message for the builtin function
    , examples :: [(String, Result)]
    -- ^ Some examples for the builtin function
    }

instance Show Builtin where
    show b = "\\" ++ name b

-- | Get the info string for a builtin function
info :: Builtin -> String
info b =
    show b
        ++ " ('"
        ++ [short b]
        ++ "', "
        ++ show (arity (func b))
        ++ "):\n"
        ++ help b
        ++ if null (examples b)
            then ""
            else
                "\nExamples:\n"
                    ++ unlines
                        [ "  "
                            ++ example
                            ++ " -> "
                            ++ if result == all_ []
                                then "Fail"
                                else show result
                        | (example, result) <- examples b
                        ]

-- | Get the info string for a builtin function in Markdown format
infoMarkdown :: Builtin -> String
infoMarkdown b =
    "### `"
        ++ name b
        ++ "` (`"
        ++ [short b]
        ++ "`, `"
        ++ show (arity (func b))
        ++ "`)\n\n"
        ++ concatMap (++ "\n\n") (lines (help b))
        ++ if null (examples b)
            then ""
            else
                "__Examples__:\n\n"
                    ++ unlines
                        [ "- `"
                            ++ example
                            ++ "` → "
                            ++ if result == all_ []
                                then "Fail"
                                else "`" ++ show result ++ "`"
                        | (example, result) <- examples b
                        ]
                    ++ "\n"

-- | Get the info string for a builtin function by name
infoByName :: String -> Maybe String
infoByName name' =
    info <$> case name' of
        [short'] -> Map.lookup short' builtinShortMap
        '\\' : name'' -> Map.lookup name'' builtinMap
        _ -> Map.lookup name' builtinMap

all_ :: [String] -> Result
all_ = All False

truncate_ :: [String] -> Result
truncate_ = All True

-- | The list of all builtin functions
builtins :: [Builtin]
builtins =
    [ Builtin
        "choice"
        '?'
        choice
        "Choose between two values.\n\
        \This function is non-deterministic."
        [("1 2?", all_ ["1", "2"])]
    , Builtin
        "fail"
        '!'
        fail'
        "Push a non-deterministic object with no values."
        [("!", all_ [])]
    , Builtin
        "allValues"
        'a'
        allValues
        "Get a list of all possible values for a non-deterministic object."
        [("1 2?a", all_ ["[1,2]"])]
    , Builtin
        "oneValue"
        '¡'
        oneValue
        "Get the first possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
        [("1 2?¡", all_ ["1"])]
    , Builtin
        "countValues"
        'n'
        countValues'
        "Count the number of values in a non-deterministic object."
        [("1 2?n", all_ ["2"])]
    , Builtin
        "uniqueValue"
        'ũ'
        uniqueValue
        "Remove duplicate values from a non-deterministic object."
        [("[1,1,2]~ũ", all_ ["1", "2"])]
    , Builtin
        "minValue"
        'å'
        minValue
        "Get the minimum possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
        [("1 2?å", all_ ["1"])]
    , Builtin
        "maxValue"
        'Å'
        maxValue
        "Get the maximum possible value from a non-deterministic object.\n\
        \Fails if the object has no values."
        [("1 2?Å", all_ ["2"])]
    , Builtin
        "if"
        'I'
        if'
        "Choose the first value that doesn't fail between two values."
        [ ("1 2I", all_ ["1"])
        , ("! 2I", all_ ["2"])
        ]
    , Builtin
        "andThen"
        '¿'
        andThen
        "Take two values, and return the first one if the second one doesn't \
        \fail. \n\
        \This is somewhat similar to the `seq` function in Haskell, \
        \which forces the first argument to be evaluated before the second."
        [ ("1 2¿", all_ ["1"])
        , ("1 !¿", all_ [])
        ]
    , Builtin
        "drop"
        '^'
        drop'
        "Drop the top value of the stack: `a ... -> ...`."
        [("1 2^", all_ ["1"])]
    , Builtin
        "dup"
        ':'
        dup
        "Duplicate the top value of the stack: `a ... -> a a ...`."
        [("1:Ð", all_ ["[1,1]"])]
    , Builtin
        "swap"
        '$'
        swap
        "Swap the top two values of the stack: `a b ... -> b a ...`."
        [("1 2$Ð", all_ ["[2,1]"])]
    , Builtin
        "rot3"
        '§'
        rot3
        "Rotate the top three values of the stack: `a b c ... -> c a b ...`."
        [("1 2 3§ÐÐ", all_ ["[2,[3,1]]"])]
    , Builtin
        "over"
        'v'
        over
        "Duplicate the second value of the stack, \
        \and put it on top of the stack: `a b ... -> b a b ...`."
        [("1 2vÐÐ", all_ ["[1,[2,1]]"])]
    , Builtin
        "eq"
        '='
        eq
        "Check if two values are equal.\n\
        \If they are, push the first value, otherwise fail."
        [ ("1 1=", all_ ["1"])
        , ("1 2=", all_ [])
        ]
    , Builtin
        "ne"
        '≠'
        ne
        "Check if two values are not equal.\n\
        \If they are not, push the first value, otherwise fail."
        [ ("1 1≠", all_ [])
        , ("1 2≠", all_ ["1"])
        ]
    , Builtin
        "lt"
        'Ļ'
        lt
        "Check if the first value is less than the second.\n\
        \If it is, push the first value, otherwise fail.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("1 2Ļ", all_ ["1"])
        , ("1 1Ļ", all_ [])
        , ("2 1Ļ", all_ [])
        , ("1 'aĻ", all_ ["1"])
        , ("'a [1]Ļ", all_ ["'a'"])
        , ("[1,2] [2]Ļ", all_ ["[1,2]"])
        , ("[1,2] [1]Ļ", all_ [])
        ]
    , Builtin
        "gt"
        'Ģ'
        gt
        "Check if the first value is greater than the second.\n\
        \If it is, push the first value, otherwise fail.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("1 2Ģ", all_ [])
        , ("1 1Ģ", all_ [])
        , ("2 1Ģ", all_ ["2"])
        , ("'a 1Ģ", all_ ["'a'"])
        , ("[1] 'aĢ", all_ ["[1]"])
        , ("[1,2] [2]Ģ", all_ [])
        , ("[1,2] [1]Ģ", all_ ["[1,2]"])
        ]
    , Builtin
        "isNonempty"
        'N'
        isNonempty
        "Check if a list is non-empty.\n\
        \If it is, push the list itself, otherwise fail."
        [ ("[1]N", all_ ["[1]"])
        , ("\"Hello\"N", all_ ["Hello"])
        , ("[]N", all_ [])
        ]
    , Builtin
        "isLong"
        'Ł'
        isLong
        "Check if the length of a list is greater than 1.\n\
        \If it is, push the list itself, otherwise fail."
        [ ("[1,2]Ł", all_ ["[1,2]"])
        , ("[1]Ł", all_ [])
        , ("[]Ł", all_ [])
        ]
    , Builtin
        "isNonzero"
        'Z'
        isNonzero
        "Check if a number is non-zero.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1Z", all_ ["1"])
        , ("0Z", all_ [])
        , ("1_Z", all_ ["-1"])
        , ("[1,[2,3]]Z", all_ ["[1,[2,3]]"])
        ]
    , Builtin
        "isPositive"
        'P'
        isPositive
        "Check if a number is positive.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1P", all_ ["1"])
        , ("0P", all_ [])
        , ("1_P", all_ [])
        , ("[1,[2,3]]P", all_ ["[1,[2,3]]"])
        ]
    , Builtin
        "isNonnegative"
        'ň'
        isNonnegative
        "Check if a number is non-negative.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1ň", all_ ["1"])
        , ("0ň", all_ ["0"])
        , ("1_ň", all_ [])
        , ("[1,[2,3]]ň", all_ ["[1,[2,3]]"])
        ]
    , Builtin
        "isZero"
        'ž'
        isZero
        "Check if a number is zero.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1ž", all_ [])
        , ("0ž", all_ ["0"])
        , ("1_ž", all_ [])
        , ("[0,[0,0]]ž", all_ ["[0,[0,0]]"])
        ]
    , Builtin
        "isBig"
        'Ƶ'
        isBig
        "Check if the absolute value of a number is greater than 1.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2Ƶ", all_ ["2"])
        , ("3\\2Ƶ", all_ ["3/2"])
        , ("1Ƶ", all_ [])
        , ("1\\2Ƶ", all_ [])
        , ("0Ƶ", all_ [])
        , ("1_Ƶ", all_ [])
        , ("2_Ƶ", all_ ["-2"])
        ]
    , Builtin
        "isSmall"
        'ƶ'
        isSmall
        "Check if the absolute value of a number is \n\
        \less than or equal to than 1.\n\
        \If it is, push the number itself, otherwise fail.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2ƶ", all_ [])
        , ("3\\2ƶ", all_ [])
        , ("1ƶ", all_ ["1"])
        , ("1\\2ƶ", all_ ["1/2"])
        , ("0ƶ", all_ ["0"])
        , ("1_ƶ", all_ ["-1"])
        , ("2_ƶ", all_ [])
        ]
    , Builtin
        "less"
        '<'
        less
        "Check if the first number is less than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
        [ ("1 2<", all_ ["1"])
        , ("1 1<", all_ [])
        , ("2 1<", all_ [])
        , ("[1,2,3] [2,3,4]<", all_ ["[1,2,3]"])
        , ("[1,2] [2,1]<", all_ [])
        ]
    , Builtin
        "lessEq"
        '≤'
        lessEq
        "Check if the first number is less than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
        [ ("1 2≤", all_ ["1"])
        , ("1 1≤", all_ ["1"])
        , ("2 1≤", all_ [])
        , ("[1,2,3] [2,3,4]≤", all_ ["[1,2,3]"])
        , ("[1,2] [2,1]≤", all_ [])
        ]
    , Builtin
        "greater"
        '>'
        greater
        "Check if the first number is greater than the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
        [ ("1 2>", all_ [])
        , ("1 1>", all_ [])
        , ("2 1>", all_ ["2"])
        , ("[2,3,4] [1,2,3]>", all_ ["[2,3,4]"])
        , ("[2,1] [1,2]>", all_ [])
        ]
    , Builtin
        "greaterEq"
        '≥'
        greaterEq
        "Check if the first number is greater than or equal to the second.\n\
        \If it is, push the first number, otherwise fail.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page \
        \before comparison, but the result is still a char.\n\
        \This function is automatically vectorized."
        [ ("1 2≥", all_ [])
        , ("1 1≥", all_ ["1"])
        , ("2 1≥", all_ ["2"])
        , ("[2,3,4] [1,2,3]≥", all_ ["[2,3,4]"])
        , ("[2,1] [1,2]≥", all_ [])
        ]
    , Builtin
        "neg1"
        '£'
        neg1
        "The constant -1."
        [("£", all_ ["-1"])]
    , Builtin
        "ten"
        '¢'
        ten
        "The constant 10."
        [("¢", all_ ["10"])]
    , Builtin
        "octet"
        '¥'
        octet
        "The constant 256."
        [("¥", all_ ["256"])]
    , Builtin
        "neg"
        '_'
        neg
        "Negate a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1_", all_ ["-1"])
        , ("[1,[2,3]]_", all_ ["[-1,[-2,-3]]"])
        ]
    , Builtin
        "abs"
        'A'
        abs'
        "Absolute value of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1A", all_ ["1"])
        , ("1_A", all_ ["1"])
        , ("[-1,[2,-3]]A", all_ ["[1,[2,3]]"])
        ]
    , Builtin
        "increment"
        '→'
        increment
        "Increment a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1→", all_ ["2"])
        , ("[1,[2,3]]→", all_ ["[2,[3,4]]"])
        ]
    , Builtin
        "decrement"
        '←'
        decrement
        "Decrement a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1←", all_ ["0"])
        , ("[1,[2,3]]←", all_ ["[0,[1,2]]"])
        ]
    , Builtin
        "logicalNot"
        '¬'
        logicalNot
        "Takes a number and returns 1 if it is 0, and 0 otherwise.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1¬", all_ ["0"])
        , ("2¬", all_ ["0"])
        , ("0¬", all_ ["1"])
        , ("[-1,[0,1]]¬", all_ ["[0,[1,0]]"])
        ]
    , Builtin
        "sign"
        '±'
        sign
        "Returns -1 if the argument is negative, 0 if it is zero, \
        \and 1 if it is positive.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2±", all_ ["1"])
        , ("0±", all_ ["0"])
        , ("[-2,[0,2]]±", all_ ["[-1,[0,1]]"])
        ]
    , Builtin
        "add"
        '+'
        add
        "Add two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
        [ ("1 1+", all_ ["2"])
        , ("[1,2] [2,3]+", all_ ["[3,5]"])
        , ("1 [2,3]+", all_ ["[3,4]"])
        , ("[1] [2,3]+", all_ ["[3,3]"])
        , ("[[1],[0,1]] [[0,2],[2]]+", all_ ["[[1,2],[2,1]]"])
        ]
    , Builtin
        "sub"
        '-'
        sub
        "Subtract two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
        [ ("1 1-", all_ ["0"])
        , ("[1,2] [2,3]-", all_ ["[-1,-1]"])
        , ("1 [2,3]-", all_ ["[-1,-2]"])
        , ("[1] [2,3]-", all_ ["[-1,-3]"])
        , ("[[1],[0,1]] [[0,2],[2]]-", all_ ["[[1,-2],[-2,1]]"])
        ]
    , Builtin
        "absDiff"
        '≈'
        absDiff
        "Absolute difference of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding zeros."
        [ ("1 2≈", all_ ["1"])
        , ("[1,2] [3,1]≈", all_ ["[2,1]"])
        , ("2 [1,3]≈", all_ ["[1,1]"])
        , ("[1] [3,1]≈", all_ ["[2,1]"])
        ]
    , Builtin
        "mul"
        '*'
        mul
        "Multiply two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("2 3*", all_ ["6"])
        , ("[2,3] [3,4]*", all_ ["[6,12]"])
        , ("2 [3,4]*", all_ ["[6,8]"])
        , ("[2] [3,4]*", all_ [])
        ]
    , Builtin
        "div"
        '/'
        div'
        "Division of two numbers.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("6 3/", all_ ["2"])
        , ("3 6/", all_ ["1/2"])
        , ("[3,6] [2,3]/", all_ ["[3/2,2]"])
        , ("3 [2,3]/", all_ ["[3/2,1]"])
        , ("[3] [2,3]/", all_ [])
        ]
    , Builtin
        "divInt"
        '÷'
        divInt
        "Integer division of two numbers. \
        \Result is rounded towards negative infinity.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("6 3÷", all_ ["2"])
        , ("3 6÷", all_ ["0"])
        , ("3_ 6÷", all_ ["-1"])
        , ("[3,6] [-2,3]÷", all_ ["[-2,2]"])
        , ("3 [-2,3]÷", all_ ["[-2,1]"])
        , ("[3] [-2,3]÷", all_ [])
        ]
    , Builtin
        "mod"
        '%'
        mod'
        "Modulo two numbers.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("5 3%", all_ ["2"])
        , ("5_ 3%", all_ ["1"])
        , ("5 3_%", all_ ["-1"])
        , ("5_ 3_%", all_ ["-2"])
        , ("[5,6] [3,4]%", all_ ["[2,2]"])
        , ("5 [3,4]%", all_ ["[2,1]"])
        , ("[5] [3,4]%", all_ [])
        ]
    , Builtin
        "divExact"
        '¦'
        divExact
        "Divide two numbers.\n\
        \Fails when the divisor is zero or \
        \the result is not an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("6 3¦", all_ ["2"])
        , ("5 3¦", all_ [])
        , ("[6,4] [3,4]¦", all_ ["[2,1]"])
        , ("6 [2,3]¦", all_ ["[3,2]"])
        , ("[6] [2,3]¦", all_ [])
        ]
    , Builtin
        "divMod"
        'þ'
        divMod'
        "Divide two numbers and return both the quotient and the remainder.\n\
        \Fails when the divisor is zero.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [("5 3þÐ", all_ ["[1,2]"])]
    , Builtin
        "half"
        '½'
        half
        "Check if an integer is even, and divide it by 2.\n\
        \Fails when the integer is odd.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("6½", all_ ["3"])
        , ("5½", all_ [])
        , ("[6,4]½", all_ ["[3,2]"])
        ]
    , Builtin
        "pow"
        'E'
        pow
        "Raise a number to a power.\n\
        \The first argument is the exponent, \
        \the second argument is the base.\n\
        \Fails when the exponent is not an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("2 3E", all_ ["9"])
        , ("3 2E", all_ ["8"])
        , ("2 1\\2E", all_ ["1/4"])
        , ("1\\2 2E", all_ [])
        , ("[-2,0,2] 2E", all_ ["[1/4,1,4]"])
        , ("[2] [3,4]E", all_ [])
        ]
    , Builtin
        "recip"
        'ŗ'
        recip'
        "Reciprocal of a number.\n\
        \Fails when the number is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2ŗ", all_ ["1/2"])
        , ("0ŗ", all_ [])
        , ("[2,3]ŗ", all_ ["[1/2,1/3]"])
        ]
    , Builtin
        "mul2"
        'Ä'
        mul2
        "Multiply a number by 2.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2Ä", all_ ["4"])
        , ("[2,3]Ä", all_ ["[4,6]"])
        ]
    , Builtin
        "div2"
        'ä'
        div2
        "Divide a number by 2.\n\
        \This is different from `half` in that \
        \it may return a non-integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("2ä", all_ ["1"])
        , ("[2,3]ä", all_ ["[1,3/2]"])
        ]
    , Builtin
        "mod2"
        'Ö'
        mod2
        "Modulo a number by 2.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("5Ö", all_ ["1"])
        , ("[5,6]Ö", all_ ["[1,0]"])
        ]
    , Builtin
        "powOf2"
        'Ë'
        powOf2
        "Raise 2 to a power.\n\
        \Fails when the exponent is not an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("3Ë", all_ ["8"])
        , ("[-2,0,2]Ë", all_ ["[1/4,1,4]"])
        ]
    , Builtin
        "denominator"
        'ḍ'
        denominator'
        "Get the denominator of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1\\2ḍ", all_ ["2"])
        , ("2ḍ", all_ ["1"])
        , ("[2/3,3/5]ḍ", all_ ["[3,5]"])
        ]
    , Builtin
        "numerator"
        'ṇ'
        numerator'
        "Get the numerator of a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1\\2ṇ", all_ ["1"])
        , ("2ṇ", all_ ["2"])
        , ("[2/3,3/5]ṇ", all_ ["[2,3]"])
        ]
    , Builtin
        "min"
        'm'
        min'
        "Get the minimum of two numbers or two chars.\n\
        \This function is automatically vectorized with padding."
        [ ("1 2m", all_ ["1"])
        , ("[1,2] [2,1]m", all_ ["[1,1]"])
        , ("2 [1,3]m", all_ ["[1,2]"])
        , ("[2] [1,3]m", all_ ["[1,3]"])
        ]
    , Builtin
        "max"
        'M'
        max'
        "Get the maximum of two numbers or two chars.\n\
        \This function is automatically vectorized with padding."
        [ ("1 2M", all_ ["2"])
        , ("[1,2] [2,1]M", all_ ["[2,2]"])
        , ("2 [1,3]M", all_ ["[2,3]"])
        , ("[2] [1,3]M", all_ ["[2,3]"])
        ]
    , Builtin
        "ceil"
        'K'
        ceil
        "Round a number up to the nearest integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1K", all_ ["1"])
        , ("1\\2K", all_ ["1"])
        , ("[5/2,-3/2]K", all_ ["[3,-1]"])
        ]
    , Builtin
        "floor"
        'k'
        floor'
        "Round a number down to the nearest integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1k", all_ ["1"])
        , ("1\\2k", all_ ["0"])
        , ("[5/2,-3/2]k", all_ ["[2,-2]"])
        ]
    , Builtin
        "range0"
        'r'
        range0
        "Create a list of integers from 0 to ceil(n)-1.\n\
        \This function is automatically vectorized."
        [ ("3r", all_ ["[0,1,2]"])
        , ("5\\2r", all_ ["[0,1,2]"])
        , ("1_r", all_ ["[]"])
        , ("[3,4]r", all_ ["[[0,1,2],[0,1,2,3]]"])
        ]
    , Builtin
        "range1"
        'R'
        range1
        "Create a list of integers from 1 to floor(n).\n\
        \This function is automatically vectorized."
        [ ("3R", all_ ["[1,2,3]"])
        , ("5\\2R", all_ ["[1,2]"])
        , ("1_R", all_ ["[]"])
        , ("[3,4]R", all_ ["[[1,2,3],[1,2,3,4]]"])
        ]
    , Builtin
        "interval"
        'ï'
        interval
        "Create a list of integers from ceil(x) to floor(y).\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("3 5ï", all_ ["[3,4,5]"])
        , ("5 3ï", all_ ["[]"])
        , ("3\\2 7\\2ï", all_ ["[2,3]"])
        , ("1_ [2,3,-3]ï", all_ ["[[-1,0,1,2],[-1,0,1,2,3],[]]"])
        , ("[3] [5,7]ï", all_ [])
        ]
    , Builtin
        "natural"
        'Ň'
        natural
        "Non-deterministically choose a natural number.\n\
        \This function is non-deterministic."
        [("Ň", truncate_ ["0", "1", "2", "3", "4", "5"])]
    , Builtin
        "integer"
        'Ž'
        integer
        "Non-deterministically choose an integer.\n\
        \This function is non-deterministic."
        [("Ž", truncate_ ["0", "1", "-1", "2", "-2", "3"])]
    , Builtin
        "sum"
        '∑'
        sum'
        "Take the sum of a list of numbers.\n\
        \The addition is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3]∑", all_ ["6"])
        , ("[[1,2],[3,4]]∑", all_ ["[4,6]"])
        , ("[1,[2,3]]∑", all_ ["[3,4]"])
        , ("[[],[1],[0,1],[0,0,1]]∑", all_ ["[1,1,1]"])
        ]
    , Builtin
        "product"
        '∏'
        product'
        "Take the product of a list of numbers.\n\
        \The multiplication is automatically vectorized \
        \and fails when the two lists are of different lengths.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3]∏", all_ ["6"])
        , ("[[1,2],[3,4]]∏", all_ ["[3,8]"])
        , ("[2,[3,4]]∏", all_ ["[6,8]"])
        , ("[[1],[2,3]]∏", all_ [])
        ]
    , Builtin
        "dot"
        '∙'
        dot
        "Take the dot product of two lists of numbers.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \The current implementation is simply a composition of \
        \mul and sum."
        [ ("[1,2,3] [4,5,6]∙", all_ ["32"])
        , ("[1,2,3] [[1,2],[3,4],[5,6]]∙", all_ ["[22,28]"])
        ]
    , Builtin
        "convolve"
        '×'
        convolve
        "Take the convolution of two lists of numbers.\n\
        \This is equivalent to multiplying two polynomials.\n\
        \If one of the arguments is a number, \
        \it simply multiplies the other argument by that number.\n\
        \If the arguments are nested lists, \
        \it takes the multi-dimensional convolution, \
        \which is equivalent to multiplying multivariate polynomials.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3] [4,5,6]×", all_ ["[4,13,28,27,18]"])
        , ("[1,2,3] 4×", all_ ["[4,8,12]"])
        , ("[1,2,3] [[1,2],[3,4]]×", all_ ["[[1,2],[5,8],[9,14],[9,12]]"])
        , ("[[0,1],[1]] [[0,1],[1]]×", all_ ["[[0,0,1],[0,2],[1]]"])
        ]
    , Builtin
        "mean"
        'µ'
        mean
        "Take the mean of a list of numbers.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3]µ", all_ ["2"])
        , ("[[1,2],[3,5]]µ", all_ ["[2,7/2]"])
        ]
    , Builtin
        "fromBase"
        'b'
        fromBase
        "Convert a list of digits to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
        \If the base is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized over the base."
        [ ("[1,2,3] 10b", all_ ["123"])
        , ("[1,2,3] 1\\10b", all_ ["321/100"])
        , ("[[1,2],[3,4],[5,6]] 10b", all_ ["[135,246]"])
        , ("[1,2,3] [10,100]b", all_ ["[123,10203]"])
        ]
    , Builtin
        "fromBaseRev"
        'd'
        fromBaseRev
        "Convert a list of digits in reverse order to a number.\n\
        \The first argument is the list of digits, \
        \the second argument is the base.\n\
        \This does not require the digits and the base to be integers.\n\
        \If the base is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized over the base."
        [ ("[1,2,3] 10d", all_ ["321"])
        , ("[1,2,3] 1\\10d", all_ ["123/100"])
        , ("[[1,2],[3,4],[5,6]] 10d", all_ ["[531,642]"])
        , ("[1,2,3] [10,100]d", all_ ["[321,30201]"])
        ]
    , Builtin
        "toBase"
        'D'
        toBase
        "Convert an integer to a list of digits.\n\
        \The first argument is the integer, \
        \the second argument is the base.\n\
        \Fails when the inputs are not integers, \
        \or the base is less than 2.\n\
        \If the integer is smaller than zero, \
        \it is negated before conversion.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized over both arguments. \
        \If both arguments are lists, \
        \the result is a list of lists of digits."
        [ ("123 10D", all_ ["[1,2,3]"])
        , ("123 1\\10D", all_ [])
        , ("123_ 10D", all_ ["[1,2,3]"])
        , ("[135,246] 10D", all_ ["[[1,3,5],[2,4,6]]"])
        , ("[135,246] [10,100]D", all_ ["[[[1,3,5],[2,4,6]],[[1,35],[2,46]]]"])
        ]
    , Builtin
        "toBaseRev"
        'B'
        toBaseRev
        "Convert an integer to a list of digits in reverse order.\n\
        \The first argument is the integer, \
        \the second argument is the base.\n\
        \Fails when the inputs are not integers, \
        \or the base is less than 2.\n\
        \If the integer is smaller than zero, \
        \it is negated before conversion.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized over both arguments. \
        \If both arguments are lists, \
        \the result is a list of lists of digits."
        [ ("123 10B", all_ ["[3,2,1]"])
        , ("123 1\\10B", all_ [])
        , ("123_ 10B", all_ ["[3,2,1]"])
        , ("[135,246] 10B", all_ ["[[5,3,1],[6,4,2]]"])
        , ("[135,246] [10,100]B", all_ ["[[[5,3,1],[6,4,2]],[[35,1],[46,2]]]"])
        ]
    , Builtin
        "binary"
        'Ƃ'
        binary'
        "Convert an integer to a list of binary digits in reverse order.\n\
        \If the integer is smaller than zero, \
        \it is negated before conversion.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("6Ƃ", all_ ["[0,1,1]"])
        , ("[-6,0,6]Ƃ", all_ ["[[0,1,1],[],[0,1,1]]"])
        ]
    , Builtin
        "fromBinary"
        'ƃ'
        fromBinary
        "Convert a list of binary digits in reverse order to an integer.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[0,1,1]ƃ", all_ ["6"])
        , ("[[1,0,1],[0,1,1]]ƃ", all_ ["[1,2,3]"])
        ]
    , Builtin
        "digits"
        'Ɗ'
        digits
        "Convert an integer to a list of decimal digits.\n\
        \If the integer is smaller than zero, \
        \it is negated before conversion.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("123Ɗ", all_ ["[1,2,3]"])
        , ("[-123,0,123]Ɗ", all_ ["[[1,2,3],[],[1,2,3]]"])
        ]
    , Builtin
        "fromDigits"
        'ɗ'
        fromDigits
        "Convert a list of decimal digits to an integer.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3]ɗ", all_ ["123"])
        , ("[[1,2,3],[0,1,2]]ɗ", all_ ["[10,21,32]"])
        ]
    , Builtin
        "cumsum"
        '∫'
        cumsum
        "Take the cumulative sum of a list of numbers.\n\
        \The addition is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,2,3]∫", all_ ["[1,3,6]"])
        , ("[[1,2],[3,4]]∫", all_ ["[[1,2],[4,6]]"])
        ]
    , Builtin
        "delta"
        '∆'
        delta
        "Take the difference between adjacent elements of a list of numbers.\n\
        \The subtraction is automatically vectorized with padding zeros.\n\
        \Fails when the input is empty.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[1,3,6]∆", all_ ["[2,3]"])
        , ("[[1,2],[4,6]]∆", all_ ["[[3,4]]"])
        , ("[1]∆", all_ ["[]"])
        , ("[]∆", all_ [])
        ]
    , Builtin
        "binomial"
        'Ç'
        binomial
        "Compute the binomial coefficient.\n\
        \The second argument must be an integer.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("5 3Ç", all_ ["10"])
        , ("5_ 3Ç", all_ ["-35"])
        , ("5 3_Ç", all_ ["0"])
        , ("1\\2 2Ç", all_ ["-1/8"])
        , ("[5,6] 3Ç", all_ ["[10,20]"])
        , ("5 [0,1,2,3,4,5]Ç", all_ ["[1,5,10,10,5,1]"])
        ]
    , Builtin
        "factorial"
        'F'
        factorial
        "Compute the factorial of an integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("0F", all_ ["1"])
        , ("5F", all_ ["120"])
        , ("[5,6]F", all_ ["[120,720]"])
        ]
    , Builtin
        "isPrime"
        'Q'
        isPrime'
        "Check if an integer is prime.\n\
        \Negative numbers whose absolute values are prime are also considered \
        \to be prime.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("1Q", all_ [])
        , ("2Q", all_ ["2"])
        , ("2_ Q", all_ ["-2"])
        , ("[1,2,3,4,5]Q‼", all_ ["[2,3,5]"])
        ]
    , Builtin
        "prime"
        'Ƥ'
        prime
        "Non-deterministically choose a prime number.\n\
        \This function is non-deterministic."
        [("Ƥ", truncate_ ["2", "3", "5", "7", "11", "13", "17", "19", "23"])]
    , Builtin
        "primePi"
        'ƥ'
        primePi
        "Compute the number of positive primes less than or equal to a \
        \number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("7ƥ", all_ ["4"])
        , ("7_ ƥ", all_ ["0"])
        , ("23\\2 ƥ", all_ ["5"])
        , ("[10,100,1000]ƥ", all_ ["[4,25,168]"])
        ]
    , Builtin
        "factor"
        'ƒ'
        factor
        "Factorize a rational number, \
        \and return a list of prime factors and a list of exponents.\n\
        \If the number is negative, it is negated before factorization.\n\
        \Fails when the input is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("12300ƒÐ", all_ ["[[2,3,5,41],[2,1,2,1]]"])
        , ("123\\100 ƒÐ", all_ ["[[2,3,5,41],[-2,1,-2,1]]"])
        , ("1ƒÐ", all_ ["[[],[]]"])
        , ("0ƒÐ", all_ [])
        , ("[6,-6]ƒÐ", all_ ["[[[2,3],[2,3]],[[1,1],[1,1]]]"])
        ]
    , Builtin
        "gcd"
        'G'
        gcd'
        "Compute the greatest common divisor of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("12 18G", all_ ["6"])
        , ("1\\12 1\\18G", all_ ["1/36"])
        , ("[12,18] [24,36]G", all_ ["[12,18]"])
        ]
    , Builtin
        "lcm"
        'g'
        lcm'
        "Compute the least common multiple of two numbers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("12 18g", all_ ["36"])
        , ("1\\12 1\\18g", all_ ["1/6"])
        , ("[12,18] [24,36]g", all_ ["[24,36]"])
        ]
    , Builtin
        "divisors"
        'Ď'
        divisors
        "Compute the list of positive divisors of an integer.\n\
        \Fail when the input is zero.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("12Ď", all_ ["[1,2,3,4,6,12]"])
        , ("12_ Ď", all_ ["[1,2,3,4,6,12]"])
        , ("0Ď", all_ [])
        , ("[12,18]Ď", all_ ["[[1,2,3,4,6,12],[1,2,3,6,9,18]]"])
        ]
    , Builtin
        "intPartition"
        'Ṗ'
        intPartition
        "Partition an integer into a list of positive integers, \
        \whose sum is the original integer.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is non-deterministic and automatically vectorized."
        [ ("4Ṗ", all_ ["[1,1,1,1]", "[1,1,2]", "[1,3]", "[2,2]", "[4]"])
        , ("0Ṗ", all_ ["[]"])
        , ("4_ Ṗ", all_ [])
        , ("[2,2]Ṗ", all_ ["[[1,1],[1,1]]", "[[1,1],[2]]", "[[2],[1,1]]", "[[2],[2]]"])
        ]
    , Builtin
        "sqrt"
        '√'
        sqrt'
        "Compute the square root of a rational number.\n\
        \Fails when the argument is not a perfect square.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("16√", all_ ["4"])
        , ("16\\9√", all_ ["4/3"])
        , ("8√", all_ [])
        , ("[16,25]√", all_ ["[4,5]"])
        ]
    , Builtin
        "unitVec2"
        'į'
        unitVec2
        "Choose one of [0, 1] and [1, 0] non-deterministically.\n\
        \This function is non-deterministic."
        [("į", all_ ["[0,1]", "[1,0]"])]
    , Builtin
        "orNeg"
        'ŋ'
        orNeg
        "Optionally negate a number.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is non-deterministic and automatically vectorized.\n\
        \When the input is a list, \
        \each element is optionally negated independently."
        [ ("1ŋ", all_ ["1", "-1"])
        , ("0ŋ", all_ ["0"])
        , ("[-1,2]ŋ", all_ ["[-1,2]", "[-1,-2]", "[1,2]", "[1,-2]"])
        ]
    , Builtin
        "bitAnd"
        '&'
        bitAnd
        "Bitwise AND of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized \
        \and fails when the two lists are of different lengths."
        [ ("5 3&", all_ ["1"])
        , ("[5,6] [3,4]&", all_ ["[1,4]"])
        , ("5 [3,4]&", all_ ["[1,4]"])
        , ("[5] [3,4]&", all_ [])
        ]
    , Builtin
        "bitOr"
        '|'
        bitOr
        "Bitwise OR of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding."
        [ ("5 3|", all_ ["7"])
        , ("[5,6] [3,4]|", all_ ["[7,6]"])
        , ("5 [3,4]|", all_ ["[7,5]"])
        , ("[5] [3,4]|", all_ ["[7,4]"])
        ]
    , Builtin
        "bitXor"
        'X'
        bitXor
        "Bitwise XOR of two integers.\n\
        \If one or both of the arguments are chars, \
        \they are converted to numbers according to Nekomata's code page.\n\
        \This function is automatically vectorized with padding."
        [ ("5 3X", all_ ["6"])
        , ("[5,6] [3,4]X", all_ ["[6,2]"])
        , ("5 [3,4]X", all_ ["[6,1]"])
        , ("[5] [3,4]X", all_ ["[6,4]"])
        ]
    , Builtin
        "popCount"
        'Þ'
        popCount'
        "Count the number of 1s in the binary digits of an integer.\n\
        \If the number is smaller than zero, the result is also negated.\n\
        \If the argument is a char, \
        \it is converted to a number according to Nekomata's code page.\n\
        \This function is automatically vectorized."
        [ ("13Þ", all_ ["3"])
        , ("[-13,0,13]Þ", all_ ["[-3,0,3]"])
        ]
    , Builtin
        "histogram"
        'Ħ'
        histogram
        "Compute the histogram of a list of integers.\n\
        \The result is a list, \
        \whose length is the maximum of the input list, \
        \and whose nth element is the number of occurrences \
        \of n in the input.\n\
        \Fails when the list contains negative integers or fractions.\n\
        \If the input is a ragged list, \
        \it is flattened before computation.\n\
        \If the input is a single integer, \
        \it is treated as a singleton list.\n\
        \If the input is a single char, \
        \it is converted to a number according to Nekomata's code page, \
        \and then treated as a singleton list."
        [ ("0Ħ", all_ ["[1]"])
        , ("1Ħ", all_ ["[0,1]"])
        , ("[1,2,3,2,1]Ħ", all_ ["[0,2,2,1]"])
        , ("[[1,2],[3,2],[1]]Ħ", all_ ["[0,2,2,1]"])
        ]
    , Builtin
        "sumEach"
        'Ŝ'
        sumEach
        "Take the sum of each list in a list of lists of numbers.\n\
        \The addition is automatically vectorized with padding zeros.\n\
        \If some of the elements are chars, \
        \they are converted to numbers according to Nekomata's code page."
        [ ("[[1,2],[3,4]]Ŝ", all_ ["[3,7]"])
        , ("[[1,2],[3,4],[5]]Ŝ", all_ ["[3,7,5]"])
        , ("[[[1,2],[3,4]],[[5,6],[7,8]]]Ŝ", all_ ["[[4,6],[12,14]]"])
        ]
    , Builtin
        "charToInt"
        'e'
        charToInt'
        "Convert a char to an integer according to Nekomata's code page.\n\
        \If the input is already an integer, it is left unchanged.\n\
        \This function is automatically vectorized."
        [ ("'a e", all_ ["97"])
        , ("\"Hello\"e", all_ ["[72,101,108,108,111]"])
        ]
    , Builtin
        "intToChar"
        'H'
        intToChar'
        "Convert an integer to a char according to Nekomata's code page.\n\
        \If the input is already a char, it is left unchanged.\n\
        \Fail when the integer is not in the range 0 to 255.\n\
        \This function is automatically vectorized."
        [ ("97H", all_ ["'a'"])
        , ("[72,101,108,108,111]H", all_ ["Hello"])
        ]
    , Builtin
        "read"
        'Ĝ'
        read'
        "Parse a string (a list of chars) or a single char \
        \as a Nekomata value.\n\
        \Fail when the string is not a valid Nekomata value."
        [ ("'1 Ĝ", all_ ["1"])
        , ("\"[1,2,3]\"Ĝ", all_ ["[1,2,3]"])
        ]
    , Builtin
        "show"
        'ĝ'
        show'
        "Convert a Nekomata value to a string (a list of chars)."
        [ ("1ĝU", all_ ["[\"1\"]"])
        , ("[1,2,3]ĝU", all_ ["[\"[1,2,3]\"]"])
        , ("\"Hello\"ĝU", all_ ["[\"\\\"Hello\\\"\"]"])
        ]
    , Builtin
        "anyOf"
        '~'
        anyOf'
        "Choose an element from a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[]~", all_ [])
        , ("[1,2,3]~", all_ ["1", "2", "3"])
        , ("5~", all_ ["0", "1", "2", "3", "4"])
        ]
    , Builtin
        "emptyList"
        'Ø'
        emptyList
        "Push an empty list."
        [("Ø", all_ ["[]"])]
    , Builtin
        "singleton"
        'U'
        singleton'
        "Create a list with a single element."
        [ ("1U", all_ ["[1]"])
        , ("[1]U", all_ ["[[1]]"])
        ]
    , Builtin
        "unsingleton"
        'z'
        unsingleton
        "Get the only element of a list with a single element.\n\
        \Fails when the list is empty or has more than one element."
        [ ("[1]z", all_ ["1"])
        , ("[[1]]z", all_ ["[1]"])
        , ("[]z", all_ [])
        , ("[1,2]z", all_ [])
        ]
    , Builtin
        "pair"
        'Ð'
        pair
        "Create a list with two elements."
        [ ("1 2Ð", all_ ["[1,2]"])
        , ("[1] 2Ð", all_ ["[[1],2]"])
        ]
    , Builtin
        "unpair"
        'đ'
        unpair
        "Get the two elements of a list with two elements.\n\
        \Fails when the length of the list is not 2."
        [ ("[1,2]đ+", all_ ["3"])
        , ("[]đ", all_ [])
        , ("[1]đ", all_ [])
        , ("[1,2,3]đ", all_ [])
        ]
    , Builtin
        "removeFail"
        '‼'
        removeFail
        "Remove failed items from a list."
        [ ("[1,2,3]‼", all_ ["[1,2,3]"])
        , ("[1,0,3]P‼", all_ ["[1,3]"])
        ]
    , Builtin
        "length"
        '#'
        length'
        "Get the length of a list."
        [ ("[1,2,3]#", all_ ["3"])
        , ("[]#", all_ ["0"])
        ]
    , Builtin
        "lengthIs"
        'L'
        lengthIs
        "Check if the length of a list is equal to a given integer.\n\
        \If it is, push the list itself, otherwise fail."
        [ ("[1,2,3] 3L", all_ ["[1,2,3]"])
        , ("[1,2,3] 4L", all_ [])
        ]
    , Builtin
        "nth"
        '@'
        nth
        "Get the nth element of a list.\n\
        \The index is 0-based.\n\
        \This function is automatically vectorized on the second argument."
        [ ("[1,2,3] 1@", all_ ["2"])
        , ("[1,2,3] [1,2]@", all_ ["[2,3]"])
        ]
    , Builtin
        "head"
        'h'
        head'
        "Get the first element of a list."
        [ ("[1,2,3]h", all_ ["1"])
        , ("[]h", all_ [])
        ]
    , Builtin
        "tail"
        't'
        tail'
        "Remove the first element of a list."
        [ ("[1,2,3]t", all_ ["[2,3]"])
        , ("[]t", all_ [])
        ]
    , Builtin
        "cons"
        'c'
        cons
        "Prepend an element to a list."
        [ ("[2,3] 1c", all_ ["[1,2,3]"])
        , ("[] 1c", all_ ["[1]"])
        ]
    , Builtin
        "uncons"
        'C'
        uncons
        "Get the first element and the rest of a list."
        [ ("[1,2,3]CÐ", all_ ["[[2,3],1]"])
        , ("[]C", all_ [])
        ]
    , Builtin
        "last"
        'l'
        last'
        "Get the last element of a list."
        [ ("[1,2,3]l", all_ ["3"])
        , ("[]l", all_ [])
        ]
    , Builtin
        "init"
        'i'
        init'
        "Remove the last element of a list."
        [ ("[1,2,3]i", all_ ["[1,2]"])
        , ("[]i", all_ [])
        ]
    , Builtin
        "snoc"
        'ɔ'
        snoc
        "Append an element to a list."
        [ ("[1,2] 3ɔ", all_ ["[1,2,3]"])
        , ("[] 1ɔ", all_ ["[1]"])
        ]
    , Builtin
        "unsnoc"
        'Ɔ'
        unsnoc
        "Get the last element and the rest of a list."
        [ ("[1,2,3]ƆÐ", all_ ["[[1,2],3]"])
        , ("[]Ɔ", all_ [])
        ]
    , Builtin
        "cons0"
        'ç'
        cons0
        "Prepend a zero to a list."
        [ ("[1,2,3]ç", all_ ["[0,1,2,3]"])
        , ("[]ç", all_ ["[0]"])
        ]
    , Builtin
        "reverse"
        '↔'
        reverse'
        "Reverse a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1."
        [ ("[1,2,3]↔", all_ ["[3,2,1]"])
        , ("3↔", all_ ["[2,1,0]"])
        ]
    , Builtin
        "prefix"
        'p'
        prefix
        "Get a prefix of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]p", all_ ["[]", "[1]", "[1,2]", "[1,2,3]"])
        , ("3p", all_ ["[]", "[0]", "[0,1]", "[0,1,2]"])
        ]
    , Builtin
        "suffix"
        's'
        suffix
        "Get a suffix of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]s", all_ ["[1,2,3]", "[2,3]", "[3]", "[]"])
        , ("3s", all_ ["[0,1,2]", "[1,2]", "[2]", "[]"])
        ]
    , Builtin
        "take"
        'T'
        take'
        "Get the first n elements of a list.\n\
        \Fail when the list is shorter than n.\n\
        \This function is automatically vectorized on the second argument."
        [ ("[1,2,3] 2T", all_ ["[1,2]"])
        , ("[1,2,3] 4T", all_ [])
        , ("[1,2,3] [2,3]T", all_ ["[[1,2],[1,2,3]]"])
        ]
    , Builtin
        "subset"
        'S'
        subset
        "Get a finite subset of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2] S", all_ ["[]", "[1]", "[2]", "[1,2]"])
        , ("2S", all_ ["[]", "[0]", "[1]", "[0,1]"])
        ]
    , Builtin
        "subsequence"
        'q'
        subsequence
        "Get a finite contiguous subsequence of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]q", all_ ["[]", "[1]", "[1,2]", "[1,2,3]", "[2]", "[2,3]", "[3]"])
        , ("3q", all_ ["[]", "[0]", "[0,1]", "[0,1,2]", "[1]", "[1,2]", "[2]"])
        ]
    , Builtin
        "join"
        ','
        join'
        "Concatenate two lists.\n\
        \If one of the arguments is a number or a char, \
        \it is converted to a singleton list before concatenation."
        [ ("[1,2] [3,4],", all_ ["[1,2,3,4]"])
        , ("[1,2] 3,", all_ ["[1,2,3]"])
        , ("1 [2,3],", all_ ["[1,2,3]"])
        , ("1 2,", all_ ["[1,2]"])
        ]
    , Builtin
        "split"
        ';'
        split
        "Split a list into two parts.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3];Ð", all_ ["[[],[1,2,3]]", "[[1],[2,3]]", "[[1,2],[3]]", "[[1,2,3],[]]"])
        , ("3;Ð", all_ ["[[],[0,1,2]]", "[[0],[1,2]]", "[[0,1],[2]]", "[[0,1,2],[]]"])
        ]
    , Builtin
        "replicate"
        'ř'
        replicate'
        "Create a list with n copies of an element.\n\
        \This function is automatically vectorized on the second argument."
        [ ("2 3ř", all_ ["[2,2,2]"])
        , ("'a 3ř", all_ ["aaa"])
        , ("[1,2] 3ř", all_ ["[[1,2],[1,2],[1,2]]"])
        , ("2 [3,4]ř", all_ ["[[2,2,2],[2,2,2,2]]"])
        ]
    , Builtin
        "minimum"
        'ṁ'
        minimum'
        "Get the minimum of a list.\n\
        \If there are multiple minimums, return the first one.\n\
        \Fail when the list is empty.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("[1,2,3]ṁ", all_ ["1"])
        , ("[[1,2],3]ṁ", all_ ["3"])
        , ("[[1,2],[3]]ṁ", all_ ["[1,2]"])
        , ("[1,'a',[1,2]]ṁ", all_ ["1"])
        ]
    , Builtin
        "maximum"
        'Ṁ'
        maximum'
        "Get the maximum of a list.\n\
        \If there are multiple maximums, return the first one.\n\
        \Fail when the list is empty.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("[1,2,3]Ṁ", all_ ["3"])
        , ("[[1,2],3]Ṁ", all_ ["[1,2]"])
        , ("[[1,2],[3]]Ṁ", all_ ["[3]"])
        , ("[1,'a',[1,2]]Ṁ", all_ ["[1,2]"])
        ]
    , Builtin
        "minMax"
        'ɱ'
        minMax
        "Get both the minimum and the maximum of a list.\n\
        \If there are multiple minimums or maximums, return the first one.\n\
        \Fail when the list is empty.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("[1,2,3]ɱÐ", all_ ["[1,3]"])
        , ("[[1,2],3]ɱÐ", all_ ["[3,[1,2]]"])
        , ("[[1,2],[3]]ɱÐ", all_ ["[[1,2],[3]]"])
        , ("[1,'a',[1,2]]ɱÐ", all_ ["[1,[1,2]]"])
        ]
    , Builtin
        "concat"
        'j'
        concat'
        "Concatenate a list of lists or a list.\n\
        \If one item in the list is a number or a char, \
        \it is converted to a singleton list before concatenation."
        [ ("[[1,2],[3,4]]j", all_ ["[1,2,3,4]"])
        , ("[1,2,3]j", all_ ["[1,2,3]"])
        , ("[1,[2,3],4]j", all_ ["[1,2,3,4]"])
        , ("[\"abc\",\"def\"]j", all_ ["abcdef"])
        ]
    , Builtin
        "unconcat"
        'J'
        unconcat
        "Split a list into a list of lists.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]J", all_ ["[[1],[2],[3]]", "[[1],[2,3]]", "[[1,2],[3]]", "[[1,2,3]]"])
        , ("3J", all_ ["[[0],[1],[2]]", "[[0],[1,2]]", "[[0,1],[2]]", "[[0,1,2]]"])
        ]
    , Builtin
        "nub"
        'u'
        nub
        "Remove duplicate elements from a list."
        [ ("[1,2,2,3,1]u", all_ ["[1,2,3]"])
        , ("[3,1,3,2,1]u", all_ ["[3,1,2]"])
        ]
    , Builtin
        "sort"
        'o'
        sort
        "Sort a list.\n\
        \This function uses an ordering that is defined on all values. \
        \Numbers are smaller than chars, which are smaller than lists. \
        \Lists are compared in the lexicographic order."
        [ ("[3,1,2]o", all_ ["[1,2,3]"])
        , ("['a',[3,4],'b',[2],1,[5]]o", all_ ["[1,'a','b',[2],[3,4],[5]]"])
        ]
    , Builtin
        "permutation"
        '↕'
        permutation
        "Get a permutation of a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]↕", all_ ["[1,2,3]", "[1,3,2]", "[2,1,3]", "[2,3,1]", "[3,1,2]", "[3,2,1]"])
        , ("[1,1,2]↕", all_ ["[1,1,2]", "[1,2,1]", "[1,1,2]", "[1,2,1]", "[2,1,1]", "[2,1,1]"])
        , ("3↕", all_ ["[0,1,2]", "[0,2,1]", "[1,0,2]", "[1,2,0]", "[2,0,1]", "[2,1,0]"])
        ]
    , Builtin
        "extract"
        'ĕ'
        extract
        "Extract an element from a list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \Returns the element and the rest of the list.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]ĕÐ", all_ ["[[2,3],1]", "[[1,3],2]", "[[1,2],3]"])
        , ("3ĕÐ", all_ ["[[1,2],0]", "[[0,2],1]", "[[0,1],2]"])
        ]
    , Builtin
        "allEqual"
        '≡'
        allEqual
        "Check if all elements in a list are equal.\n\
        \If it is, push the equal element, otherwise fail.\n\
        \If the list is empty, this function fails."
        [ ("[1,1,1]≡", all_ ["1"])
        , ("[1,2,1]≡", all_ [])
        , ("[1]≡", all_ ["1"])
        , ("[]≡", all_ [])
        ]
    , Builtin
        "isUnique"
        'ů'
        isUnique
        "Check if all elements in a list are unique.\n\
        \If it is, push the list itself, otherwise fail.\n\
        \The empty list is considered unique."
        [ ("[1,2,3]ů", all_ ["[1,2,3]"])
        , ("[1,2,1]ů", all_ [])
        , ("[1]ů", all_ ["[1]"])
        , ("[]ů", all_ ["[]"])
        ]
    , Builtin
        "free"
        'f'
        free
        "Check if a list is free of a given element.\n\
        \This means that the list is not equal to the element, \
        \and recursively, every item of the list if free of that element.\n\
        \If it is, push the list itself, otherwise fail."
        [ ("[1,2,3] 2f", all_ [])
        , ("[1,3,1] 2f", all_ ["[1,3,1]"])
        , ("[1,[2,3]] 2f", all_ [])
        , ("2 2f", all_ [])
        , ("3 2f", all_ ["3"])
        , ("[1,2,3] [2,3]f", all_ ["[1,2,3]"])
        , ("[1,[2,3]] [2,3]f", all_ [])
        ]
    , Builtin
        "enumerate"
        'x'
        enumerate
        "Push a list of integers from 0 to the length of the argument minus 1 \
        \without popping the original argument."
        [ ("[1,2,3]xÐ", all_ ["[[1,2,3],[0,1,2]]"])
        , ("[4,3,2,1]xÐ", all_ ["[[4,3,2,1],[0,1,2,3]]"])
        ]
    , Builtin
        "rotate"
        'Ř'
        rotate
        "Rotate a list by a given number of positions.\n\
        \If the first argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is automatically vectorized on the second argument."
        [ ("[1,2,3] 1Ř", all_ ["[2,3,1]"])
        , ("[1,2,3] 1_Ř", all_ ["[3,1,2]"])
        , ("[1,2,3] [4,5]Ř", all_ ["[[2,3,1],[3,1,2]]"])
        , ("3 1Ř", all_ ["[1,2,0]"])
        ]
    , Builtin
        "transpose"
        'Ť'
        transpose
        "Transpose a list of lists.\n\
        \Fail if the sublists are not all of the same length."
        [ ("[[1,2],[3,4],[5,6]]Ť", all_ ["[[1,3,5],[2,4,6]]"])
        , ("[[1,2],[3,4,5]]Ť", all_ [])
        ]
    , Builtin
        "setPartition"
        'O'
        setPartition
        "Partition a list into a list of lists such that their concatenation \
        \is a permutation of the original list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic."
        [ ("[1,2,3]O", all_ ["[[1,2,3]]", "[[2,3],[1]]", "[[1,3],[2]]", "[[3],[1,2]]", "[[3],[2],[1]]"])
        , ("3O", all_ ["[[0,1,2]]", "[[1,2],[0]]", "[[0,2],[1]]", "[[2],[0,1]]", "[[2],[1],[0]]"])
        ]
    , Builtin
        "setMinus"
        '∕'
        setMinus
        "For each element in the second list, \
        \remove the first occurrence of that element in the first list.\n\
        \If the second argument is a number or a char, \
        \it is converted to a singleton list."
        [ ("[1,2,3,2,1] [2,1]∕", all_ ["[3,2,1]"])
        , ("[1,2,3,2,1] [2,1,1,1]∕", all_ ["[3,2]"])
        , ("[1,2,3,2,1] [2,4]∕", all_ ["[1,3,2,1]"])
        , ("[1,2,3,2,1] 2∕", all_ ["[1,3,2,1]"])
        ]
    , Builtin
        "index"
        'Ĩ'
        index
        "Get the index of any occurrence of an element in a list.\n\
        \The index is 0-based.\n\
        \Fail if the element does not occur in the list.\n\
        \This function is non-deterministic."
        [ ("[1,2,3,2,1] 2Ĩ", all_ ["1", "3"])
        , ("[1,2,3,2,1] 4Ĩ", all_ [])
        ]
    , Builtin
        "count"
        'Ĉ'
        count
        "Count the number of occurrences of an element in a list."
        [ ("[1,2,3,2,1] 2Ĉ", all_ ["2"])
        , ("[1,2,3,2,1] 4Ĉ", all_ ["0"])
        ]
    , Builtin
        "tally"
        'Ţ'
        tally
        "Count the number of occurrences of each element in a list.\n\
        \Return a list of elements and a list of counts in the same order."
        [ ("[1,2,3,2,1]ŢÐ", all_ ["[[1,2,3],[2,2,1]]"])
        , ("[3,1,3,2,1]ŢÐ", all_ ["[[3,1,2],[2,2,1]]"])
        , ("[]ŢÐ", all_ ["[[],[]]"])
        ]
    , Builtin
        "intersect"
        '∩'
        intersect
        "Get the multiset intersection of two lists.\n\
        \If one of the arguments is a number or a char, \
        \it is converted to a singleton list."
        [ ("[1,2,3,2,1] [2,1]∩", all_ ["[2,1]"])
        , ("[1,2,3,2,1] [2,1,1,1]∩", all_ ["[2,1,1]"])
        , ("[1,2,3,2,1] [2,4]∩", all_ ["[2]"])
        , ("[1,1,2,3] [1,2,3,3]∩", all_ ["[1,2,3]"])
        , ("[1,2,3,2,1] 2∩", all_ ["[2]"])
        ]
    , Builtin
        "union"
        'Ŭ'
        union
        "Get the multiset union of two lists."
        [ ("[1,2,3,2,1] [2,1]Ŭ", all_ ["[1,2,3,2,1]"])
        , ("[1,2,3,2,1] [2,1,1,1]Ŭ", all_ ["[1,2,3,2,1,1]"])
        , ("[1,2,3,2,1] [2,4]Ŭ", all_ ["[1,2,3,2,1,4]"])
        , ("[1,1,2,3] [1,2,3,3]Ŭ", all_ ["[1,1,2,3,3]"])
        , ("[1,2,3,2,1] 4Ŭ", all_ ["[1,2,3,2,1,4]"])
        ]
    , Builtin
        "chunks"
        'ĉ'
        chunks
        "Split a list into a list of chunks of equal elements."
        [ ("[1,1,2,2,2,3,3,3,3]ĉ", all_ ["[[1,1],[2,2,2],[3,3,3,3]]"])
        , ("\"aaabbbccaa\"ĉ", all_ ["[\"aaa\",\"bbb\",\"cc\",\"aa\"]"])
        ]
    , Builtin
        "rle"
        'Y'
        rle
        "Run-length encode a list.\n\
        \Returns a list of elements and a list of lengths."
        [ ("[1,1,2,2,2,3,3,3,3]YÐ", all_ ["[[1,2,3],[2,3,4]]"])
        , ("\"aaabbbccaa\"YÐ", all_ ["[\"abca\",[3,3,2,2]]"])
        ]
    , Builtin
        "unrle"
        'y'
        unrle
        "Run-length decode a list.\n\
        \The first argument is a list of elements, \
        \the second argument is a list of lengths.\n\
        \Fails when the two lists are of different lengths."
        [ ("[1,2,3] [2,3,4]y", all_ ["[1,1,2,2,2,3,3,3,3]"])
        , ("\"abca\" [3,3,2,2]y", all_ ["aaabbbccaa"])
        ]
    , Builtin
        "slices"
        'Š'
        slices
        "Split a list into a list of slices of a given length.\n\
        \If the length of the list is not a multiple of the slice length, \
        \the last slice is shorter.\n\
        \If the first argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \Fails when the given length is not positive."
        [ ("[1,2,3,4,5,6] 2Š", all_ ["[[1,2],[3,4],[5,6]]"])
        , ("[1,2,3,4,5,6] 3Š", all_ ["[[1,2,3],[4,5,6]]"])
        , ("[1,2,3,4,5,6] 4Š", all_ ["[[1,2,3,4],[5,6]]"])
        , ("[1,2,3,4,5,6] 8Š", all_ ["[[1,2,3,4,5,6]]"])
        ]
    , Builtin
        "uninterleave"
        'ĭ'
        uninterleave
        "uninterleave a list into a list of elements \
        \at even positions and a list of elements at odd positions.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1."
        [ ("[1,2,3,4,5,6]ĭÐ", all_ ["[[1,3,5],[2,4,6]]"])
        , ("[1,2,3,4,5]ĭÐ", all_ ["[[1,3,5],[2,4]]"])
        , ("5ĭÐ", all_ ["[[0,2,4],[1,3]]"])
        ]
    , Builtin
        "interleave"
        'Ĭ'
        interleave
        "Interleave two lists.\n\
        \The length of the first list must be either equal to or one more than \
        \the length of the second list. Otherwise, this function fails."
        [ ("[1,3,5] [2,4,6]Ĭ", all_ ["[1,2,3,4,5,6]"])
        , ("[1,3,5] [2,4]Ĭ", all_ ["[1,2,3,4,5]"])
        , ("[2,4] [1,3,5]Ĭ", all_ [])
        ]
    , Builtin
        "minimumBy"
        'ṃ'
        minimumBy
        "Get the minimum value of a list according to a list of keys.\n\
        \If there are multiple minimums, \
        \return any of them non-deterministically.\n\
        \Fails when the two lists are of different lengths.\n\
        \This function is non-deterministic."
        [ ("[1,2,3,4,5] [2,4,5,1,3]ṃ", all_ ["4"])
        , ("[1,2,3,4,5] [1,2,1,2,1]ṃ", all_ ["1", "3", "5"])
        ]
    , Builtin
        "maximumBy"
        'Ṃ'
        maximumBy
        "Get the maximum value of a list according to a list of keys.\n\
        \If there are multiple maximums, \
        \return any of them non-deterministically.\n\
        \Fails when the two lists are of different lengths.\n\
        \This function is non-deterministic."
        [ ("[1,2,3,4,5] [2,4,5,1,3]Ṃ", all_ ["3"])
        , ("[1,2,3,4,5] [1,2,1,2,1]Ṃ", all_ ["2", "4"])
        ]
    , Builtin
        "shortest"
        'ş'
        shortest
        "Get the shortest one in a list of lists.\n\
        \If there are multiple shortest ones, \
        \return any of them non-deterministically.\n\
        \This function is non-deterministic."
        [ ("[[1,2,3],[4],[5,6]]ş", all_ ["[4]"])
        , ("[[1,2],[3,4],[5],[6]]ş", all_ ["[5]", "[6]"])
        ]
    , Builtin
        "longest"
        'Ş'
        longest
        "Get the longest one in a list of lists.\n\
        \If there are multiple longest ones, \
        \return any of them non-deterministically.\n\
        \This function is non-deterministic."
        [ ("[[1,2,3],[4],[5,6]]Ş", all_ ["[1,2,3]"])
        , ("[[1,2],[3,4],[5],[6]]Ş", all_ ["[1,2]", "[3,4]"])
        ]
    , Builtin
        "tuple"
        'ŧ'
        tuple
        "Create a list with length n, \
        \whose elements are taken from another list.\n\
        \If the first argument is a number, \
        \it is converted to a range from 0 to that number minus 1.\n\
        \This function is non-deterministic, \
        \and automatically vectorized on the second argument."
        [ ("[1,2] 2ŧ", all_ ["[1,1]", "[1,2]", "[2,1]", "[2,2]"])
        , ("2 2ŧ", all_ ["[0,0]", "[0,1]", "[1,0]", "[1,1]"])
        ]
    , Builtin
        "bifurcate"
        'ƀ'
        bifurcate
        "Push the reverse of a list without popping the original list.\n\
        \If the argument is a number, \
        \it is converted to a range from 0 to that number minus 1."
        [ ("[1,2,3]ƀÐ", all_ ["[[1,2,3],[3,2,1]]"])
        , ("3ƀÐ", all_ ["[[0,1,2],[2,1,0]]"])
        ]
    , Builtin
        "flatten"
        'V'
        flatten
        "Flatten a nested list.\n\
        \If the argument is a number or a char, \
        \it is converted to a singleton list."
        [ ("[[1,2],[3,4]]V", all_ ["[1,2,3,4]"])
        , ("[1,2,3]V", all_ ["[1,2,3]"])
        , ("[1,[2,[3,4]]]V", all_ ["[1,2,3,4]"])
        , ("1V", all_ ["[1]"])
        ]
    , Builtin
        "pad"
        'Ḟ'
        pad
        "Pad a nested list with zeros to make it rectangular.\n\
        \If the argument is a number or a char, it is unchanged."
        [ ("[[1,2],[3]]Ḟ", all_ ["[[1,2],[3,0]]"])
        , ("[[[1,2,3],[4,5]],6]Ḟ", all_ ["[[[1,2,3],[4,5,0]],[[6,0,0],[0,0,0]]]"])
        , ("[1,2]Ḟ", all_ ["[1,2]"])
        , ("1Ḟ", all_ ["1"])
        ]
    , Builtin
        "ordering"
        'õ'
        ordering
        "Get the ordering of a list.\n\
        \The n'th element of the result is the index of the n'th element \
        \in the sorted list."
        [ ("[3,1,2]õ", all_ ["[1,2,0]"])
        , ("[1,2,3]õ", all_ ["[0,1,2]"])
        , ("[1,1,2]õ", all_ ["[0,1,2]"])
        ]
    , Builtin
        "elem"
        'ē'
        elem'
        "Check if an element is in a list.\n\
        \If it is, push the element, otherwise fail."
        [ ("2 [1,2,3]ē", all_ ["2"])
        , ("4 [1,2,3]ē", all_ [])
        , ("'a \"abc\"ē", all_ ["'a'"])
        ]
    , Builtin
        "filterBy"
        'ḟ'
        filterBy
        "Filter a list by whether the corresponding element in another list \
        \is not failed.\n\
        \If the first list also contains failed items, \
        \those items are also removed.\n\
        \Fail when the two lists are of different lengths."
        [ ("[1,2,3,4] [1,0,1,0]Zḟ", all_ ["[1,3]"])
        , ("[1,2,3,4] [1,0,1]Zḟ", all_ [])
        ]
    ]

-- | The map from names to builtin functions
builtinMap :: Map String Builtin
builtinMap = Map.fromList [(name b, b) | b <- builtins]

-- | The map from short names to builtin functions
builtinShortMap :: Map Char Builtin
builtinShortMap = Map.fromList [(short b, b) | b <- builtins]

-- | An error that occurs when a builtin function is not found
data BuiltinNotFoundError
    = -- | Cannot find a builtin function with the given full name
      BuiltinNotFound String
    | -- | Cannot find a builtin function with the given short name
      BuiltinShortNotFound Char
    deriving (Eq)

instance Show BuiltinNotFoundError where
    show (BuiltinNotFound name') =
        "cannot find builtin function with full name \"\\" ++ name' ++ "\""
    show (BuiltinShortNotFound short') =
        "cannot find builtin function with short name '" ++ [short'] ++ "'"
