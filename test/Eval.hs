{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eval (testBuiltins, testEval) where

import Control.Monad
import Data.Either (isRight)
import Nekomata.Builtin
import Nekomata.Eval
import Test.Hspec

shouldMatch :: NekomataData -> Result -> Expectation
shouldMatch x (All False xs) = allResults x `shouldBe` xs
shouldMatch x (All True xs) = take (length xs) (allResults x) `shouldBe` xs
shouldMatch x (First y) = firstResult x `shouldBe` y
shouldMatch x (Count n) = countResults x `shouldBe` n
shouldMatch x (Check b) = checkResult x `shouldBe` b

specBuiltin :: Builtin -> Spec
specBuiltin builtin =
    if null (examples builtin)
        then return ()
        else describe (show builtin ++ " (" ++ [short builtin] ++ ")") $ do
            forM_ (examples builtin) $ \(input, output) -> do
                it (input ++ " -> " ++ show output) $ do
                    let Right f = compile input
                    let runtime = initRuntime []
                    let result = snd $ runFunction f runtime
                    result `shouldMatch` output

testBuiltins :: Spec
testBuiltins = describe "Examples of built-in functions" $ do
    forM_ builtins specBuiltin

specEval :: String -> [(String, Result)] -> Spec
specEval code testCases = context code $ do
    it "should compile" $ do
        code `shouldSatisfy` isRight . compile
    let Right f = compile code
    forM_ testCases $ \(input, output) -> do
        it (input ++ " -> " ++ show output) $ do
            let Right input' = readInput input
            let runtime = initRuntime input'
            let result = snd $ runFunction f runtime
            result `shouldMatch` output

all_ :: [String] -> Result
all_ = All False

truncate_ :: [String] -> Result
truncate_ = All True

first_ :: String -> Result
first_ = First . Just

nothing_ :: Result
nothing_ = First Nothing

testEval :: Spec
testEval = describe "Solutions to Code Golf Stack Exchange challenges" $ do
    describe "q69: Golf you a quine for great good!" $ do
        specEval
            "\"ᵉĝ,\"ᵉĝ,"
            [("", all_ ["\"ᵉĝ,\"ᵉĝ,"])]
    describe "q85: Fibonacci function or sequence" $ do
        specEval
            "1:ᶦ{$ᵉ+"
            [("", truncate_ ["1", "1", "2", "3", "5", "8", "13", "21", "34", "55"])]
        specEval
            "ʷ{←Pᶜ←"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 3)
            , ("4", Count 5)
            , ("5", Count 8)
            , ("6", Count 13)
            , ("7", Count 21)
            , ("8", Count 34)
            , ("9", Count 55)
            ]
        specEval
            "ᴶ#3<"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 3)
            , ("4", Count 5)
            , ("5", Count 8)
            , ("6", Count 13)
            , ("7", Count 21)
            , ("8", Count 34)
            , ("9", Count 55)
            ]
    describe "q183: Finding \"sub-palindromes\"." $ do
        specEval
            "qŁƀ=ũ"
            [ ("\"12131331\"", all_ ["121", "131", "313", "1331", "33"])
            , ("\"3333\"", all_ ["33", "333", "3333"])
            ]
    describe "q1294: Determine whether strings are anagrams" $ do
        specEval
            "↕="
            [ ("\"boat\" \"boat\"", Check True)
            , ("\"toab\" \"boat\"", Check True)
            , ("\"oabt\" \"toab\"", Check True)
            , ("\"a\" \"aa\"", Check False)
            , ("\"zzz\" \"zzzzzzzz\"", Check False)
            , ("\"zyyyzzzz\" \"yyzzzzzy\"", Check True)
            , ("\"sleepy\" \"pyels\"", Check False)
            , ("\"p\" \"p\"", Check True)
            ]
    describe "q5529: Is string X a subsequence of string Y?" $ do
        specEval
            "S="
            [ ("\"z00\" \"\"", Check True)
            , ("\"z00\" \"z00\"", Check True)
            , ("\"00z0\" \"z00\"", Check False)
            , ("\"anna\" \"aa\"", Check True)
            , ("\"banana\" \"anna\"", Check True)
            , ("\"banana\" \"Anna\"", Check False)
            ]
    describe "q12177: Collatz Conjecture (OEIS A006577)" $ do
        specEval
            "ˡ{Ƶᵉ½3*→I"
            [ ("2", all_ ["1"])
            , ("16", all_ ["4"])
            , ("5", all_ ["5"])
            , ("7", all_ ["16"])
            ]
    describe "q12902: Run Length Decoding" $ do
        specEval
            "ĭᵐĜy"
            [("\":144,1'1\"", all_ [":4444,'"])]
    describe "q38325: Minimum excluded number" $ do
        specEval
            "ᵏf"
            [ ("[1]", all_ ["0"])
            , ("[0]", all_ ["1"])
            , ("[2,0]", all_ ["1"])
            , ("[3,1,0,1,3,3]", all_ ["2"])
            , ("[]", all_ ["0"])
            , ("[1,2,3]", all_ ["0"])
            , ("[5,4,1,5,4,8,2,1,5,4,0,7,7]", all_ ["3"])
            , ("[3,2,1,0]", all_ ["4"])
            , ("[0,0,1,1,2,2,3]", all_ ["4"])
            , ("[1,0,7,6,3,11,15,1,9,2,3,1,5,2,3,4,6,8,1,18]", all_ ["10"])
            ]
    describe "q42529: Mode (most common element) of a list" $ do
        specEval
            "ŢṂ"
            [("[4,3,1,0,6,1,6,4,4,0,3,1,7,7,3,4,1,1,2,8]", all_ ["1"])]
    describe "q46836: Total number of topological sorts" $ do
        specEval
            "↕ᵚ{S="
            [("6 [[0,1],[0,2],[0,3],[0,5],[1,2],[1,4],[3,2],[5,3]]", Count 9)]
    describe "q50020: List Sophie Germain primes" $ do
        specEval
            "Ƥ←½Q"
            [("", truncate_ ["2", "3", "5", "11", "23", "29", "41", "53", "83", "89"])]
    describe "q50240: XOR multiplication" $ do
        specEval
            "ᵃƂ×Öƃ"
            [ ("0 1", all_ ["0"])
            , ("1 2", all_ ["2"])
            , ("9 0", all_ ["0"])
            , ("6 1", all_ ["6"])
            , ("3 3", all_ ["5"])
            , ("2 5", all_ ["10"])
            , ("7 9", all_ ["63"])
            , ("13 11", all_ ["127"])
            , ("5 17", all_ ["85"])
            , ("14 13", all_ ["70"])
            , ("19 1", all_ ["19"])
            , ("63 63", all_ ["1365"])
            ]
    describe "q50472: Check if words are isomorphs" $ do
        specEval
            "ŤuŤᵐů"
            [ ("[\"ESTATE\",\"DUELED\"]", Check True)
            , ("[\"DUELED\",\"ESTATE\"]", Check True)
            , ("[\"XXX\",\"YYY\"]", Check True)
            , ("[\"CBAABC\",\"DEFFED\"]", Check True)
            , ("[\"RAMBUNCTIOUSLY\",\"THERMODYNAMICS\"]", Check True)
            , ("[\"DISCRIMINATIVE\",\"SIMPLIFICATION\"]", Check True)
            , ("[\"SEE\",\"SAW\"]", Check False)
            , ("[\"ANTS\",\"PANTS\"]", Check False)
            , ("[\"BANANA\",\"SERENE\"]", Check False)
            , ("[\"BANANA\",\"SENSES\"]", Check False)
            , ("[\"AB\",\"CC\"]", Check False)
            , ("[\"XXY\",\"XYY\"]", Check False)
            , ("[\"ABCBACCBA\",\"ABCBACCAB\"]", Check False)
            , ("[\"ABAB\",\"CD\"]", Check False)
            ]
        specEval
            "ᶻᵒ-¬≡"
            [ ("[\"ESTATE\",\"DUELED\"]", Check True)
            , ("[\"DUELED\",\"ESTATE\"]", Check True)
            , ("[\"XXX\",\"YYY\"]", Check True)
            , ("[\"CBAABC\",\"DEFFED\"]", Check True)
            , ("[\"RAMBUNCTIOUSLY\",\"THERMODYNAMICS\"]", Check True)
            , ("[\"DISCRIMINATIVE\",\"SIMPLIFICATION\"]", Check True)
            , ("[\"SEE\",\"SAW\"]", Check False)
            , ("[\"ANTS\",\"PANTS\"]", Check False)
            , ("[\"BANANA\",\"SERENE\"]", Check False)
            , ("[\"BANANA\",\"SENSES\"]", Check False)
            , ("[\"AB\",\"CC\"]", Check False)
            , ("[\"XXY\",\"XYY\"]", Check False)
            , ("[\"ABCBACCBA\",\"ABCBACCAB\"]", Check False)
            , ("[\"ABAB\",\"CD\"]", Check False)
            ]
    describe "q55422: \"Hello, World!\"" $ do
        specEval
            "\"Hello, World!\""
            [("", all_ ["Hello, World!"])]
    describe "q57617: Is this number a prime?" $ do
        specEval
            "Q"
            [ ("1", Check False)
            , ("2", Check True)
            , ("3", Check True)
            , ("4", Check False)
            , ("5", Check True)
            , ("6", Check False)
            , ("7", Check True)
            , ("8", Check False)
            , ("9", Check False)
            ]
        specEval
            "Ďđ"
            [ ("1", Check False)
            , ("2", Check True)
            , ("3", Check True)
            , ("4", Check False)
            , ("5", Check True)
            , ("6", Check False)
            , ("7", Check True)
            , ("8", Check False)
            , ("9", Check False)
            ]
    describe "q60610: Non-Unique/Duplicate Elements" $ do
        specEval
            "u∕u"
            [ ("[]", all_ ["[]"])
            , ("[-1,0,1]", all_ ["[]"])
            , ("[1,1]", all_ ["[1]"])
            , ("[3,0,0,1,1,0,5,3]", all_ ["[0,1,3]"])
            , ("[-34,0,1,-34,4,8,4]", all_ ["[-34,4]"])
            ]
        specEval
            "ŢƵḟ"
            [ ("[]", all_ ["[]"])
            , ("[-1,0,1]", all_ ["[]"])
            , ("[1,1]", all_ ["[1]"])
            , ("[3,0,0,1,1,0,5,3]", all_ ["[3,0,1]"])
            , ("[-34,0,1,-34,4,8,4]", all_ ["[-34,4]"])
            ]
    describe "q61808: Lossy Sorting (Implement Dropsort)" $ do
        specEval
            "pƆᵖ≤"
            [ ("[1,2,5,4,3,7]", all_ ["1", "2", "5", "7"])
            , ("[10,-1,12]", all_ ["10", "12"])
            , ("[-7,-8,-5,0,-1,1]", all_ ["-7", "-5", "0", "1"])
            , ("[9,8,7,6,5]", all_ ["9"])
            , ("[10,13,17,21]", all_ ["10", "13", "17", "21"])
            , ("[10,10,10,9,10]", all_ ["10", "10", "10", "10"])
            ]
    describe "q62732: Implement a Truth-Machine" $ do
        specEval
            "ᶦP"
            [ ("0", all_ ["0"])
            , ("1", truncate_ ["1", "1", "1", "1", "1", "1", "1", "1", "1", "1"])
            ]
    describe "q62752: Longest Common Prefix of 2 Strings" $ do
        specEval
            "ᵃp=al"
            [ ("\"global\" \"glossary\"", all_ ["glo"])
            , ("\"department\" \"depart\"", all_ ["depart"])
            , ("\"glove\" \"dove\"", all_ ["[]"])
            ]
    describe "q63999: Parenthifiable Binary Numbers" $ do
        specEval
            "Ƃ£E∫Ɔž≥"
            [ ("2", Check True)
            , ("10", Check True)
            , ("12", Check True)
            , ("42", Check True)
            , ("44", Check True)
            , ("50", Check True)
            , ("52", Check True)
            , ("56", Check True)
            , ("1", Check False)
            , ("3", Check False)
            , ("4", Check False)
            , ("18", Check False)
            , ("27", Check False)
            , ("39", Check False)
            , ("74", Check False)
            , ("80", Check False)
            , ("86", Check False)
            ]
    describe "q65770: Cover up zeroes in a list" $ do
        specEval
            "pZ‼l"
            [ ("[1,0,2,0,7,7,7,0,5,0,0,0,9]", all_ ["1", "1", "2", "2", "7", "7", "7", "7", "5", "5", "5", "5", "9"])
            , ("[1,0,0,0,0,0]", all_ ["1", "1", "1", "1", "1", "1"])
            , ("[-1,0,5,0,0,-7]", all_ ["-1", "-1", "5", "5", "5", "-7"])
            , ("[23,0,0,-42,0,0,0]", all_ ["23", "23", "23", "-42", "-42", "-42", "-42"])
            , ("[1,2,3,4]", all_ ["1", "2", "3", "4"])
            , ("[-1234]", all_ ["-1234"])
            ]
    describe "q66127: Catalan Numbers" $ do
        specEval
            "Ä$Ç$→/"
            [ ("0", all_ ["1"])
            , ("1", all_ ["1"])
            , ("2", all_ ["2"])
            , ("3", all_ ["5"])
            , ("4", all_ ["14"])
            , ("5", all_ ["42"])
            , ("6", all_ ["132"])
            , ("7", all_ ["429"])
            , ("8", all_ ["1430"])
            , ("9", all_ ["4862"])
            ]
        specEval
            "Äřŋ∫çƆž≥"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 5)
            , ("4", Count 14)
            , ("5", Count 42)
            , ("6", Count 132)
            , ("7", Count 429)
            , ("8", Count 1430)
            ]
        specEval
            "ÄØ$ᵑ{ᵉçt?}Ø="
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 5)
            , ("4", Count 14)
            , ("5", Count 42)
            , ("6", Count 132)
            , ("7", Count 429)
            , ("8", Count 1430)
            ]
    describe "q66851: Motzkin Numbers" $ do
        specEval
            "3$ŧ←∫Ɔž≥"
            [ ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 4)
            , ("4", Count 9)
            , ("5", Count 21)
            , ("6", Count 51)
            , ("7", Count 127)
            , ("8", Count 323)
            , ("9", Count 835)
            , ("10", Count 2188)
            ]
    describe "q68685: The Rien Number" $ do
        specEval
            "RtƊjo1cɗ"
            [ ("1", all_ ["1"])
            , ("2", all_ ["12"])
            , ("3", all_ ["123"])
            , ("7", all_ ["1234567"])
            , ("9", all_ ["123456789"])
            , ("10", all_ ["10123456789"])
            , ("15", all_ ["101111111223344556789"])
            , ("34", all_ ["10001111111111111222222222222223333333334444555666777888999"])
            , ("42", all_ ["100001111111111111122222222222222233333333333333444444455556666777788889999"])
            ]
    describe "q70365: Construct the Identity Matrix" $ do
        specEval
            "ᵒ-¬"
            [ ("1", all_ ["[[1]]"])
            , ("2", all_ ["[[1,0],[0,1]]"])
            , ("3", all_ ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", all_ ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
            ]
        specEval
            "ᵐĦḞ"
            [ ("1", all_ ["[[1]]"])
            , ("2", all_ ["[[1,0],[0,1]]"])
            , ("3", all_ ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", all_ ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
            ]
    describe "q70558: Hypercube elements" $ do
        specEval
            "→rᵉÇË*"
            [ ("0", all_ ["[1]"])
            , ("1", all_ ["[1,2]"])
            , ("3", all_ ["[1,6,12,8]"])
            , ("10", all_ ["[1,20,180,960,3360,8064,13440,15360,11520,5120,1024]"])
            , ("12", all_ ["[1,24,264,1760,7920,25344,59136,101376,126720,112640,67584,24576,4096]"])
            ]
        specEval
            "2ᵚR1cʳ×"
            [ ("0", all_ ["1"])
            , ("1", all_ ["[1,2]"])
            , ("3", all_ ["[1,6,12,8]"])
            , ("10", all_ ["[1,20,180,960,3360,8064,13440,15360,11520,5120,1024]"])
            , ("12", all_ ["[1,24,264,1760,7920,25344,59136,101376,126720,112640,67584,24576,4096]"])
            ]
    describe "q70837: Say What You See" $ do
        specEval
            "1ᶦ{ƊY$Ĭɗ"
            [("", truncate_ ["1", "11", "21", "1211", "111221", "312211", "13112221", "1113213211", "31131211131221", "13211311123113112211"])]
    describe "q70779: Consolidate an Array" $ do
        specEval
            "¬õ@"
            [ ("[1,0,2,0,-5,0,3,4,5,0,0,6]", all_ ["[1,2,-5,3,4,5,6,0,0,0,0,0]"])
            , ("[0,5,8,8,3,5,1,6,8,4,0,3,7,5]", all_ ["[5,8,8,3,5,1,6,8,4,3,7,5,0,0]"])
            ]
    describe "q71476: Determine the depth of an array" $ do
        specEval
            "ˡ∑"
            [ ("[1]", all_ ["1"])
            , ("[1,2,3]", all_ ["1"])
            , ("[[1,2,3]]", all_ ["2"])
            , ("[3,[3,[3],3],3]", all_ ["3"])
            , ("[[[[1],2],[3,[4]]]]", all_ ["4"])
            , ("[1,[[3]],[5,6],[[[[8]]]],1]", all_ ["5"])
            , ("[1,[[2,3,[[4],5],6,[7,8]],9,[10,[[[11]]]],12,13],14]", all_ ["6"])
            , ("[[[[[[[3]]]]]]]", all_ ["7"])
            ]
    describe "q71833: How even is a number?" $ do
        specEval
            "ˡ½"
            [ ("14", all_ ["1"])
            , ("20", all_ ["2"])
            , ("94208", all_ ["12"])
            , ("7", all_ ["0"])
            , ("-4", all_ ["2"])
            ]
    describe "q74273: Output all strings" $ do
        specEval
            "Ňŧ"
            [("\"ab\"", truncate_ ["[]", "a", "b", "aa", "ab", "ba", "bb", "aaa", "aab", "aba", "abb"])]
    describe "q77270: Greatest Common Divisor" $ do
        specEval
            "G"
            [ ("0 2", all_ ["2"])
            , ("6 0", all_ ["6"])
            , ("30 42", all_ ["6"])
            , ("15 14", all_ ["1"])
            , ("7 7", all_ ["7"])
            , ("69 25", all_ ["1"])
            , ("21 12", all_ ["3"])
            , ("169 123", all_ ["1"])
            , ("20 142", all_ ["2"])
            , ("101 202", all_ ["101"])
            ]
        specEval
            "ʷ{$Zᵉ%"
            [ ("0 2", all_ ["2"])
            , ("6 0", all_ ["6"])
            , ("30 42", all_ ["6"])
            , ("15 14", all_ ["1"])
            , ("7 7", all_ ["7"])
            , ("69 25", all_ ["1"])
            , ("21 12", all_ ["3"])
            , ("169 123", all_ ["1"])
            , ("20 142", all_ ["2"])
            , ("101 202", all_ ["101"])
            ]
        specEval
            "ṀRᶠ¦Ṁ"
            [ ("[0,2]", all_ ["2"])
            , ("[6,0]", all_ ["6"])
            , ("[30,42]", all_ ["6"])
            , ("[15,14]", all_ ["1"])
            , ("[7,7]", all_ ["7"])
            , ("[69,25]", all_ ["1"])
            , ("[21,12]", all_ ["3"])
            , ("[169,123]", all_ ["1"])
            , ("[20,142]", all_ ["2"])
            , ("[101,202]", all_ ["101"])
            ]
    describe "q77608: All together now" $ do
        specEval
            "Y$ů"
            [ ("[3]", Check True)
            , ("[5,1]", Check True)
            , ("[4,4,9,9,9,9,1,1]", Check True)
            , ("[1,2,3,4,5,6,7,8,9]", Check True)
            , ("[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]", Check True)
            , ("[8,1,8]", Check False)
            , ("[8,8,8,4,4,4,3,3,3,3,3,3,4]", Check False)
            , ("[4,5,4,5]", Check False)
            , ("[5,5,4,5,5,3]", Check False)
            , ("[1,2,3,4,5,6,7,8,9,1]", Check False)
            ]
    describe "q79037: Smallest groups in an array" $ do
        specEval
            "ĉş"
            [ ("[1,1,1,2,2,1,1,1,1,2,2,2,1,1,1]", all_ ["[2,2]"])
            , ("[3,3,3,4,4,4,4,5,5,4,4,3,3,4,4]", all_ ["[5,5]", "[4,4]", "[3,3]", "[4,4]"])
            , ("[1,1,2,2,3,3,4]", all_ ["[4]"])
            , ("[1]", all_ ["[1]"])
            , ("[1,1,10,10,10,100,100]", all_ ["[1,1]", "[100,100]"])
            ]
    describe "q79483: Continued Fraction of a Rational Number" $ do
        specEval
            "ᶦ{1%ŗ}k"
            [ ("860438", all_ ["860438"])
            , ("3245/1000", all_ ["3", "4", "12", "4"])
            , ("-42/10", all_ ["-5", "1", "4"])
            , ("-1147802/10000", all_ ["-115", "4", "1", "1", "4", "1", "1", "5", "1", "1", "4"])
            , ("0/11", all_ ["0"])
            , ("1/42", all_ ["0", "42"])
            , ("2/7", all_ ["0", "3", "2"])
            , ("-18/17056", all_ ["-1", "1", "946", "1", "1", "4"])
            , ("-17056/18", all_ ["-948", "2", "4"])
            ]
    describe "q82497: Pseudofactorial" $ do
        specEval
            "Rʳg"
            [ ("1", all_ ["1"])
            , ("2", all_ ["2"])
            , ("3", all_ ["6"])
            , ("4", all_ ["12"])
            , ("5", all_ ["60"])
            , ("6", all_ ["60"])
            , ("7", all_ ["420"])
            ]
    describe "q82604: Approximation of e" $ do
        specEval
            "rFŗ∑"
            [ ("1", all_ ["1"])
            , ("2", all_ ["2"])
            , ("3", all_ ["5/2"])
            , ("4", all_ ["8/3"])
            , ("5", all_ ["65/24"])
            , ("6", all_ ["163/60"])
            , ("7", all_ ["1957/720"])
            , ("8", all_ ["685/252"])
            , ("9", all_ ["109601/40320"])
            , ("10", all_ ["98641/36288"])
            ]
    describe "q83377: Write a program to elasticize strings" $ do
        specEval
            "#Ry"
            [ ("\"Why\"", all_ ["Whhyyy"])
            , ("\"SKype\"", all_ ["SKKyyyppppeeeee"])
            , ("\"LobbY\"", all_ ["LoobbbbbbbYYYYY"])
            , ("\"A and B\"", all_ ["A  aaannnnddddd      BBBBBBB"])
            ]
    describe "q83533: Calculate Euler's totient function" $ do
        specEval
            "RG1Ĉ"
            [ ("1", all_ ["1"])
            , ("2", all_ ["1"])
            , ("3", all_ ["2"])
            , ("8", all_ ["4"])
            , ("9", all_ ["6"])
            , ("26", all_ ["12"])
            , ("44", all_ ["20"])
            , ("105", all_ ["48"])
            ]
    describe "q84519: Implement Takewhile" $ do
        specEval
            "Ö∫¬∑T"
            [ ("[14,42,2324,97090,4080622,171480372]", all_ ["[14,42,2324,97090,4080622,171480372]"])
            , ("[42,14,42,2324]", all_ ["[42,14,42,2324]"])
            , ("[7,14,42]", all_ ["[]"])
            , ("[]", all_ ["[]"])
            , ("[171480372,13,14,42]", all_ ["[171480372]"])
            , ("[42,14,42,43,41,4080622,171480372]", all_ ["[42,14,42]"])
            ]
        specEval
            "pᵖ½al"
            [ ("[14,42,2324,97090,4080622,171480372]", all_ ["[14,42,2324,97090,4080622,171480372]"])
            , ("[42,14,42,2324]", all_ ["[42,14,42,2324]"])
            , ("[7,14,42]", all_ ["[]"])
            , ("[]", all_ ["[]"])
            , ("[171480372,13,14,42]", all_ ["[171480372]"])
            , ("[42,14,42,43,41,4080622,171480372]", all_ ["[42,14,42]"])
            ]
    describe "q84673: Is it a sum-free set?" $ do
        specEval
            "ᵒ+j∕="
            [ ("[]", Check True)
            , ("[4]", Check True)
            , ("[1,5,7]", Check True)
            , ("[16,1,4,9]", Check True)
            , ("[0]", Check False)
            , ("[1,4,5,7]", Check False)
            , ("[3,0]", Check False)
            , ("[16,1,4,8]", Check False)
            ]
    describe "q85994: Count of \"a\"s and \"b\"s must be equal. Did you get it computer?" $ do
        specEval
            "N;<"
            [ ("\"ab\"", Check True)
            , ("\"aaabbb\"", Check True)
            , ("\"aaaaabbbbb\"", Check True)
            , ("\"\"", Check False)
            , ("\"b\"", Check False)
            , ("\"ba\"", Check False)
            , ("\"aab\"", Check False)
            , ("\"aaba\"", Check False)
            , ("\"abbb\"", Check False)
            , ("\"bbaa\"", Check False)
            , ("\"bbbb\"", Check False)
            ]
    describe "q91154: Sylvester's sequence" $ do
        specEval
            "2ᶦ{:←*→"
            [("", truncate_ ["2", "3", "7", "43", "1807", "3263443", "10650056950807"])]
    describe "q91420: Excessive Integers" $ do
        specEval
            "ƒ←∑"
            [ ("1", all_ ["0"])
            , ("2", all_ ["0"])
            , ("3", all_ ["0"])
            , ("4", all_ ["1"])
            , ("5", all_ ["0"])
            , ("6", all_ ["0"])
            , ("7", all_ ["0"])
            , ("8", all_ ["2"])
            , ("9", all_ ["1"])
            , ("10", all_ ["0"])
            , ("11", all_ ["0"])
            , ("12", all_ ["1"])
            , ("13", all_ ["0"])
            , ("14", all_ ["0"])
            , ("15", all_ ["0"])
            ]
    describe "q93261: StringgnirtSStringgnirtSStringgnirtS" $ do
        specEval
            "xᵐᵑ↔j"
            [ ("\"a\"", all_ ["a"])
            , ("\"abcd\"", all_ ["abcddcbaabcddcba"])
            , ("\"OK!\"", all_ ["OK!!KOOK!"])
            , ("\"4815162342\"", all_ ["4815162342243261518448151623422432615184481516234224326151844815162342243261518448151623422432615184"])
            , ("\"PPCG\"", all_ ["PPCGGCPPPPCGGCPP"])
            , ("\"42\"", all_ ["4224"])
            ]
    describe "q94028: Find the largest number that's adjacent to a zero" $ do
        specEval
            "qᵗZđ+Å"
            [ ("[1,4,3,6,0,3,7,0]", all_ ["7"])
            , ("[9,4,9,0,9,0,9,15,-2]", all_ ["9"])
            , ("[-4,-6,-2,0,-9]", all_ ["-2"])
            , ("[-11,0,0,0,0,0,-12,10]", all_ ["0"])
            , ("[0,20]", all_ ["20"])
            ]
    describe "q94291: Is it a balanced number?" $ do
        specEval
            "Ɗ;ᶜtᶻ-∑ž"
            [ ("1", Check True)
            , ("6", Check True)
            , ("11", Check True)
            , ("141", Check True)
            , ("1221", Check True)
            , ("23281453796004414", Check True)
            , ("523428121656666655655556655656502809745249552466339089702361716477983610754966885128041975406005088", Check True)
            , ("10", Check False)
            , ("12", Check False)
            , ("110", Check False)
            , ("15421", Check False)
            , ("5234095123508321", Check False)
            , ("6240911314399072459493765661191058613491863144152352262897351988250431140546660035648795316740212454", Check False)
            ]
        specEval
            "Ɗ:Ĭ;ÐŤ∑≡"
            [ ("1", Check True)
            , ("6", Check True)
            , ("11", Check True)
            , ("141", Check True)
            , ("1221", Check True)
            , ("23281453796004414", Check True)
            , ("523428121656666655655556655656502809745249552466339089702361716477983610754966885128041975406005088", Check True)
            , ("10", Check False)
            , ("12", Check False)
            , ("110", Check False)
            , ("15421", Check False)
            , ("5234095123508321", Check False)
            , ("6240911314399072459493765661191058613491863144152352262897351988250431140546660035648795316740212454", Check False)
            ]
        specEval
            "Ɗx:µ-±∙ž"
            [ ("1", Check True)
            , ("6", Check True)
            , ("11", Check True)
            , ("141", Check True)
            , ("1221", Check True)
            , ("23281453796004414", Check True)
            , ("523428121656666655655556655656502809745249552466339089702361716477983610754966885128041975406005088", Check True)
            , ("10", Check False)
            , ("12", Check False)
            , ("110", Check False)
            , ("15421", Check False)
            , ("5234095123508321", Check False)
            , ("6240911314399072459493765661191058613491863144152352262897351988250431140546660035648795316740212454", Check False)
            ]
    describe "q94348: Prime counting function" $ do
        specEval
            "ƥ"
            [ ("1", all_ ["0"])
            , ("2", all_ ["1"])
            , ("5", all_ ["3"])
            ]
        specEval
            "Fƒ#"
            [ ("1", all_ ["0"])
            , ("2", all_ ["1"])
            , ("5", all_ ["3"])
            ]
        specEval
            "R~Q"
            [ ("1", Count 0)
            , ("2", Count 1)
            , ("5", Count 3)
            ]
    describe "q94999: Least Common Multiple" $ do
        specEval
            "ʳg"
            [ ("[7,2]", all_ ["14"])
            , ("[8,1]", all_ ["8"])
            , ("[6,4,8]", all_ ["24"])
            , ("[8,2,1,10]", all_ ["40"])
            , ("[9,6,2,1,5]", all_ ["90"])
            , ("[5,5,7,1,1]", all_ ["35"])
            , ("[4,13,8,8,11,1]", all_ ["1144"])
            , ("[7,2,2,11,11,8,5]", all_ ["3080"])
            , ("[1,6,10,3,4,10,7]", all_ ["420"])
            , ("[5,2,9,10,3,4,4,4,7]", all_ ["1260"])
            , ("[9,7,10,9,7,8,5,10,1]", all_ ["2520"])
            ]
    describe "q95409: 2048-like array shift" $ do
        specEval
            "ĉʲ{ĭ+↔"
            [ ("[]", all_ ["[]"])
            , ("[2,2,4,4]", all_ ["[4,8]"])
            , ("[2,2,2,4,4,8]", all_ ["[2,4,8,8]"])
            , ("[2,2,2,2]", all_ ["[4,4]"])
            , ("[4,4,2,8,8,2]", all_ ["[8,2,16,2]"])
            , ("[1024,1024,512,512,256,256]", all_ ["[2048,1024,512]"])
            , ("[3,3,3,1,1,7,5,5,5,5]", all_ ["[3,6,2,7,10,10]"])
            ]
    describe "q96923: Find the maximum deviation" $ do
        specEval
            "q$Lɱ≈Å"
            [("[6,9,4,7,4,1] 3", all_ ["6"])]
    describe "q98730: Count trailing truths" $ do
        specEval
            "sNP"
            [ ("[]", Count 0)
            , ("[0]", Count 0)
            , ("[1]", Count 1)
            , ("[0,1,1,0,0]", Count 0)
            , ("[1,1,1,0,1]", Count 1)
            , ("[1,1,0,1,1]", Count 2)
            , ("[0,0,1,1,1]", Count 3)
            , ("[1,1,1,1,1,1]", Count 6)
            ]
        specEval
            "çĉl∑"
            [ ("[]", all_ ["0"])
            , ("[0]", all_ ["0"])
            , ("[1]", all_ ["1"])
            , ("[0,1,1,0,0]", all_ ["0"])
            , ("[1,1,1,0,1]", all_ ["1"])
            , ("[1,1,0,1,1]", all_ ["2"])
            , ("[0,0,1,1,1]", all_ ["3"])
            , ("[1,1,1,1,1,1]", all_ ["6"])
            ]
    describe "q100205: Do Matrix Multiplication!" $ do
        specEval
            "ᵐ∙"
            [ ("[[1,2],[3,4],[5,6]] [[1,2,3,4,5],[6,7,8,9,10]]", all_ ["[[13,16,19,22,25],[27,34,41,48,55],[41,52,63,74,85]]"])
            , ("[[2,3],[3,4]] [[3,5],[3,1]]", all_ ["[[15,13],[21,19]]"])
            , ("[[5,3],[1,3],[9,3],[1,-1000]] [[1,3],[2,4]]", all_ ["[[11,27],[7,15],[15,39],[-1999,-3997]]"])
            ]
        specEval
            "ᵐ*Ŝ"
            [ ("[[1,2],[3,4],[5,6]] [[1,2,3,4,5],[6,7,8,9,10]]", all_ ["[[13,16,19,22,25],[27,34,41,48,55],[41,52,63,74,85]]"])
            , ("[[2,3],[3,4]] [[3,5],[3,1]]", all_ ["[[15,13],[21,19]]"])
            , ("[[5,3],[1,3],[9,3],[1,-1000]] [[1,3],[2,4]]", all_ ["[[11,27],[7,15],[15,39],[-1999,-3997]]"])
            ]
    describe "q101389: Increment an Array" $ do
        specEval
            "Ṁ←ɔõhĦ+"
            [ ("[1]", all_ ["[1,1]"])
            , ("[2]", all_ ["[2,1]"])
            , ("[1,1]", all_ ["[1,1,1]"])
            , ("[3,3,3,3,3]", all_ ["[3,3,3,3,3,1]"])
            , ("[1,2]", all_ ["[2,2]"])
            , ("[2,1]", all_ ["[2,2]"])
            , ("[3,1,1]", all_ ["[3,2,1]"])
            , ("[3,4,9,3]", all_ ["[4,4,9,3]"])
            ]
    describe "q103624: Find the sum of all numbers below n that are a multiple of some set of numbers" $ do
        specEval
            "ᵒ%ᵐ∏¬x∙"
            [ ("[2] 50", all_ ["600"])
            , ("[3,5] 10", all_ ["23"])
            , ("[4,2] 28", all_ ["182"])
            , ("[7,5] 19", all_ ["51"])
            , ("[2,3,5] 50", all_ ["857"])
            ]
        specEval
            "ᶠ{$~¦}∑"
            [ ("50 [2]", first_ "600")
            , ("10 [3,5]", first_ "23")
            , ("28 [4,2]", first_ "182")
            , ("19 [7,5]", first_ "51")
            , ("50 [2,3,5]", first_ "857")
            ]
    describe "q103756: Big numbers: Ultrafactorials" $ do
        specEval
            "→rF:E∑"
            [ ("0", all_ ["1"])
            , ("1", all_ ["2"])
            , ("2", all_ ["6"])
            , ("3", all_ ["46662"])
            ]
    describe "q104665: Coprimes up to N" $ do
        specEval
            "rG1Ĩ"
            [ ("2", all_ ["1"])
            , ("3", all_ ["1", "2"])
            , ("6", all_ ["1", "5"])
            , ("10", all_ ["1", "3", "7", "9"])
            , ("20", all_ ["1", "3", "7", "9", "11", "13", "17", "19"])
            , ("25", all_ ["1", "2", "3", "4", "6", "7", "8", "9", "11", "12", "13", "14", "16", "17", "18", "19", "21", "22", "23", "24"])
            , ("30", all_ ["1", "7", "11", "13", "17", "19", "23", "29"])
            ]
    describe "q105861: Can this number be written in (3^x) - 1 format?" $ do
        specEval
            "3D←P∑"
            [ ("2", all_ ["1"])
            , ("26", all_ ["3"])
            , ("1024", all_ [])
            ]
    describe "q106149: Compute the Median" $ do
        specEval
            ",o;↔ᶻÐlµ"
            [ ("[1,2,3,4,5,6,7,8,9]", all_ ["5"])
            , ("[1,4,3,2]", all_ ["5/2"])
            , ("[3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,-5,100000,13/10,7/5]", all_ ["3/2"])
            , ("[3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,-5,100000,13/10,7/5]", all_ ["3/2"])
            ]
    describe "q106656: Bit run rundown" $ do
        specEval
            "ƂYṀ"
            [ ("6", all_ ["2"])
            , ("16", all_ ["4"])
            , ("893", all_ ["5"])
            , ("1337371", all_ ["6"])
            , ("1", all_ ["1"])
            , ("9965546", all_ ["7"])
            ]
    describe "q108675: Is this word Lexically Ordered?" $ do
        specEval
            "oᶜ↔="
            [ ("\"ABCDEF\"", Check True)
            , ("\"ZYX\"", Check True)
            , ("\"no\"", Check True)
            , ("\"tree\"", Check True)
            , ("\"q\"", Check True)
            , ("\"ABCDC\"", Check False)
            , ("\"yes\"", Check False)
            , ("\"deed\"", Check False)
            ]
    describe "q111678: N-dimensional N^N array filled with N" $ do
        specEval
            "ᵑᵉř^"
            [ ("1", all_ ["[1]"])
            , ("2", all_ ["[[2,2],[2,2]]"])
            , ("3", all_ ["[[[3,3,3],[3,3,3],[3,3,3]],[[3,3,3],[3,3,3],[3,3,3]],[[3,3,3],[3,3,3],[3,3,3]]]"])
            ]
    describe "q113238: Is it true? Ask Jelly!" $ do
        specEval
            "NZ"
            [ ("[1]", Check True)
            , ("[10]", Check True)
            , ("[[]]", Check True)
            , ("[[[[1]]]]", Check True)
            , ("[[],[1],[1,2]]", Check True)
            , ("[[1],[1,[2]],[1,[2,[3]]]]", Check True)
            , ("[[8],[8,[9]],[8,[9,[10]]]]", Check True)
            , ("[]", Check False)
            , ("[0]", Check False)
            , ("[0,-1]", Check False)
            , ("[-1,0]", Check False)
            , ("[[[[0]]]]", Check False)
            , ("[[0],[1,2],[3,4,5]]", Check False)
            , ("[[8],[8,[9]],[8,[9,[1,0]]]]", Check False)
            , ("[-1,0,0,0]", Check False)
            ]
    describe "q117774: Moving modest minimum" $ do
        specEval
            "ĕ^ṁ"
            [ ("[4,3,2,5]", all_ ["2", "2", "3", "2"])
            , ("[4,2,2,5]", all_ ["2", "2", "2", "2"])
            , ("[6,3,5,5,8]", all_ ["3", "5", "3", "3", "3"])
            , ("[7,1]", all_ ["1", "7"])
            , ("[9,9]", all_ ["9", "9"])
            , ("[9,8,9]", all_ ["8", "9", "8"])
            ]
    describe "q118444: Stay away from zero" $ do
        specEval
            "1M"
            [ ("0", all_ ["1"])
            , ("1", all_ ["1"])
            , ("2", all_ ["2"])
            , ("3", all_ ["3"])
            , ("4", all_ ["4"])
            , ("5", all_ ["5"])
            , ("6", all_ ["6"])
            , ("7", all_ ["7"])
            ]
    describe "q118597: Halve the falses" $ do
        specEval
            "ĭZĬ‼"
            [ ("[1,0,0,1,0,0,1]", all_ ["[1,0,1,0,1]"])
            , ("[1,1,0,0,1,1,0,0,1]", all_ ["[1,1,0,1,1,0,1]"])
            , ("[1,1,0,0,1,1,1,0,0,1,1]", all_ ["[1,1,0,1,1,1,0,1,1]"])
            , ("[1,1,1]", all_ ["[1,1,1]"])
            , ("[0,0,1]", all_ ["[0,1]"])
            , ("[0,0]", all_ ["[0]"])
            , ("[1,1,1,0,0,0,0,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,1,1,0,0,1,0,0]", all_ ["[1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0]"])
            ]
        specEval
            "¬∫½ḟ"
            [ ("[1,0,0,1,0,0,1]", all_ ["[1,0,1,0,1]"])
            , ("[1,1,0,0,1,1,0,0,1]", all_ ["[1,1,0,1,1,0,1]"])
            , ("[1,1,0,0,1,1,1,0,0,1,1]", all_ ["[1,1,0,1,1,1,0,1,1]"])
            , ("[1,1,1]", all_ ["[1,1,1]"])
            , ("[0,0,1]", all_ ["[0,1]"])
            , ("[0,0]", all_ ["[0]"])
            , ("[1,1,1,0,0,0,0,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,1,1,0,0,1,0,0]", all_ ["[1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0]"])
            ]
    describe "q118960: Is this a function?" $ do
        specEval
            "uᵐhů"
            [ ("[[3,5],[3,5],[6,4],[4,4]]", Check True)
            , ("[[9,4],[1,4],[2,4]]", Check True)
            , ("[]", Check True)
            , ("[[1,1]]", Check True)
            , ("[[1,2],[2,1]]", Check True)
            , ("[[3,1],[2,5],[3,6]]", Check False)
            , ("[[1,2],[2,1],[5,2],[1,2],[2,5]]", Check False)
            , ("[[8,8],[8,8],[8,9],[8,9]]", Check False)
            , ("[[1,2],[1,3],[1,4]]", Check False)
            , ("[[1,2],[1,3],[2,3],[2,4]]", Check False)
            ]
    describe "q118982: Replace twos with threes" $ do
        specEval
            "ƒy1|∏"
            [ ("1", all_ ["1"])
            , ("2", all_ ["3"])
            , ("3", all_ ["3"])
            , ("4", all_ ["9"])
            , ("5", all_ ["5"])
            , ("6", all_ ["9"])
            , ("7", all_ ["7"])
            , ("8", all_ ["27"])
            , ("9", all_ ["9"])
            , ("10", all_ ["15"])
            ]
    describe "q119854: Raise integer x to power x, without exponentiation built-ins" $ do
        specEval
            "ř∏"
            [ ("2", all_ ["4"])
            , ("3", all_ ["27"])
            , ("5", all_ ["3125"])
            , ("6", all_ ["46656"])
            , ("10", all_ ["10000000000"])
            ]
    describe "q120350: Determine if an Array contains something other than 2" $ do
        specEval
            "2-ž"
            [ ("[2]", Check True)
            , ("[2,2]", Check True)
            , ("[[2],[2,2],2]", Check True)
            , ("[]", Check True)
            , ("[[],[]]", Check True)
            , ("[1]", Check False)
            , ("[22]", Check False)
            , ("[2,2,2,1]", Check False)
            , ("[[1,2],2]", Check False)
            ]
    describe "q122087: Is this number triangular?" $ do
        specEval
            "8*→√"
            [ ("1", Check True)
            , ("3", Check True)
            , ("6", Check True)
            , ("10", Check True)
            , ("500500", Check True)
            , ("998991", Check True)
            , ("2", Check False)
            , ("4", Check False)
            , ("5", Check False)
            , ("7", Check False)
            , ("501500", Check False)
            , ("999999", Check False)
            ]
        specEval
            "R∫ē"
            [ ("1", Check True)
            , ("3", Check True)
            , ("6", Check True)
            , ("10", Check True)
            , ("18915", Check True)
            , ("71253", Check True)
            , ("2", Check False)
            , ("4", Check False)
            , ("5", Check False)
            , ("7", Check False)
            , ("4576", Check False)
            , ("31988", Check False)
            ]
    describe "q122520: Is this relationship creepy?" $ do
        specEval
            "Ṁä7+≥"
            [ ("[40,40]", Check True)
            , ("[18,21]", Check True)
            , ("[80,32]", Check False)
            , ("[15,50]", Check False)
            , ("[47,10000]", Check False)
            , ("[37,38]", Check True)
            , ("[22,18]", Check True)
            ]
    describe "q125104: Cartesian product of two lists" $ do
        specEval
            "ᵐ~"
            [ ("[\"abc\",\"123\"]", all_ ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"])
            , ("[\"aa\",\"aba\"]", all_ ["aa", "ab", "aa", "aa", "ab", "aa"])
            ]
    describe "q126373: Am I a Fibonacci Number?" $ do
        specEval
            "*5*4ŋ≈√"
            [ ("0", Check True)
            , ("1", Check True)
            , ("2", Check True)
            , ("12", Check False)
            ]
    describe "q126699: Create a checkerboard matrix" $ do
        specEval
            "ᵒ+→Ö"
            [ ("1", all_ ["[[1]]"])
            , ("2", all_ ["[[1,0],[0,1]]"])
            , ("3", all_ ["[[1,0,1],[0,1,0],[1,0,1]]"])
            , ("4", all_ ["[[1,0,1,0],[0,1,0,1],[1,0,1,0],[0,1,0,1]]"])
            ]
    describe "q129773: Is it a Lynch-Bell number?" $ do
        specEval
            "Ɗ¦ů"
            [ ("7", Check True)
            , ("126", Check True)
            , ("54", Check False)
            , ("55", Check False)
            , ("3915", Check True)
            ]
    describe "q130390: Is it a super-prime?" $ do
        specEval
            "QƥQ"
            [ ("2", Check False)
            , ("3", Check True)
            , ("4", Check False)
            , ("5", Check True)
            , ("7", Check False)
            , ("11", Check True)
            , ("13", Check False)
            , ("17", Check True)
            , ("709", Check True)
            , ("851", Check False)
            , ("991", Check True)
            ]
    describe "q130454: Product of Divisors" $ do
        specEval
            "Ď∏"
            [ ("1", all_ ["1"])
            , ("2", all_ ["2"])
            , ("3", all_ ["3"])
            , ("4", all_ ["8"])
            , ("5", all_ ["5"])
            , ("6", all_ ["36"])
            , ("7", all_ ["7"])
            , ("8", all_ ["64"])
            , ("9", all_ ["27"])
            , ("10", all_ ["100"])
            , ("12", all_ ["1728"])
            , ("14", all_ ["196"])
            , ("24", all_ ["331776"])
            , ("25", all_ ["125"])
            , ("28", all_ ["21952"])
            , ("30", all_ ["810000"])
            ]
    describe "q132379: Output the n-th Bell Number" $ do
        specEval
            "O"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 5)
            , ("4", Count 15)
            , ("5", Count 52)
            , ("6", Count 203)
            , ("7", Count 877)
            ]
    describe "q136713: Find the first duplicated element" $ do
        specEval
            "pƆᵗf"
            [ ("[2,3,3,1,5,2]", first_ "3")
            , ("[2,4,3,5,1]", nothing_)
            ]
    describe "q136887: Fold a List in Half" $ do
        specEval
            ";↔Ĭĭ+"
            [ ("[1,2,3,4,5,6,7,8]", all_ ["[9,9,9,9]"])
            , ("[1,2,3,4,5,6,7]", all_ ["[8,8,8,4]"])
            ]
    describe "q138510: Running second maximum of a list" $ do
        specEval
            "poil"
            [ ("[1,5,2,3,5,9,5,8]", all_ ["1", "2", "3", "5", "5", "5", "8"])
            , ("[1,1,2,2,3,3,4]", all_ ["1", "1", "2", "2", "3", "3"])
            , ("[2,1,0,-1,0,1,2]", all_ ["1", "1", "1", "1", "1", "2"])
            ]
    describe "q138982: Divisibility Streak" $ do
        specEval
            "ᵏ{ᵉ+→%Z"
            [ ("2", all_ ["1"])
            , ("3", all_ ["2"])
            , ("4", all_ ["1"])
            , ("5", all_ ["2"])
            , ("6", all_ ["1"])
            , ("7", all_ ["3"])
            , ("8", all_ ["1"])
            , ("9", all_ ["2"])
            , ("10", all_ ["1"])
            , ("2521", all_ ["10"])
            ]
    describe "q139804: Undo a Range of Numbers" $ do
        specEval
            "ᵏ{ʲĝ="
            [ ("\"0123\"", all_ ["4"])
            , ("\"0\"", all_ ["1"])
            , ("\"012345678910111213141516171819202122232425262728293031323334353637383940\"", all_ ["41"])
            ]
    describe "q141949: Count edits accounting for grace period" $ do
        specEval
            "ˡ{C4+>‼"
            [ ("[0]", all_ ["1"])
            , ("[0,3,5,7]", all_ ["2"])
            , ("[0,3,4,7,9,10,11,12]", all_ ["3"])
            , ("[0,30,120]", all_ ["3"])
            , ("[0,4,8,12,16]", all_ ["3"])
            , ("[0,4,8,12,16,20]", all_ ["3"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]", all_ ["4"])
            , ("[0,5,10,15,20]", all_ ["5"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]", all_ ["5"])
            , ("[0,1,4,5,9,11,12,14,16,18,23,24,26,28,29,30]", all_ ["6"])
            ]
    describe "q142037: Matrix with 1 to L(n), in all n columns" $ do
        specEval
            "RḞŤ"
            [ ("[3,5,2,1,6]", all_ ["[[1,1,1,1,1],[2,2,2,0,2],[3,3,0,0,3],[0,4,0,0,4],[0,5,0,0,5],[0,0,0,0,6]]"])
            , ("[1]", all_ ["[[1]]"])
            , ("[1,2,3,4,3,2,1]", all_ ["[[1,1,1,1,1,1,1],[0,2,2,2,2,2,0],[0,0,3,3,3,0,0],[0,0,0,4,0,0,0]]"])
            ]
    describe "q142071: Find the sum of the divisors of N" $ do
        specEval
            "Ď∑"
            [ ("7", all_ ["8"])
            , ("15", all_ ["24"])
            , ("20", all_ ["42"])
            , ("1", all_ ["1"])
            , ("5", all_ ["6"])
            ]
    describe "q142534: Is it a completely even number?" $ do
        specEval
            "←Ƃ≡"
            [ ("2", Check True)
            , ("4", Check True)
            , ("16", Check True)
            , ("128", Check True)
            , ("10", Check False)
            , ("12", Check False)
            , ("14", Check False)
            , ("18", Check False)
            , ("1", Check False)
            ]
        specEval
            "←X<"
            [ ("2", Check True)
            , ("4", Check True)
            , ("16", Check True)
            , ("128", Check True)
            , ("10", Check False)
            , ("12", Check False)
            , ("14", Check False)
            , ("18", Check False)
            , ("1", Check False)
            ]
    describe "q143278: Am I an insignificant array?" $ do
        specEval
            "∆ƶ"
            [ ("[1,2,3,4,3,4,5,5,5,4]", Check True)
            , ("[1,2,3,4,5,6,7,8,9,8]", Check True)
            , ("[3,3,3,3,3,3,3]", Check True)
            , ("[3,4,4,4,3,3,3,4,4,4]", Check True)
            , ("[1,2,3,4]", Check True)
            , ("[5,4,3,2]", Check True)
            , ("[1,3,5,7,9,7,5,3,1]", Check False)
            , ("[1,1,1,2,3,4,5,6,19]", Check False)
            , ("[3,4,5,6,7,8,7,5]", Check False)
            , ("[1,2,4,10,18,10,100]", Check False)
            , ("[10,20,30,30,30]", Check False)
            ]
    describe "q144201: Stackable sequences" $ do
        specEval
            "Oᵐ{x="
            [ ("[0]", Check True)
            , ("[0,1]", Check True)
            , ("[0,1,2,3,4]", Check True)
            , ("[0,0,0,1,1,1,2,2,2,3,4,5,6,7,8,9,0]", Check True)
            , ("[0,1,2,0,3,1]", Check True)
            , ("[0,1,2,0,3,0,4,5,1,1,6,2,7,3,2,8,3,9,0]", Check True)
            , ("[1]", Check False)
            , ("[0,2,1]", Check False)
            , ("[0,0,0,1,1,1,1]", Check False)
            , ("[0,0,1,2,3,1,2,4,2,5]", Check False)
            , ("[0,1,2,3,0,1,2,1,0]", Check False)
            , ("[0,0,0,1,1,2,2,2,3]", Check False)
            ]
    describe "q144233: How many Wazirs can be placed on an N×N Chessboard?" $ do
        specEval
            "*äK"
            [ ("7", all_ ["25"])
            , ("8", all_ ["32"])
            , ("100", all_ ["5000"])
            ]
    describe "q145518: Square pyramidal numbers" $ do
        specEval
            "R:∙"
            [ ("0", all_ ["0"])
            , ("4", all_ ["30"])
            , ("5", all_ ["55"])
            ]
    describe "q146059: Is my triangle right?" $ do
        specEval
            "ʰ_∙ž"
            [ ("[5,3,4]", Check True)
            , ("[3,5,4]", Check True)
            , ("[12,37,35]", Check True)
            , ("[21,38,50]", Check False)
            , ("[210,308,250]", Check False)
            ]
    describe "q149890: Visible Dice Faces" $ do
        specEval
            "ᵒ+7f"
            [ ("[6]", Check True)
            , ("[6,2]", Check True)
            , ("[1,3]", Check True)
            , ("[2,1,3]", Check True)
            , ("[3,2,6]", Check True)
            , ("[1,6]", Check False)
            , ("[5,4,2]", Check False)
            , ("[3,1,4]", Check False)
            , ("[5,4,6,2]", Check False)
            , ("[1,2,3,4,5,6]", Check False)
            ]
    describe "q150117: Boustrophedonise" $ do
        specEval
            "ĭᵐ↔Ĭ"
            [ ("[\"Here are some lines\",\"of text for you\",\"to make a\",\"boustrophedon\"]", all_ ["[\"Here are some lines\",\"uoy rof txet fo\",\"to make a\",\"nodehportsuob\"]"])
            , ("[\"My boustrophedon\"]", all_ ["[\"My boustrophedon\"]"])
            , ("[]", all_ ["[]"])
            , ("[\"Some text\",[],\"More text\",[],[],\"Last bit of text\"]", all_ ["[\"Some text\",[],\"More text\",[],[],\"txet fo tib tsaL\"]"])
            ]
    describe "q152114: Output the hours at 90 degrees" $ do
        specEval
            "258Ɗ+12%→"
            [ ("1", all_ ["[4,7,10]"])
            , ("2", all_ ["[5,8,11]"])
            , ("3", all_ ["[6,9,12]"])
            , ("4", all_ ["[7,10,1]"])
            , ("5", all_ ["[8,11,2]"])
            , ("6", all_ ["[9,12,3]"])
            , ("7", all_ ["[10,1,4]"])
            , ("8", all_ ["[11,2,5]"])
            , ("9", all_ ["[12,3,6]"])
            , ("10", all_ ["[1,4,7]"])
            , ("11", all_ ["[2,5,8]"])
            , ("12", all_ ["[3,6,9]"])
            ]
        specEval
            "12Rᶠ{-Z3¦"
            [ ("1", all_ ["[4,7,10]"])
            , ("2", all_ ["[5,8,11]"])
            , ("3", all_ ["[6,9,12]"])
            , ("4", all_ ["[1,7,10]"])
            , ("5", all_ ["[2,8,11]"])
            , ("6", all_ ["[3,9,12]"])
            , ("7", all_ ["[1,4,10]"])
            , ("8", all_ ["[2,5,11]"])
            , ("9", all_ ["[3,6,12]"])
            , ("10", all_ ["[1,4,7]"])
            , ("11", all_ ["[2,5,8]"])
            , ("12", all_ ["[3,6,9]"])
            ]
    describe "q153221: Generate combinations that add up to a target value" $ do
        specEval
            "xSᵖ{@∑="
            [ ("[1,2,1,5] 8", all_ ["[0,1,3]", "[1,2,3]"])
            , ("[48/10,95/10,27/10,11/12,10] 148/10", all_ ["[0,4]"])
            , ("[7,8,9,-10,20,27] 17", all_ ["[1,2]", "[0,3,4]", "[3,5]"])
            , ("[1,2,3] 7", all_ [])
            ]
    describe "q153783: The first n numbers without consecutive equal binary digits" $ do
        specEval
            "RË3÷"
            [ ("1", all_ ["[0]"])
            , ("18", all_ ["[0,1,2,5,10,21,42,85,170,341,682,1365,2730,5461,10922,21845,43690,87381]"])
            ]
    describe "q154363: Check if all non-zero elements in a matrix are connected" $ do
        specEval
            "ᵗ{±1Ĩ$þÐaOđᵒ{≈∑Ƶ"
            [ ("[0] 1", Check True)
            , ("[0,0] 2", Check True)
            , ("[1,1,1,0,0,0] 3", Check True)
            , ("[1,0,0,1,1,1,0,0,1] 3", Check True)
            , ("[0,1,1,0] 2", Check False)
            , ("[1,1,1,0,0,0,0,2,0,0,0,5] 4", Check False)
            , ("[0,0,5,2,1,2,0,0,5,3,2,1,5,7,3,2] 4", Check False)
            ]
    describe "q154553: Column-wise summation of overlapping slices" $ do
        specEval
            "xq§LaĦ*"
            [ ("[1,3,12,100,23] 4", all_ ["[1,6,24,200,23]"])
            , ("[3,-6,-9,19,2,0] 2", all_ ["[3,-12,-18,38,4,0]"])
            , ("[5,6,7,8,2,-4,7] 3", all_ ["[5,12,21,24,6,-8,7]"])
            , ("[1,2,3,4,5,6,7,8,9] 3", all_ ["[1,4,9,12,15,18,21,16,9]"])
            , ("[1,1,1,1,1,1,1] 6", all_ ["[1,2,2,2,2,2,1]"])
            , ("[1,2,3,4,5,6,7,8,9] 6", all_ ["[1,4,9,16,20,24,21,16,9]"])
            ]
    describe "q156602: Quickly regrouping lists" $ do
        specEval
            "ˡ{Łĉᵐ#"
            [ ("[1,2,3,3,2,1]", all_ ["4"])
            , ("[1,2,3,4,5,6,7]", all_ ["2"])
            , ("[1,1,1,1,1,1]", all_ ["1"])
            , ("[2]", all_ ["0"])
            , ("[1,2,4]", all_ ["2"])
            , ("[1,2,2,1,1,2]", all_ ["4"])
            , ("[1,2,2,1,1,2,1,2,2]", all_ ["5"])
            , ("[1]", all_ ["0"])
            ]
    describe "q162254: Generate a Walsh Matrix" $ do
        specEval
            "Ë:ᵒ&Þ£E"
            [ ("0", all_ ["[[1]]"])
            , ("1", all_ ["[[1,1],[1,-1]]"])
            , ("2", all_ ["[[1,1,1,1],[1,-1,1,-1],[1,1,-1,-1],[1,-1,-1,1]]"])
            ]
    describe "q164509: Sign-Swapping Sums" $ do
        specEval
            "ŋ∑ũ"
            [ ("[1,2]", Count 4)
            , ("[1,2,2]", Count 6)
            , ("[1,1,1]", Count 4)
            , ("[1,2,27]", Count 8)
            , ("[1,2,3,4,5,6,7]", Count 29)
            , ("[3,1,4,1,5,9,2,6,5,3,5]", Count 45)
            , ("[1,7,2,8,3,1,6,8,10,9]", Count 56)
            ]
    describe "q167573: Consecutive 1-Bits are Incremented" $ do
        specEval
            "ĉʲ∫"
            [ ("[0,1,1,1,0,1,1,0,0,0,1,1,1,1,1,1]", all_ ["[0,1,2,3,0,1,2,0,0,0,1,2,3,4,5,6]"])
            , ("[0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1]", all_ ["[0,1,2,0,1,0,1,2,3,4,5,0,1,0,1,2,0,1,2,3,4,5,6,0,1,0,1,2,3,4,5,6,7,8]"])
            , ("[1,1,1,1,1,1,1,1,1,1,1,1,0,1]", all_ ["[1,2,3,4,5,6,7,8,9,10,11,12,0,1]"])
            ]
    describe "q169724: Is this number evil?" $ do
        specEval
            "Þ½"
            [ ("3", Check True)
            , ("11", Check False)
            , ("777", Check True)
            , ("43", Check True)
            , ("55", Check False)
            , ("666", Check False)
            ]
    describe "q170047: Most Common Multiple" $ do
        specEval
            "Sđ*aŢṂ"
            [ ("[2,3,4,5,6]", all_ ["12"])
            , ("[7,2]", all_ ["14"])
            , ("[2,3,3]", all_ ["6"])
            , ("[3,3,3]", all_ ["9"])
            , ("[1,1,1,1,2,2]", all_ ["2"])
            , ("[6,200,10,120]", all_ ["1200"])
            , ("[2,3,4,5,6,7,8,8]", all_ ["24"])
            , ("[5,2,9,10,3,4,4,4,7]", all_ ["20"])
            , ("[9,7,10,9,7,8,5,10,1]", all_ ["63", "90", "70"])
            ]
    describe "q170695: Construct a line graph / conjugate graph" $ do
        specEval
            "S:đ∩z¿ᵐj"
            [ ("[]", all_ [])
            , ("[[\"0\",\"1\"]]", all_ [])
            , ("[[\"0\",\"1\"],[\"1\",\"2\"]]", all_ ["[\"01\",\"12\"]"])
            , ("[[\"a\",\"b\"],[\"b\",\"c\"],[\"c\",\"a\"]]", all_ ["[\"ab\",\"bc\"]", "[\"ab\",\"ca\"]", "[\"bc\",\"ca\"]"])
            ]
    describe "q175248: Count the contiguous submatrices" $ do
        specEval
            "qŤqŤ="
            [ ("[[3,1],[1,4]] [[1,4],[3,1]]", Count 0)
            , ("[[3,1,4,0,5],[6,3,1,0,4],[5,6,3,0,1]] [[1,4],[3,1]]", Count 1)
            , ("[[3,1,4,5],[6,3,1,4],[5,6,3,1]] [[1,4],[3,1]]", Count 2)
            , ("[[3,1,4,5],[6,3,1,4],[5,6,3,1]] [[3]]", Count 3)
            , ("[[3,1,3,1,3,1,3,1,3]] [[3,1,3]]", Count 4)
            ]
    describe "q177221: String rotation - output string repeatedly moving first character to the end" $ do
        specEval
            ";$N,"
            [ ("\"john\"", all_ ["ohnj", "hnjo", "njoh", "john"])
            , ("\"heehee\"", all_ ["eeheeh", "eheehe", "heehee", "eeheeh", "eheehe", "heehee"])
            ]
        specEval
            "xŘ↔"
            [ ("\"john\"", all_ ["[\"njoh\",\"hnjo\",\"ohnj\",\"john\"]"])
            , ("\"heehee\"", all_ ["[\"eheehe\",\"eeheeh\",\"heehee\",\"eheehe\",\"eeheeh\",\"heehee\"]"])
            ]
    describe "q178173: The inverse Collatz Conjecture" $ do
        specEval
            "ᶦ{Z:←½$3*→I"
            [ ("0", all_ ["0"])
            , ("1", all_ ["1", "0"])
            , ("2", all_ ["2", "7", "3", "1", "0"])
            , ("3", all_ ["3", "1", "0"])
            , ("10", all_ ["10", "31", "15", "7", "3", "1", "0"])
            , ("14", all_ ["14", "43", "21", "10", "31", "15", "7", "3", "1", "0"])
            ]
    describe "q179464: Covering a Skyline with brush strokes" $ do
        specEval
            "ç-P‼∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", all_ ["9"])
            , ("[5,8]", all_ ["8"])
            , ("[1,1,1,1]", all_ ["1"])
            , ("[]", all_ ["0"])
            , ("[0,0]", all_ ["0"])
            , ("[2]", all_ ["2"])
            , ("[2,0,2]", all_ ["4"])
            , ("[10,9,8,9]", all_ ["11"])
            ]
        specEval
            "çM-_∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", all_ ["9"])
            , ("[5,8]", all_ ["8"])
            , ("[1,1,1,1]", all_ ["1"])
            , ("[]", all_ ["0"])
            , ("[0,0]", all_ ["0"])
            , ("[2]", all_ ["2"])
            , ("[2,0,2]", all_ ["4"])
            , ("[10,9,8,9]", all_ ["11"])
            ]
    describe "q180302: Count repetitions of an array" $ do
        specEval
            "u∕u#"
            [ ("[1,10,16,4,8,10,9,19,2,15,18,19,10,9,17,15,19,5,13,20]", all_ ["4"])
            , ("[11,8,6,15,9,19,2,2,4,19,14,19,13,12,16,13,0,5,0,8]", all_ ["5"])
            , ("[9,7,8,16,3,9,20,19,15,6,8,4,18,14,19,12,12,16,11,19]", all_ ["5"])
            , ("[10,17,17,7,2,18,7,13,3,10,1,5,15,4,6,0,19,4,17,0]", all_ ["5"])
            , ("[12,7,17,13,5,3,4,15,20,15,5,18,18,18,4,8,15,13,11,13]", all_ ["5"])
            , ("[0,3,6,1,5,2,16,1,6,3,12,1,16,5,4,5,6,17,4,8]", all_ ["6"])
            , ("[11,19,2,3,11,15,19,8,2,12,12,20,13,18,1,11,19,7,11,2]", all_ ["4"])
            , ("[6,4,11,14,17,3,17,11,2,16,14,1,2,1,15,15,12,10,11,13]", all_ ["6"])
            , ("[0,19,2,0,10,10,16,9,19,9,15,0,10,18,0,17,18,18,0,9]", all_ ["5"])
            , ("[1,19,17,17,0,2,14,10,10,12,5,14,16,7,15,15,18,11,17,7]", all_ ["5"])
            ]
        specEval
            "Ţ~Ƶ"
            [ ("[1,10,16,4,8,10,9,19,2,15,18,19,10,9,17,15,19,5,13,20]", Count 4)
            , ("[11,8,6,15,9,19,2,2,4,19,14,19,13,12,16,13,0,5,0,8]", Count 5)
            , ("[9,7,8,16,3,9,20,19,15,6,8,4,18,14,19,12,12,16,11,19]", Count 5)
            , ("[10,17,17,7,2,18,7,13,3,10,1,5,15,4,6,0,19,4,17,0]", Count 5)
            , ("[12,7,17,13,5,3,4,15,20,15,5,18,18,18,4,8,15,13,11,13]", Count 5)
            , ("[0,3,6,1,5,2,16,1,6,3,12,1,16,5,4,5,6,17,4,8]", Count 6)
            , ("[11,19,2,3,11,15,19,8,2,12,12,20,13,18,1,11,19,7,11,2]", Count 4)
            , ("[6,4,11,14,17,3,17,11,2,16,14,1,2,1,15,15,12,10,11,13]", Count 6)
            , ("[0,19,2,0,10,10,16,9,19,9,15,0,10,18,0,17,18,18,0,9]", Count 5)
            , ("[1,19,17,17,0,2,14,10,10,12,5,14,16,7,15,15,18,11,17,7]", Count 5)
            ]
    describe "q182305: Return the Closest Prime Number" $ do
        specEval
            "Ž-Q"
            [ ("80", first_ "79")
            , ("100", first_ "101")
            , ("5", first_ "5")
            , ("9", first_ "7")
            , ("532", first_ "523")
            , ("1", first_ "2")
            ]
    describe "q186881: First occurrence in the Sixers sequence" $ do
        specEval
            "Ň6*ƊajĭÐɗ$Ĩ"
            [ ("0", first_ "241")
            , ("17", first_ "297")
            , ("36", first_ "80")
            , ("55", first_ "128")
            , ("82", first_ "2")
            , ("95", first_ "557")
            ]
    describe "q187879: Integer Lists of Noah" $ do
        specEval
            "Ţ≡2="
            [ ("[7,13,9,2,10,2,4,10,7,13,4,9]", Check True)
            , ("[1,2,3,1,2,3]", Check True)
            , ("[10,100,1000,1,100,10,1000,1]", Check True)
            , ("[123,123]", Check True)
            , ("[8,22,57189,492,22,57188,8,492,57188,57189,1,1]", Check True)
            , ("[6,4,4,6,4,7,4,7]", Check False)
            , ("[2,2,2,2,2,2]", Check False)
            , ("[5,1,4,5,1,1,4]", Check False)
            , ("[77,31,5,31,80,77,5,8,8]", Check False)
            , ("[1,2,3,2,1]", Check False)
            , ("[44,4,4]", Check False)
            , ("[500,30,1]", Check False)
            , ("[1,2,1,1]", Check False)
            , ("[2,4,6,4,4,4]", Check False)
            , ("[2,23,34,4]", Check False)
            , ("[2,23,3,3,34,4]", Check False)
            ]
        specEval
            "oĉŤđ"
            [ ("[7,13,9,2,10,2,4,10,7,13,4,9]", Check True)
            , ("[1,2,3,1,2,3]", Check True)
            , ("[10,100,1000,1,100,10,1000,1]", Check True)
            , ("[123,123]", Check True)
            , ("[8,22,57189,492,22,57188,8,492,57188,57189,1,1]", Check True)
            , ("[6,4,4,6,4,7,4,7]", Check False)
            , ("[2,2,2,2,2,2]", Check False)
            , ("[5,1,4,5,1,1,4]", Check False)
            , ("[77,31,5,31,80,77,5,8,8]", Check False)
            , ("[1,2,3,2,1]", Check False)
            , ("[44,4,4]", Check False)
            , ("[500,30,1]", Check False)
            , ("[1,2,1,1]", Check False)
            , ("[2,4,6,4,4,4]", Check False)
            , ("[2,23,34,4]", Check False)
            , ("[2,23,3,3,34,4]", Check False)
            ]
        specEval
            "oĭu="
            [ ("[7,13,9,2,10,2,4,10,7,13,4,9]", Check True)
            , ("[1,2,3,1,2,3]", Check True)
            , ("[10,100,1000,1,100,10,1000,1]", Check True)
            , ("[123,123]", Check True)
            , ("[8,22,57189,492,22,57188,8,492,57188,57189,1,1]", Check True)
            , ("[6,4,4,6,4,7,4,7]", Check False)
            , ("[2,2,2,2,2,2]", Check False)
            , ("[5,1,4,5,1,1,4]", Check False)
            , ("[77,31,5,31,80,77,5,8,8]", Check False)
            , ("[1,2,3,2,1]", Check False)
            , ("[44,4,4]", Check False)
            , ("[500,30,1]", Check False)
            , ("[1,2,1,1]", Check False)
            , ("[2,4,6,4,4,4]", Check False)
            , ("[2,23,34,4]", Check False)
            , ("[2,23,3,3,34,4]", Check False)
            ]
    describe "q189358: Is it double speak?" $ do
        specEval
            "ĭ="
            [ ("\"aba\"", Check False)
            , ("\"abba\"", Check False)
            , ("\"aabb\"", Check True)
            , ("\"aaabb\"", Check False)
            , ("\"tthhiiss\"", Check True)
            , ("\"ttthhhiiisss\"", Check False)
            ]
    describe "q189932: Chunk + Enumerate a list of digits" $ do
        specEval
            "pNĉ#"
            [("[4,4,4,7,7,9,9,9,9,2,2,2,4,4]", all_ ["1", "1", "1", "2", "2", "3", "3", "3", "3", "4", "4", "4", "5", "5"])]
    describe "q190949: Count the number of triangles" $ do
        specEval
            "SƆ$đ+<"
            [ ("[1,2,3]", Count 0)
            , ("[1,1,1]", Count 1)
            , ("[1,1,1,1]", Count 4)
            , ("[1,2,3,4]", Count 1)
            , ("[3,4,5,7]", Count 3)
            , ("[1,42,69,666,1000000]", Count 0)
            , ("[12,23,34,45,56,67,78,89]", Count 34)
            , ("[1,2,3,4,5,6,7,8,9,10]", Count 50)
            ]
    describe "q194929: Is it a circumfix?" $ do
        specEval
            "≠;sᵃN,="
            [ ("\"apply\" \"appreciably\"", Check True)
            , ("\"rake\" \"racket by the lake\"", Check True)
            , ("\"bring\" \"brought him a gong\"", Check False)
            , ("\"falcon\" \"false conundrum\"", Check False)
            , ("\"pale\" \"pale ale\"", Check True)
            , ("\"b\" \"barb\"", Check False)
            , ("\"abba\" \"aba\"", Check False)
            , ("\"friend\" \"friend\"", Check False)
            , ("\"\" \"\"", Check False)
            , ("\"Twin Sister\" \"Twister\"", Check False)
            , ("\"case\" \"Castle\"", Check False)
            , ("\"<<@ 23|>\" \"<<@23??|> 23|>\"", Check True)
            ]
    describe "q195592: Average Two Letters" $ do
        specEval
            "µkH"
            [ ("[\"A\",\"C\"]", all_ ["B"])
            , ("[\"a\",\"z\"]", all_ ["m"])
            , ("[\"d\",\"j\"]", all_ ["g"])
            , ("[\"B\",\"e\"]", all_ ["S"])
            , ("[\"Z\",\"a\"]", all_ ["]"])
            ]
    describe "q196683: Round up my number" $ do
        specEval
            "/K*"
            [ ("3 1", all_ ["3"])
            , ("3 5", all_ ["6"])
            , ("3 9", all_ ["9"])
            , ("5 12", all_ ["15"])
            ]
    describe "q199290: Reversed Iota's" $ do
        specEval
            "RRᵐ↔"
            [("4", all_ ["[[1],[2,1],[3,2,1],[4,3,2,1]]"])]
        specEval
            "RpN↔"
            [("4", all_ ["[1]", "[2,1]", "[3,2,1]", "[4,3,2,1]"])]
    describe "q199353: Is this a triangle?" $ do
        specEval
            "∑ä<"
            [ ("[1,1,1]", Check True)
            , ("[1,2,3]", Check False)
            , ("[2,1,3]", Check False)
            , ("[1,3,2]", Check False)
            , ("[3,2,1]", Check False)
            , ("[3,1,2]", Check False)
            , ("[2,2,2]", Check True)
            , ("[3,4,5]", Check True)
            , ("[3,5,4]", Check True)
            , ("[5,3,4]", Check True)
            , ("[5,4,3]", Check True)
            , ("[10,9,3]", Check True)
            , ("[100,10,10]", Check False)
            ]
    describe "q199409: Is it a doubling sequence?" $ do
        specEval
            "∆$i≥"
            [ ("[10,20,30]", Check False)
            , ("[10,20,40]", Check True)
            , ("[1,2,3]", Check False)
            , ("[1,2,4]", Check True)
            , ("[1,2,10]", Check True)
            , ("[1,1]", Check False)
            , ("[10,1]", Check False)
            ]
    describe "q203330: Is it a brainfuck instruction?" $ do
        specEval
            "\"+,-.<>[]\"ē"
            [ ("'+'", Check True)
            , ("'#'", Check False)
            , ("'<'", Check True)
            , ("'>'", Check True)
            , ("'.'", Check True)
            , ("'P'", Check False)
            , ("','", Check True)
            ]
    describe "q203797: Generate list of numbers and their negative counterparts" $ do
        specEval
            "ïᶜ_"
            [ ("9 6", all_ ["[6,7,8,9]", "[-6,-7,-8,-9]"])
            , ("6 6", all_ ["[6]", "[-6]"])
            ]
    describe "q206853: Find the perfect square!" $ do
        specEval
            "Ď√‼Ṁ"
            [ ("4", all_ ["2"])
            , ("9", all_ ["3"])
            , ("12", all_ ["2"])
            , ("13", all_ ["1"])
            , ("108", all_ ["6"])
            ]
    describe "q206967: Sum the array times n, except the last" $ do
        specEval
            "Ɔᵈ∙+"
            [ ("[3,1,4,1,5] 10", all_ ["95"])
            , ("[3,1,4,1,5] 1", all_ ["14"])
            , ("[1] 999", all_ ["1"])
            ]
    describe "q207736: The shortest way to find one unique value when all other values are the same" $ do
        specEval
            "Ţṃ"
            [ ("[1,1,1,2,1,1]", all_ ["2"])
            , ("[3,5,5,5,5]", all_ ["3"])
            , ("[9,2,9,9,9,9,9]", all_ ["2"])
            , ("[4,4,4,6]", all_ ["6"])
            , ("[5,8,8]", all_ ["5"])
            , ("[8,5,8]", all_ ["5"])
            , ("[8,8,5]", all_ ["5"])
            ]
        specEval
            "ĕᵖf"
            [ ("[1,1,1,2,1,1]", all_ ["2"])
            , ("[3,5,5,5,5]", all_ ["3"])
            , ("[9,2,9,9,9,9,9]", all_ ["2"])
            , ("[4,4,4,6]", all_ ["6"])
            , ("[5,8,8]", all_ ["5"])
            , ("[8,5,8]", all_ ["5"])
            , ("[8,8,5]", all_ ["5"])
            ]
        specEval
            "oĉ~z"
            [ ("[1,1,1,2,1,1]", all_ ["2"])
            , ("[3,5,5,5,5]", all_ ["3"])
            , ("[9,2,9,9,9,9,9]", all_ ["2"])
            , ("[4,4,4,6]", all_ ["6"])
            , ("[5,8,8]", all_ ["5"])
            , ("[8,5,8]", all_ ["5"])
            , ("[8,8,5]", all_ ["5"])
            ]
    describe "q208982: Antisymmetry of a Matrix" $ do
        specEval
            "Ť_="
            [ ("[[1,1,1],[1,1,1],[1,1,1]]", Check False)
            , ("[[0,0,1],[0,0,0],[-1,0,0]]", Check True)
            , ("[[0,-2],[2,0]]", Check True)
            ]
    describe "q209146: How many 1's we get" $ do
        specEval
            "ïƊj1Ĉ"
            [ ("100 1", all_ ["21"])
            , ("200 11", all_ ["138"])
            , ("678 123", all_ ["182"])
            ]
    describe "q210162: Is it almost-prime?" $ do
        specEval
            "ƒz"
            [ ("2", Check True)
            , ("3", Check True)
            , ("8", Check True)
            , ("27", Check True)
            , ("1331", Check True)
            , ("4913", Check True)
            , ("40353607", Check True)
            , ("6", Check False)
            , ("36", Check False)
            , ("54", Check False)
            , ("1938", Check False)
            , ("175560", Check False)
            , ("17294403", Check False)
            ]
    describe "q216734: Jelly's Untruth" $ do
        specEval
            "Ħ±"
            [ ("[0,2,4,5]", all_ ["[1,0,1,0,1,1]"])
            , ("[4]", all_ ["[0,0,0,0,1]"])
            , ("[1,0,0,1]", all_ ["[1,1]"])
            , ("[4,3,2]", all_ ["[0,0,1,1,1]"])
            , ("[0]", all_ ["[1]"])
            ]
    describe "q217303: Linear integer function generator" $ do
        specEval
            "ᵉᵑ{ˣ∙ɔᵈç}T"
            [ ("10 [0,1] [1,1]", all_ ["[0,1,1,2,3,5,8,13,21,34]"])
            , ("20 [1,0,0] [1,1,0]", all_ ["[1,0,0,1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37]"])
            , ("10 [3,0,2] [1,1,0]", all_ ["[3,0,2,3,2,5,5,7,10,12]"])
            , ("5 [0,0] [1,1]", all_ ["[0,0,0,0,0]"])
            , ("20 [0,-1,0,1] [0,1,0,-1]", all_ ["[0,-1,0,1,-2,2,-1,-1,3,-4,3,0,-4,7,-7,3,4,-11,14,-10]"])
            ]
    describe "q224125: Replace all items with their counts" $ do
        specEval
            "ᵐĈ"
            [ ("[1]", all_ ["[1]"])
            , ("[1,2]", all_ ["[1,1]"])
            , ("[1,1]", all_ ["[2,2]"])
            , ("[1,4,4]", all_ ["[1,2,2]"])
            , ("[4,4,2]", all_ ["[2,2,1]"])
            , ("[4,4,4,4]", all_ ["[4,4,4,4]"])
            , ("[10,20,10,20]", all_ ["[2,2,2,2]"])
            , ("[1,2,2,4,4,4,4]", all_ ["[1,2,2,4,4,4,4]"])
            , ("[1,2,2,1,4,8,1]", all_ ["[3,2,2,3,1,1,3]"])
            ]
    describe "q225203: Delannoy numbers" $ do
        specEval
            "Ṁ→ᵒÇ∏ƃ"
            [ ("[5,8]", all_ ["13073"])
            , ("[5,7]", all_ ["7183"])
            , ("[3,9]", all_ ["1159"])
            , ("[8,6]", all_ ["40081"])
            , ("[1,7]", all_ ["15"])
            , ("[7,0]", all_ ["1"])
            , ("[11,6]", all_ ["227305"])
            , ("[0,4]", all_ ["1"])
            ]
        specEval
            "ʷ{į1?-ň"
            [ ("[3,3]", Count 63)
            , ("[3,9]", Count 1159)
            , ("[1,7]", Count 15)
            , ("[7,0]", Count 1)
            , ("[0,4]", Count 1)
            ]
    describe "q226593: Split a list into maximal equal-sum sections" $ do
        specEval
            "O:Ŝ≡$Ðål"
            [ ("[9,5,1,2,9,2]", all_ ["[[1,2,9,2],[9,5]]"])
            , ("[1,1,3,5,7,4]", all_ ["[[3,4],[7],[1,1,5]]"])
            , ("[2,9,6,1,5,8,2]", all_ ["[[1,8,2],[6,5],[2,9]]"])
            , ("[5,4,1]", all_ ["[[4,1],[5]]"])
            , ("[3,8,1,4,2,2]", all_ ["[[3,1,4,2],[8,2]]"])
            , ("[6,9,3,8,1]", all_ ["[[8,1],[6,3],[9]]"])
            , ("[4,1,6,9,1,4,5,2]", all_ ["[[1,9,4,2],[4,6,1,5]]"])
            , ("[8,7,8,6,1]", all_ ["[[8,6,1],[7,8]]"])
            , ("[2,7,4,5]", all_ ["[[4,5],[2,7]]"])
            , ("[5,2,1,4,4]", all_ ["[[4,4],[5,2,1]]"])
            , ("[5,7,4,6,2]", all_ ["[[4,6,2],[5,7]]"])
            , ("[4,1,6,6,9]", all_ ["[[4,9],[1,6,6]]"])
            , ("[2,6,4]", all_ ["[[2,4],[6]]"])
            , ("[6,3,1,6,8,4,5,7]", all_ ["[[1,8,4,7],[6,3,6,5]]"])
            , ("[2,2,2]", all_ ["[[2],[2],[2]]"])
            , ("[2,4,5]", all_ ["[[2,4,5]]"])
            ]
    describe "q229624: Generalised multi-dimensional chess knight's moves" $ do
        specEval
            "8ᵚ~ᵖ{≈←ň‼į="
            [ ("[0,7]", all_ ["[1,5]", "[2,6]"])
            , ("[3,4]", all_ ["[1,3]", "[1,5]", "[2,2]", "[2,6]", "[4,2]", "[4,6]", "[5,3]", "[5,5]"])
            , ("[7,7,7]", all_ ["[5,6,7]", "[5,7,6]", "[6,5,7]", "[6,7,5]", "[7,5,6]", "[7,6,5]"])
            ]
        specEval
            "0*2R+ŋ↕ũ+ň8<"
            [ ("[0,7]", all_ ["[1,5]", "[2,6]"])
            , ("[3,4]", all_ ["[4,6]", "[4,2]", "[2,6]", "[2,2]", "[5,5]", "[5,3]", "[1,5]", "[1,3]"])
            , ("[7,7,7]", all_ ["[6,5,7]", "[6,7,5]", "[5,6,7]", "[5,7,6]", "[7,6,5]", "[7,5,6]"])
            ]
    describe "q230402: Is this a Permutation of 1..n" $ do
        specEval
            "ox="
            [ ("[0]", Check True)
            , ("[0,1]", Check True)
            , ("[1,0]", Check True)
            , ("[0,2,1]", Check True)
            , ("[2,0,1]", Check True)
            , ("[0,1,2,3,4,5,6,7,8,9,10,11]", Check True)
            , ("[11,10,9,8,7,6,5,4,3,2,1,0]", Check True)
            , ("[5,2,7,11,0,9,3,1,6,8,4,10]", Check True)
            , ("[15,36,13,14,22,7,28,34,20,5,4,33,37,8,35,25,23,31,27,6,19,32,38,11,29,26,39,21,10,40,41,0,9,18,1,24,16,12,2,17,30,3]", Check True)
            , ("[1]", Check False)
            , ("[11]", Check False)
            , ("[0,0]", Check False)
            , ("[0,2]", Check False)
            , ("[1,0,2,1]", Check False)
            , ("[5,2,4,3,6,0,7,0,1]", Check False)
            , ("[14,6,12,4,10,8,16,2]", Check False)
            , ("[33,32,37,16,34,10,35,30,27,13,5,14,17,1,18,39,28,40,8,0,26,22,19,31,25,24,36,7,12,29,38,6,4,2,20,3,10,15,9,21,11,23]", Check False)
            ]
        specEval
            "x↕="
            [ ("[0]", Check True)
            , ("[0,1]", Check True)
            , ("[1,0]", Check True)
            , ("[0,2,1]", Check True)
            , ("[2,0,1]", Check True)
            , ("[0,1,2,3,4,5,6,7,8,9,10,11]", Check True)
            , ("[11,10,9,8,7,6,5,4,3,2,1,0]", Check True)
            , ("[5,2,7,11,0,9,3,1,6,8,4,10]", Check True)
            , ("[15,36,13,14,22,7,28,34,20,5,4,33,37,8,35,25,23,31,27,6,19,32,38,11,29,26,39,21,10,40,41,0,9,18,1,24,16,12,2,17,30,3]", Check True)
            , ("[1]", Check False)
            , ("[11]", Check False)
            , ("[0,0]", Check False)
            , ("[0,2]", Check False)
            , ("[1,0,2,1]", Check False)
            , ("[5,2,4,3,6,0,7,0,1]", Check False)
            , ("[14,6,12,4,10,8,16,2]", Check False)
            , ("[33,32,37,16,34,10,35,30,27,13,5,14,17,1,18,39,28,40,8,0,26,22,19,31,25,24,36,7,12,29,38,6,4,2,20,3,10,15,9,21,11,23]", Check False)
            ]
    describe "q231654: Third Stirling numbers of the second kind" $ do
        specEval
            "→4ᵒE∑∆l"
            [ ("1", all_ ["1"])
            , ("2", all_ ["6"])
            , ("3", all_ ["25"])
            , ("4", all_ ["90"])
            , ("5", all_ ["301"])
            , ("6", all_ ["966"])
            , ("7", all_ ["3025"])
            , ("8", all_ ["9330"])
            , ("9", all_ ["28501"])
            , ("10", all_ ["86526"])
            ]
        specEval
            "O3L"
            [ ("1", Count 0)
            , ("2", Count 0)
            , ("3", Count 1)
            , ("4", Count 6)
            , ("5", Count 25)
            , ("6", Count 90)
            , ("7", Count 301)
            , ("8", Count 966)
            , ("9", Count 3025)
            , ("10", Count 9330)
            ]
    describe "q233182: Split into sublists of specified sizes" $ do
        specEval
            "J$ᶻL"
            [ ("[4,5,1,2,6,1,7,9,6] [2,4,3]", all_ ["[[4,5],[1,2,6,1],[7,9,6]]"])
            , ("[4,2,8,7,3,5,9,3,1,9,1,8,1,7,2,8,3,7,6] [1,3,1,14]", all_ ["[[4],[2,8,7],[3],[5,9,3,1,9,1,8,1,7,2,8,3,7,6]]"])
            , ("[8,7,4,6] [1,3]", all_ ["[[8],[7,4,6]]"])
            , ("[7] [1]", all_ ["[[7]]"])
            , ("[6,4,3,8,9,3,6,5,7,8,3,2,5,1,2] [3,3,3,3,3]", all_ ["[[6,4,3],[8,9,3],[6,5,7],[8,3,2],[5,1,2]]"])
            , ("[2,7,9,3,8,1,5] [4,3]", all_ ["[[2,7,9,3],[8,1,5]]"])
            , ("[1,9,8,9,6,3,4,2,3,4,1,8,5,5,2,9,3,6,7] [3,1,2,13]", all_ ["[[1,9,8],[9],[6,3],[4,2,3,4,1,8,5,5,2,9,3,6,7]]"])
            , ("[7,4,4,7,5,5] [1,2,3]", all_ ["[[7],[4,4],[7,5,5]]"])
            , ("[8,7,3] [3]", all_ ["[[8,7,3]]"])
            ]
    describe "q233641: Hunt for discount" $ do
        specEval
            "o↔ĭ∑ä"
            [ ("[10]", all_ ["0"])
            , ("[10,20]", all_ ["5"])
            , ("[10,20,30]", all_ ["10"])
            , ("[2,2,2,2]", all_ ["2"])
            , ("[4,10,6,8,2,40]", all_ ["9"])
            ]
    describe "q235964: Implement the hyperfactorial" $ do
        specEval
            "R:E∏"
            [ ("0", all_ ["1"])
            , ("1", all_ ["1"])
            , ("2", all_ ["4"])
            , ("3", all_ ["108"])
            , ("4", all_ ["27648"])
            , ("5", all_ ["86400000"])
            , ("6", all_ ["4031078400000"])
            , ("7", all_ ["3319766398771200000"])
            , ("8", all_ ["55696437941726556979200000"])
            ]
    describe "q236285: Maybe fractal sequence?" $ do
        specEval
            "ux→=∕$p="
            [ ("[1]", Check True)
            , ("[1,1,1]", Check True)
            , ("[1,2,3]", Check True)
            , ("[1,2,1]", Check True)
            , ("[1,2,1,3,2,1,4,3]", Check True)
            , ("[1,1,2,3,1,2,3,1,4,2,5,3,1]", Check True)
            , ("[999]", Check False)
            , ("[1,3]", Check False)
            , ("[1,2,1,3,2,1,4,1]", Check False)
            , ("[1,3,1,5,3,1,7,5]", Check False)
            ]
    describe "q237085: Swap every two elements in a list" $ do
        specEval
            "ĭ$Ĭ"
            [ ("[1,2,3,4,5,6]", all_ ["[2,1,4,3,6,5]"])
            , ("[0,1,0,1]", all_ ["[1,0,1,0]"])
            ]
    describe "q237377: Ways to add 1 to lists of lists" $ do
        specEval
            "ʰ{$c"
            [ ("[] 9", all_ [])
            , ("[[]] 9", all_ ["[[9]]"])
            , ("[[1,2,3]] 10", all_ ["[[10,1,2,3]]"])
            , ("[[1,2],[3,4],[5],[]] 7", all_ ["[[7,1,2],[3,4],[5],[]]", "[[1,2],[7,3,4],[5],[]]", "[[1,2],[3,4],[7,5],[]]", "[[1,2],[3,4],[5],[7]]"])
            , ("[[1,2],[2,2],[2]] 2", all_ ["[[2,1,2],[2,2],[2]]", "[[1,2],[2,2,2],[2]]", "[[1,2],[2,2],[2,2]]"])
            ]
    describe "q238607: Converge to a number" $ do
        specEval
            "ƊsC↔~cɗ-"
            [ ("4", all_ ["1", "2", "3", "4"])
            , ("16", all_ ["10", "11", "12", "13", "14", "15", "16"])
            , ("35", all_ ["10", "20", "30", "31", "32", "33", "34", "35"])
            , ("103", all_ ["100", "101", "102", "103"])
            , ("320", all_ ["100", "200", "300", "310", "320"])
            , ("354", all_ ["100", "200", "300", "310", "320", "330", "340", "350", "351", "352", "353", "354"])
            , ("1000", all_ ["1000"])
            , ("1001", all_ ["1000", "1001"])
            , ("4037 ", all_ ["1000", "2000", "3000", "4000", "4010", "4020", "4030", "4031", "4032", "4033", "4034", "4035", "4036", "4037"])
            ]
        specEval
            "¢Bx¢E$y↔∫"
            [ ("4", all_ ["[1,2,3,4]"])
            , ("16", all_ ["[10,11,12,13,14,15,16]"])
            , ("35", all_ ["[10,20,30,31,32,33,34,35]"])
            , ("103", all_ ["[100,101,102,103]"])
            , ("320", all_ ["[100,200,300,310,320]"])
            , ("354", all_ ["[100,200,300,310,320,330,340,350,351,352,353,354]"])
            , ("1000", all_ ["[1000]"])
            , ("1001", all_ ["[1000,1001]"])
            , ("4037 ", all_ ["[1000,2000,3000,4000,4010,4020,4030,4031,4032,4033,4034,4035,4036,4037]"])
            ]
    describe "q239994: 1 bit, 2 bits, 3 bits, …" $ do
        specEval
            "ṖÞx→↕="
            [ ("1", Count 1)
            , ("2", Count 1)
            , ("3", Count 0)
            , ("4", Count 2)
            , ("5", Count 1)
            , ("10", Count 2)
            , ("14", Count 6)
            , ("19", Count 7)
            , ("20", Count 10)
            ]
    describe "q240187: Repeating slices of an array incrementally" $ do
        specEval
            "Jᵖ{ix→ᶻL"
            [ ("[1,2,3,4,5,6,7,8,9,10]", first_ "[[1],[2,3],[4,5,6],[7,8,9,10]]")
            , ("[10,20,30,40]", first_ "[[10],[20,30],[40]]")
            , ("[100,200,300,400,500]", first_ "[[100],[200,300],[400,500]]")
            ]
    describe "q241156: Array depth of a ragged list" $ do
        specEval
            "Uˡ{Ťj"
            [ ("[1]", all_ ["1"])
            , ("[1,2,3]", all_ ["1"])
            , ("[[1,2,3]]", all_ ["2"])
            , ("[3,[3,[3],3],3]", all_ ["1"])
            , ("[[[[1],2],[3,[4]]]]", all_ ["3"])
            , ("[[1,2,3],[4,5,6]]", all_ ["2"])
            , ("[[1,2,[3]],[4,[5],[6,[7]]]]", all_ ["2"])
            , ("[[1,2],[3,4,5],[6,7,8,9]]", all_ ["1"])
            , ("[[[1,2]],[[3,4],[5,6]]]", all_ ["1"])
            , ("[[1,[2]],[[[3]],[[[4]]]]]", all_ ["2"])
            , ("[[[1],[2]],[[3,4],[5,6]]]", all_ ["2"])
            , ("[[[[[[[3]]]]]]]", all_ ["7"])
            ]
    describe "q241267: Remove odd indices and double the even indices" $ do
        specEval
            "ĭ:Ĭ"
            [ ("\"abcdef\"", all_ ["bbddff"])
            , ("\"umbrella\"", all_ ["mmrrllaa"])
            , ("\"looooooooong text\"", all_ ["ooooooooooggttxx"])
            , ("\"abc\"", all_ ["bb"])
            , ("\"xkcd\"", all_ ["kkdd"])
            , ("\"Hello, World!\"", all_ ["eell,,WWrrdd"])
            , ("\"D\"", all_ ["[]"])
            , ("\"KK\"", all_ ["KK"])
            , ("\"Hi\"", all_ ["ii"])
            , ("\"odd_length!\"", all_ ["dd__eegghh"])
            , ("\"\"", all_ ["[]"])
            ]
    describe "q241474: Move to Right and left" $ do
        specEval
            "çç+"
            [ ("[1,2,3]", all_ ["[1,2,4,2,3]"])
            , ("[4,2]", all_ ["[4,2,4,2]"])
            , ("[1]", all_ ["[1,0,1]"])
            , ("[7,4,5,6]", all_ ["[7,4,12,10,5,6]"])
            , ("[1,2,4,2,1]", all_ ["[1,2,5,4,5,2,1]"])
            , ("[1,0,1]", all_ ["[1,0,2,0,1]"])
            , ("[1,0,2,0,1]", all_ ["[1,0,3,0,3,0,1]"])
            , ("[8,7,6,5,4]", all_ ["[8,7,14,12,10,5,4]"])
            , ("[1,4,9,16]", all_ ["[1,4,10,20,9,16]"])
            , ("[1,2]", all_ ["[1,2,1,2]"])
            ]
        specEval
            "5Ƃ×"
            [ ("[1,2,3]", all_ ["[1,2,4,2,3]"])
            , ("[4,2]", all_ ["[4,2,4,2]"])
            , ("[1]", all_ ["[1,0,1]"])
            , ("[7,4,5,6]", all_ ["[7,4,12,10,5,6]"])
            , ("[1,2,4,2,1]", all_ ["[1,2,5,4,5,2,1]"])
            , ("[1,0,1]", all_ ["[1,0,2,0,1]"])
            , ("[1,0,2,0,1]", all_ ["[1,0,3,0,3,0,1]"])
            , ("[8,7,6,5,4]", all_ ["[8,7,14,12,10,5,4]"])
            , ("[1,4,9,16]", all_ ["[1,4,10,20,9,16]"])
            , ("[1,2]", all_ ["[1,2,1,2]"])
            ]
    describe "q245804: Damerau-Damerau distance" $ do
        specEval
            "Sđ>"
            [ ("[3,2,1]", Count 3)
            , ("[1,2,3,4]", Count 0)
            , ("[2,1,3,4]", Count 1)
            , ("[2,1,4,3]", Count 2)
            , ("[2,4,1,3]", Count 3)
            , ("[4,2,3,1]", Count 5)
            , ("[4,3,2,1]", Count 6)
            ]
    describe "q247104: Euler characteristic of a binary matrix" $ do
        specEval
            "2R:4*Ð×j:1Ĉ$2÷3Ĉ-"
            [ ("[[1]]", all_ ["1"])
            , ("[[1,0,1]]", all_ ["2"])
            , ("[[1,0],[0,1]]", all_ ["1"])
            , ("[[0,1,0],[0,0,1],[1,1,1]]", all_ ["1"])
            , ("[[0,1,1,0],[1,0,1,1],[1,1,0,1],[0,1,1,0]]", all_ ["-1"])
            , ("[[0,0,1,1,0],[0,1,1,1,1],[1,1,0,1,1],[0,1,1,0,0]]", all_ ["0"])
            , ("[[1,1,1,0,1,1,1],[1,0,1,0,1,0,1],[1,1,1,0,1,1,1]]", all_ ["0"])
            , ("[[1,1,1,1,1],[1,0,0,0,1],[1,0,1,0,1],[1,0,0,0,1],[1,1,1,1,1]] ", all_ ["1"])
            ]
    describe "q247326: There's more than one way to skin a set" $ do
        specEval
            "S∑a:u∕u"
            [ ("[1]", all_ ["[]"])
            , ("[4,5,2]", all_ ["[]"])
            , ("[9,10,11,12]", all_ ["[21]"])
            , ("[2,3,5,6]", all_ ["[5,8,11]"])
            , ("[15,16,7,1,4]", all_ ["[16,23,20,27]"])
            , ("[1,2,3,4,5]", all_ ["[3,4,5,6,7,8,9,10,11,12]"])
            ]
        specEval
            "Ë→Ƃʳ×2m2Ĩ"
            [ ("[1]", all_ [])
            , ("[4,5,2]", all_ [])
            , ("[9,10,11,12]", all_ ["21"])
            , ("[2,3,5,6]", all_ ["5", "8", "11"])
            , ("[15,16,7,1,4]", all_ ["16", "20", "23", "27"])
            , ("[1,2,3,4,5]", all_ ["3", "4", "5", "6", "7", "8", "9", "10", "11", "12"])
            ]
    describe "q247398: Alternating sums of multidimensional arrays" $ do
        specEval
            "ʷ{£d"
            [ ("[1]", all_ ["1"])
            , ("[-1]", all_ ["-1"])
            , ("[1,2]", all_ ["-1"])
            , ("[2,0,4]", all_ ["6"])
            , ("[1,-2]", all_ ["3"])
            , ("[[1]]", all_ ["1"])
            , ("[[1,2],[4,8]]", all_ ["3"])
            , ("[[-1,-1],[2,2]]", all_ ["0"])
            , ("[[[[1],[2]],[[4],[8]]]]", all_ ["3"])
            , ("[[[1,2],[2,4],[4,8]],[[-4,-4],[-1,1],[2,-2]]]", all_ ["-9"])
            ]
    describe "q247676: Generate all_ 8 Knight's Moves" $ do
        specEval
            "į→ŋ"
            [("", all_ ["[1,2]", "[1,-2]", "[-1,2]", "[-1,-2]", "[2,1]", "[2,-1]", "[-2,1]", "[-2,-1]"])]
    describe "q248245: Make a list flat" $ do
        specEval
            "V"
            [ ("[[3],[3,[[6]]]]", all_ ["[3,3,6]"])
            , ("[]", all_ ["[]"])
            , ("[[],[]]", all_ ["[]"])
            , ("[[1,4,6],[1,[2,67,[5,7]]]]", all_ ["[1,4,6,1,2,67,5,7]"])
            ]
        specEval
            "ʸj"
            [ ("[[3],[3,[[6]]]]", all_ ["[3,3,6]"])
            , ("[]", all_ ["[]"])
            , ("[[],[]]", all_ ["[]"])
            , ("[[1,4,6],[1,[2,67,[5,7]]]]", all_ ["[1,4,6,1,2,67,5,7]"])
            ]
    describe "q248445: Print the power set of the power set ... of an empty set" $ do
        specEval
            "Øᶦ{Sa"
            [("", truncate_ ["[]", "[[]]", "[[],[[]]]", "[[],[[]],[[[]]],[[],[[]]]]"])]
    describe "q248991: The Unaverageables" $ do
        specEval
            "ᶠ{+ä∩z"
            [ ("[1,2,3]", all_ ["[2]"])
            , ("[1,2,3,4]", all_ ["[]"])
            , ("[1,3,4,5]", all_ ["[4]"])
            , ("[1,5,10,20,40]", all_ ["[1,5,10,20,40]"])
            , ("[1,5,6,10]", all_ ["[1,5,6,10]"])
            , ("[1,2,3,4,10,52,100,200]", all_ ["[10,52,200]"])
            , ("[1,2,3,5,8,13,21,34]", all_ ["[]"])
            ]
    describe "q249868: Every possible pairing" $ do
        specEval
            "O2ᵚL"
            [ ("2", all_ ["[[0,1]]"])
            , ("4", all_ ["[[2,3],[0,1]]", "[[1,3],[0,2]]", "[[0,3],[1,2]]"])
            ]
    describe "q250213: Generate an arbitrary half of a string" $ do
        specEval
            "↕;="
            [ ("\"aaaabbbb\"", first_ "aabb")
            , ("\"abab\"", first_ "ab")
            , ("\"aabbaa\"", first_ "aab")
            , ("\"aabbaaaa\"", first_ "aaba")
            , ("\"baccba\"", first_ "bac")
            , ("\"aabbcc\"", first_ "abc")
            , ("\"abcabc\"", first_ "abc")
            ]
    describe "q250283: Rearrange to a palindrome" $ do
        specEval
            "↕ƀ="
            [ ("\"nanas\"", first_ "nasan")
            , ("\"coconutnut\"", first_ "conuttunoc")
            , ("\"apotato\"", first_ "aotptoa")
            , ("\"canadadance\"", first_ "canadedanac")
            , ("\"nananana\"", first_ "nanaanan")
            , ("\"anaan\"", first_ "anana")
            ]
    describe "q250395: Have you heard of tralindromes?" $ do
        specEval
            "p:ƀᵃᶜt$,,="
            [ ("\"a\"", Check True)
            , ("\"bb\"", Check True)
            , ("\"ccc\"", Check True)
            , ("\"coco\"", Check True)
            , ("\"mamma\"", Check True)
            , ("\"uhhuh\"", Check True)
            , ("\"xyyxxy\"", Check True)
            , ("\"banaban\"", Check True)
            , ("\"dottodot\"", Check True)
            , ("\"dadadadada\"", Check True)
            , ("\"xy\"", Check False)
            , ("\"coc\"", Check False)
            , ("\"faff\"", Check False)
            , ("\"xyzzy\"", Check False)
            , ("\"mummy\"", Check False)
            , ("\"random\"", Check False)
            , ("\"hotshot\"", Check False)
            , ("\"tralindrome\"", Check False)
            , ("\"dadadadadada\"", Check False)
            , ("\"aabaabaaaabaa\"", Check False)
            , ("\"abccbacbaabcabc\"", Check False)
            ]
    describe "q250942: Divisor of a string" $ do
        specEval
            "J≡"
            [ ("\"abcdabcd\"", all_ ["abcd", "abcdabcd"])
            , ("\"aaa\"", all_ ["a", "aaa"])
            , ("\"aaaaaaaa\"", all_ ["a", "aa", "aaaa", "aaaaaaaa"])
            , ("\"abcdef\"", all_ ["abcdef"])
            ]
    describe "q251594: Find the nth Mersenne Prime" $ do
        specEval
            "ŇË←Q"
            [("", truncate_ ["3", "7", "31", "127", "8191"])]
    describe "q251674: Number of ways to make an amount with coins" $ do
        specEval
            "Ṗ¢$¦"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 2)
            , ("4", Count 3)
            , ("5", Count 4)
            , ("6", Count 5)
            , ("7", Count 6)
            , ("8", Count 7)
            , ("9", Count 8)
            ]
    describe "q251772: Tut-tut-tut-tut-tut" $ do
        specEval
            "ʷ{;\"tut-tut\"=ip,}Ø="
            [ ("\"tut-tut\"", Check True)
            , ("\"tut-tutut-tut\"", Check True)
            , ("\"tut-tut-tut-tut-tut\"", Check True)
            , ("\"tut-tutut-tut-tuttut-tut\"", Check True)
            , ("\"tut-tuttut-tutut-tut-tutut-tut\"", Check True)
            , ("\"x\"", Check False)
            , ("\"-tut\"", Check False)
            , ("\"tut-tutx\"", Check False)
            , ("\"xtut-tut\"", Check False)
            , ("\"tut-tutt-tut\"", Check False)
            , ("\"tut-tuttuttut-tut\"", Check False)
            , ("\"tut-tututut-tutut-tut\"", Check False)
            ]
    describe "q252057: Numbers vs. Strings: Language fitness challenge" $ do
        specEval
            "D:×Ṁ"
            [ ("2 22", all_ ["2"])
            , ("2 8", all_ ["1"])
            , ("2 15", all_ ["4"])
            , ("3 100", all_ ["6"])
            , ("10 12345", all_ ["46"])
            ]
    describe "q252082: Reconstruct Matrix from its diagonals" $ do
        specEval
            "#2÷:→:ᵒ{ᵋ{-+@}m@"
            [ ("[[5]]", all_ ["[[5]]"])
            , ("[[0],[1,69],[13]]", all_ ["[[1,0],[13,69]]"])
            , ("[[25],[0,1],[6,23,10],[420,9],[67]]", all_ ["[[6,0,25],[420,23,1],[67,9,10]]"])
            ]
        specEval
            "Øc;$ᶻ,ŤxᶻŘ"
            [ ("[[5]]", all_ ["[[5]]"])
            , ("[[0],[1,69],[13]]", all_ ["[[1,13],[0,69]]"])
            , ("[[25],[0,1],[6,23,10],[420,9],[67]]", all_ ["[[6,420,67],[9,0,23],[1,10,25]]"])
            ]
    describe "q252126: Count the number of compositions of n in which the greatest part is odd" $ do
        specEval
            "Ṗ↕ũṀ←½"
            [ ("1", Count 1)
            , ("2", Count 1)
            , ("3", Count 2)
            , ("4", Count 3)
            , ("5", Count 7)
            , ("6", Count 14)
            , ("7", Count 30)
            ]
    describe "q252189: Carryless factors" $ do
        specEval
            "Rᶠ{$R~ᵃƂ×Öƃ="
            [ ("2", all_ ["[1,2]"])
            , ("4", all_ ["[1,2,4]"])
            , ("5", all_ ["[1,3,5]"])
            , ("6", all_ ["[1,2,3,6]"])
            , ("25", all_ ["[1,25]"])
            , ("39", all_ ["[1,3,5,11,29,39]"])
            , ("42", all_ ["[1,2,7,14,21,42]"])
            , ("100", all_ ["[1,2,4,25,50,100]"])
            ]
    describe "q252303: Cut along the lines" $ do
        specEval
            "ĉᴶj"
            [ ("[1,0]", all_ ["[[1],[0]]", "[[1,0]]"])
            , ("[1,1,1,1]", all_ ["[[1,1,1,1]]"])
            , ("[1,1,0,0,1]", all_ ["[[1,1],[0,0],[1]]", "[[1,1],[0,0,1]]", "[[1,1,0,0],[1]]", "[[1,1,0,0,1]]"])
            ]
    describe "q252927: Make a Court Transcriber" $ do
        specEval
            "ᶠ{JS=}ş"
            [ ("[\"dictionary\",\"transcriber\"] [\"dic\",\"ion\",\"ary\"]", all_ ["dictionary"])
            , ("[\"dictionary\",\"transcriber\"] [\"tra\",\"scr\",\"ber\"]", all_ ["transcriber"])
            ]
    describe "q254224: Maximum average ord" $ do
        specEval
            "ᵐµṀ"
            [ ("[\"hello\",\"world\",\"bye\"]", all_ ["552/5"])
            , ("[\"code\",\"golf\",\"stack\",\"exchange\"]", all_ ["534/5"])
            , ("[\"!@#\",\"$%^\",\"&*(\"]", all_ ["167/3"])
            , ("[\"qwertyuiop[\",\"asdfghjkl;\",\"zxcvbnm,\"]", all_ ["1220/11"])
            ]
    describe "q254947: Range of ASCII values" $ do
        specEval
            "o:h-l"
            [ ("\"Hello, World!\"", all_ ["82"])
            , ("\"aaaaa\"", all_ ["0"])
            , ("\"Code Golf\"", all_ ["79"])
            , ("\"Stack Exchange\"", all_ ["88"])
            , ("\"ASCII\"", all_ ["18"])
            , ("\"eo:h-l\"", all_ ["66"])
            ]
    describe "q255274: CGAC2022 Day 7: Fen The Wicked" $ do
        specEval
            "p↔:#:ËGT∑"
            [ ("[]", all_ [])
            , ("[999]", all_ ["999"])
            , ("[3,1,4]", all_ ["3", "4", "4"])
            , ("[3,1,4,1,5,9,2,6]", all_ ["3", "4", "4", "9", "5", "14", "2", "31"])
            ]
        specEval
            "x:→&xï@Ŝ"
            [ ("[]", all_ ["[]"])
            , ("[999]", all_ ["[999]"])
            , ("[3,1,4]", all_ ["[3,4,4]"])
            , ("[3,1,4,1,5,9,2,6]", all_ ["[3,4,4,9,5,14,2,31]"])
            ]
    describe "q255344: CGAC2022 Day 9: Playing with bits" $ do
        specEval
            "Ňᵖ{:ÄXÞ½="
            [ ("1", truncate_ ["1", "2", "3", "4", "6", "7", "8", "12", "14", "15"])
            , ("2", truncate_ ["5", "9", "10", "11", "13", "17", "18", "19", "20", "22"])
            , ("3", truncate_ ["21", "37", "41", "42", "43", "45", "53", "69", "73", "74"])
            , ("4", truncate_ ["85", "149", "165", "169", "170", "171", "173", "181", "213", "277"])
            , ("5", truncate_ ["341", "597", "661", "677", "681", "682", "683", "685", "693", "725"])
            ]
    describe "q255373: CGAC2022 Day 10: Help Santa sort presents!" $ do
        specEval
            "ˡ{ᵗ≡ĭ?}Å"
            [ ("[1]", all_ ["0"])
            , ("[1,0,1,0,1,0,1]", all_ ["1"])
            , ("[1,0,1,0,1,1]", all_ ["3"])
            , ("[1,0,1,1,0,0,1,1,0,1,1,1,0,0,0,1,1,0,1,0,1,1,1,0,0]", all_ ["5"])
            ]
    describe "q255650: Sum every second digit in a number" $ do
        specEval
            "Ɗĭ∑"
            [ ("10", all_ ["0"])
            , ("101011", all_ ["1"])
            , ("548915381", all_ ["26"])
            , ("999999", all_ ["27"])
            , ("2147483647", all_ ["29"])
            , ("999999999", all_ ["36"])
            ]
    describe "q255822: Replace 0s In a String With Their Consecutive Counts" $ do
        specEval
            "ĉʲ{ᵉĜ#Mĝ"
            [ ("\"1234500362000440\"", all_ ["1234523623441"])
            , ("\"123450036200044\"", all_ ["123452362344"])
            , ("\"000000000000\"", all_ ["12"])
            , ("\"0123456789\"", all_ ["1123456789"])
            , ("\"1234567890\"", all_ ["1234567891"])
            , ("\"123456789\"", all_ ["123456789"])
            , ("\"010203004050\"", all_ ["11121324151"])
            ]
    describe "q256017: CGAC2022 Day 25: When The Planets Align" $ do
        specEval
            "ᵏ{*+$/1%≡"
            [ ("[0,0] [0,0] [1,1]", all_ ["0"])
            , ("[1,0] [1,0] [100,100]", all_ ["99"])
            , ("[1,5,3] [0,1,0] [4,8,12]", all_ ["5"])
            ]
    describe "q256034: Normal Subgroups of S4" $ do
        specEval
            "@ᵃ{x-¬∑}ä-"
            [ ("[0,1,2,3]", all_ ["2"])
            , ("[0,1,3,2]", all_ ["0"])
            , ("[0,2,1,3]", all_ ["0"])
            , ("[0,2,3,1]", all_ ["1/2"])
            , ("[0,3,1,2]", all_ ["1/2"])
            , ("[0,3,2,1]", all_ ["0"])
            , ("[1,0,2,3]", all_ ["0"])
            , ("[1,0,3,2]", all_ ["-2"])
            , ("[1,2,0,3]", all_ ["1/2"])
            , ("[1,2,3,0]", all_ ["0"])
            , ("[1,3,0,2]", all_ ["0"])
            , ("[1,3,2,0]", all_ ["1/2"])
            ]
    describe "q256147: Find the Prime Signature" $ do
        specEval
            "ƒo↔"
            [ ("1", all_ ["[]"])
            , ("2", all_ ["[1]"])
            , ("4", all_ ["[2]"])
            , ("6", all_ ["[1,1]"])
            , ("8", all_ ["[3]"])
            , ("12", all_ ["[2,1]"])
            , ("16", all_ ["[4]"])
            , ("24", all_ ["[3,1]"])
            , ("30", all_ ["[1,1,1]"])
            , ("32", all_ ["[5]"])
            , ("36", all_ ["[2,2]"])
            , ("1234567", all_ ["[1,1]"])
            , ("5174928", all_ ["[5,4,3]"])
            , ("8388608", all_ ["[23]"])
            , ("9999991", all_ ["[1]"])
            ]
    describe "q256502: Guess the song title" $ do
        specEval
            "ŢṂaş"
            [("[\"Hello, world\",\"Hello, world\",\"I just got to say it, hello world\",\"Goodbye, world\",\"Goodbye, world\",\"Goodbye\"]", all_ ["Hello, world"])]
    describe "q256814: Knight to fork!" $ do
        specEval
            "ᵐ{į→ŋ+}≡H"
            [ ("[\"d6\",\"f6\",\"g3\"]", all_ ["e4"])
            , ("[\"d4\",\"d6\",\"e7\"]", all_ ["f5"])
            , ("[\"c3\",\"f2\",\"b2\"]", all_ ["d1"])
            ]
    describe "q256920: Simplify a Cycle" $ do
        specEval
            "Cᶦ{ᵈsf¡C"
            [ ("[1,2,3,4,2,5]", all_ ["1", "2", "5"])
            , ("[4,3,6,2,3,8,5,2,8,7]", all_ ["4", "3", "8", "7"])
            , ("[1,1,2]", all_ ["1", "2"])
            , ("[1,2,7,2,7,2,3,7]", all_ ["1", "2", "3", "7"])
            ]
    describe "q256978: Painting with Line Filler" $ do
        specEval
            "ʷ{ᶜŤĕ≡¿}Ø="
            [ ("[[1,1,1,1,1],[2,3,4,5,6],[2,3,4,5,6]]", Check True)
            , ("[[1,1,1,1,1],[2,3,4,1,6],[2,3,4,1,6]]", Check True)
            , ("[[1,1,1,1,6],[2,3,4,1,6],[2,3,4,1,6]]", Check True)
            , ("[[1,1],[1,1]]", Check True)
            , ("[[1,2],[2,1]]", Check False)
            , ("[[1,2],[3,4]]", Check False)
            , ("[[1,1,2],[4,5,2],[4,3,3]]", Check False)
            , ("[[1,2,4],[2,1,4],[3,3,3]]", Check False)
            ]
    describe "q257631: Time to shortest permutation" $ do
        specEval
            "↕ᵃ{2Šᵐɗ\"∩<\"<60b}-_På"
            [ ("[1,1,4,3]", all_ ["91"])
            , ("[0,1,0,1]", all_ ["9"])
            , ("[1,7,3,8]", all_ ["59"])
            , ("[1,4,2,1]", all_ ["413"])
            , ("[1,3,2,0]", all_ ["413"])
            , ("[2,3,4,1]", all_ [])
            , ("[0,0,0,0]", all_ [])
            ]
    describe "q257649: Arbitrary Apple Dilemma" $ do
        specEval
            ":←/∏*"
            [ ("[3,4,6] 10", all_ ["24"])
            , ("[2] 14", all_ ["28"])
            , ("[6] 30", all_ ["36"])
            , ("[4,3], 20", all_ ["40"])
            , ("[5,8,7], 9", all_ ["15"])
            , ("[2,9,4,8], 7", all_ ["24"])
            ]
    describe "q257372: Smallest Bit Rotation" $ do
        specEval
            "ƂxŘƃṁ"
            [ ("177", all_ ["27"])
            , ("1", all_ ["1"])
            , ("12", all_ ["3"])
            , ("23", all_ ["15"])
            , ("34", all_ ["5"])
            , ("45", all_ ["27"])
            , ("56", all_ ["7"])
            , ("67", all_ ["7"])
            , ("78", all_ ["29"])
            , ("89", all_ ["27"])
            , ("100", all_ ["19"])
            ]
    describe "q257458: Sum of Consecutive Squares" $ do
        specEval
            "qŁ:∙="
            [ ("5", Check True)
            , ("13", Check True)
            , ("14", Check True)
            , ("25", Check True)
            , ("29", Check True)
            , ("4", Check False)
            , ("12", Check False)
            , ("15", Check False)
            , ("16", Check False)
            , ("26", Check False)
            ]
    describe "q257752: Print all pandigital numbers" $ do
        specEval
            "Ňᵖ{*$Bu$L"
            [ ("2", truncate_ ["1", "2", "3", "4", "5", "6"])
            , ("3", truncate_ ["5", "7", "11", "14", "15", "16", "17", "19"])
            , ("4", truncate_ ["27", "30", "39", "45"])
            ]
        specEval
            "Ňᵖ{$Bçu#="
            [ ("2", truncate_ ["1", "2", "3", "4", "5", "6"])
            , ("3", truncate_ ["5", "7", "11", "14", "15", "16", "17", "19"])
            , ("4", truncate_ ["27", "30", "39", "45"])
            ]
    describe "q257998: Recognize a counting tree" $ do
        specEval
            "qCᵉL↔≤a*$h→L"
            [ ("[5,2,0,0,0,0]", Check True)
            , ("[5,2,1,0,0,0]", Check True)
            , ("[5,2,1,0,1,0]", Check True)
            , ("[5,3,1,0,0,0]", Check True)
            , ("[6,5,4,3,2,1,0]", Check True)
            , ("[0]", Check True)
            , ("[0,0,0,0,0,0]", Check False)
            , ("[5,2,1,1,0,0]", Check False)
            , ("[5,2,0,1,0,0]", Check False)
            , ("[5,3,1,0,1,0]", Check False)
            , ("[6,2,1,0,0,0]", Check False)
            , ("[5,3,1,0,0,1]", Check False)
            , ("[5,2,3,0,0,0]", Check False)
            ]
    describe "q258110: A Fine sequence with fine interpretations" $ do
        specEval
            "→ᵉ_rÇ∫µA"
            [ ("0", all_ ["1"])
            , ("1", all_ ["0"])
            , ("2", all_ ["1"])
            , ("3", all_ ["2"])
            , ("4", all_ ["6"])
            , ("5", all_ ["18"])
            , ("6", all_ ["57"])
            , ("7", all_ ["186"])
            , ("8", all_ ["622"])
            , ("9", all_ ["2120"])
            ]
    describe "q258299: Primes with Distinct Prime Digits" $ do
        specEval
            "ƤƊQůɗ"
            [("", truncate_ ["2", "3", "5", "7", "23", "37", "53", "73", "257", "523", "2357", "2753", "3257", "3527", "5237", "5273", "7253", "7523"])]
        specEval
            "¢SQ↕ɗQao"
            [("", all_ ["[2,3,5,7,23,37,53,73,257,523,2357,2753,3257,3527,5237,5273,7253,7523]"])]
    describe "q258335: Shortest Code to Find the Smallest Missing Positive Integer" $ do
        specEval
            "ŇPᵖf"
            [ ("[1,2,3]", first_ "4")
            , ("[3,4,-1,1]", first_ "2")
            , ("[7,8,9,11,12]", first_ "1")
            , ("[-5,-4,-3,-2,-1,0,1,2,3,5,7,10]", first_ "4")
            , ("[]", first_ "1")
            , ("[-1,-4,-7]", first_ "1")
            ]
    describe "q258432: Shortest code to generate all Pythagorean triples up to a given limit" $ do
        specEval
            "RSᵖ{:*Ɔ$đ+="
            [ ("15", all_ ["[3,4,5]", "[6,8,10]", "[5,12,13]", "[9,12,15]"])
            , ("5", all_ ["[3,4,5]"])
            ]
        specEval
            "RS2L::∙√ɔ$≤"
            [ ("15", all_ ["[3,4,5]", "[6,8,10]", "[5,12,13]", "[9,12,15]"])
            , ("5", all_ ["[3,4,5]"])
            ]
    describe "q258511: Longest Valid Parentheses" $ do
        specEval
            "q£E→∫x>çƆᵖLÅ"
            [ ("\"(()())\"", all_ ["6"])
            , ("\")()())\"", all_ ["4"])
            , ("\"()(())\"", all_ ["6"])
            , ("\"()(()\"", all_ ["2"])
            , ("\"))\"", all_ ["0"])
            , ("\"\"", all_ ["0"])
            ]
    describe "q258951: \"Sort\" by element duplication" $ do
        specEval
            "ᶦ{:Ɔ≥$tI}ṁ"
            [ ("[4,3,1,2]", all_ ["1", "1", "1", "2"])
            , ("[1,2,3,4]", all_ ["1", "2", "3", "4"])
            , ("[3,2,1,0]", all_ ["0", "1", "2", "3"])
            , ("[1,2,3,1]", all_ ["1", "1", "2", "3"])
            , ("[101,103,101,105]", all_ ["101", "101", "101", "105"])
            ]
    describe "q258992: Guessing on straws" $ do
        specEval
            "SjŢ½ađ"
            [ ("[[1,2],[1,2]]", Check True)
            , ("[[1,3],[1,3],[2,4],[2,4]]", Check False)
            , ("[[1,3],[2,4],[2,3],[1,4]]", Check True)
            , ("[[1,5],[1,6],[2,6],[2,7],[3,5],[3,8],[4,8],[4,5]]", Check True)
            ]
    describe "q259083: Is it traversable?" $ do
        specEval
            "R↔$∆çᴶ{CᵈAc}-ň"
            [ ("0 [1,1,1,1,1]", Check True)
            , ("0 [50,45,20,19,18,10,1,1,1]", Check True)
            , ("5 [1,6,11,16,21,26,31]", Check True)
            , ("100 [500,1,100]", Check True)
            , ("45 [20,50]", Check True)
            , ("4 [6,2,1,2,5,6,1,2,3,5,1,1,1,3]", Check True)
            , ("17 [59,61,47,64,23,34,21,22,25,29,25]", Check True)
            , ("4 [6,2,1,2,5,6,1,2,3,5,1,1,1,4]", Check False)
            , ("0 [1,1,2,1,1]", Check False)
            , ("5 [30,28,22,18,13,9,7,9,11,14,22,23]", Check False)
            , ("6 [40,47,49,55,61,66,69,70,50,55]", Check False)
            , ("45 [79,48,41,70,76,85,27,12,31,66,13,17,94,77]", Check False)
            , ("31 [65,21,20,32,9,9,37,14,23,19,32,63]", Check False)
            ]
    describe "q259143: Is it a valid chemical?" $ do
        specEval
            "∑½$ṁ±*$:#←c≥"
            [ ("[1]", Check False)
            , ("[4]", Check False)
            , ("[1,1,1,1]", Check False)
            , ("[0,0]", Check False)
            , ("[1,3]", Check False)
            , ("[2,3]", Check False)
            , ("[1,1,1,1,1,3]", Check False)
            , ("[1,1]", Check True)
            , ("[4,2,2]", Check True)
            , ("[3,3]", Check True)
            , ("[2,2,2]", Check True)
            , ("[0]", Check True)
            , ("[3,2,1]", Check True)
            , ("[3,3,3,3]", Check True)
            , ("[4,1,1,1,1,2,2,2]", Check True)
            , ("[4,2,2,1,1]", Check True)
            , ("[3,3,2,1,1]", Check True)
            ]
        specEval
            "ʷ{P↕1:Ð-:Cž¿?}žz"
            [ ("[1]", Check False)
            , ("[4]", Check False)
            , ("[1,1,1,1]", Check False)
            , ("[0,0]", Check False)
            , ("[1,3]", Check False)
            , ("[2,3]", Check False)
            , ("[1,1]", Check True)
            , ("[4,2,2]", Check True)
            , ("[3,3]", Check True)
            , ("[2,2,2]", Check True)
            , ("[0]", Check True)
            , ("[3,2,1]", Check True)
            , ("[3,3,3,3]", Check True)
            ]
    describe "q259167: How long to carry sort?" $ do
        specEval
            "õx-çṀ"
            [ ("[]", all_ ["0"])
            , ("[-2,3,9]", all_ ["0"])
            , ("[4,1,2,3]", all_ ["1"])
            , ("[1,3,2,4]", all_ ["1"])
            , ("[4,3,2,1]", all_ ["3"])
            , ("[0,-1,-2,-3,-4]", all_ ["4"])
            , ("[1,2,0,3,2,1,2,4,3]", all_ ["3"])
            ]
    describe "q259233: We're gonna need a bigger podium!" $ do
        specEval
            "O↕ᶠz$L"
            [ ("0 0", Count 1)
            , ("1 0", Count 0)
            , ("1 1", Count 1)
            , ("2 0", Count 1)
            , ("2 1", Count 0)
            , ("2 2", Count 2)
            , ("3 0", Count 1)
            , ("3 1", Count 6)
            , ("3 2", Count 0)
            , ("3 3", Count 6)
            , ("4 0", Count 7)
            , ("4 1", Count 8)
            , ("4 2", Count 36)
            , ("4 3", Count 0)
            , ("4 4", Count 24)
            , ("5 0", Count 21)
            , ("5 1", Count 100)
            , ("5 2", Count 60)
            , ("5 3", Count 240)
            , ("5 4", Count 0)
            , ("5 5", Count 120)
            ]
    describe "q259576: All of the Boards" $ do
        specEval
            "į3ƂÐ3~ᵑᵐç3~ᵑçaᵐᶜ{0*}∑ƶ"
            [("", Count 215)]
    describe "q259633: Make a Custom Bayer Matrix" $ do
        specEval
            "ᵒ{ᵃƂv≈Ä+ç4ŗd"
            [ ("1", all_ ["[[0]]"])
            , ("2", all_ ["[[0,1/2],[3/4,1/4]]"])
            , ("4", all_ ["[[0,1/2,1/8,5/8],[3/4,1/4,7/8,3/8],[3/16,11/16,1/16,9/16],[15/16,7/16,13/16,5/16]]"])
            ]
        specEval
            "0UU$ᵑ{4*4ᵐ+↔3Ř;ᶻᶻ,j"
            [ ("0", all_ ["[[0]]"])
            , ("1", all_ ["[[0,2],[3,1]]"])
            , ("2", all_ ["[[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]"])
            ]
    describe "q259707: Shortest distinguishable slice" $ do
        specEval
            "ḞŤxqNᵖ{@Ťů}aşᵉhl→Ð"
            [ ("[\"happy\",\"angry\",\"hungry\"]", first_ "[1,2]")
            , ("[\"sheer\",\"shrew\",\"shine\",\"shire\",\"spike\",\"shy\"]", first_ "[2,4]")
            , ("[\"snap\",\"crackle\",\"pop\",\"smack\",\"sizzle\",\"whiff\",\"sheen\"]", first_ "[0,2]")
            ]
    describe "q259881: The Jaccard Index" $ do
        specEval
            ",Ţ←µ"
            [ ("[1,2] []", all_ ["0"])
            , ("[-7,3,-9] [9,2,3,4]", all_ ["1/6"])
            , ("[1,2,3] [2,4,6]", all_ ["1/5"])
            , ("[0,64] [0,64,89,93]", all_ ["1/2"])
            , ("[6,42,7,1] [42,7,6]", all_ ["3/4"])
            , ("[3,6,9] [3,6,9]", all_ ["1"])
            ]
        specEval
            "ᵋ∩Ŭᵃ#/"
            [ ("[1,2] []", all_ ["0"])
            , ("[-7,3,-9] [9,2,3,4]", all_ ["1/6"])
            , ("[1,2,3] [2,4,6]", all_ ["1/5"])
            , ("[0,64] [0,64,89,93]", all_ ["1/2"])
            , ("[6,42,7,1] [42,7,6]", all_ ["3/4"])
            , ("[3,6,9] [3,6,9]", all_ ["1"])
            ]
    describe "q259875: How Super is this Prime?" $ do
        specEval
            "ˡ{Qƥ}Ƃ#←"
            [ ("2", all_ ["0"])
            , ("3", all_ ["1"])
            , ("11", all_ ["2"])
            , ("211", all_ ["1"])
            , ("277", all_ ["2"])
            , ("823", all_ ["0"])
            , ("4397", all_ ["2"])
            , ("5381", all_ ["3"])
            , ("171697", all_ ["2"])
            , ("499403", all_ ["2"])
            , ("648391", all_ ["3"])
            ]
    describe "q259987: String Comparison" $ do
        specEval
            "↔ᵃjĻ"
            [ ("[\"a\",\"b\"]", Check True)
            , ("[\"ac\",\"a\"]", Check False)
            , ("[\"bekcka\",\"kwnfoe\"]", Check True)
            , ("[\"beztbest\",\"bestbe\"]", Check False)
            , ("[\"mcjaf\",\"mc\"]", Check True)
            , ("[\"akboe\",\"uenvi\"]", Check True)
            ]
    describe "q260120: Alternating factorial" $ do
        specEval
            "RF£b"
            [ ("0", all_ ["0"])
            , ("1", all_ ["1"])
            , ("2", all_ ["1"])
            , ("3", all_ ["5"])
            , ("4", all_ ["19"])
            , ("5", all_ ["101"])
            , ("6", all_ ["619"])
            , ("7", all_ ["4421"])
            , ("8", all_ ["35899"])
            , ("9", all_ ["326981"])
            ]
    describe "q260198: String Concatenate" $ do
        specEval
            "Ṗ↕rj@ũ"
            [ ("8 \"aaaaa\"", Count 1)
            , ("4 \"abcde\"", Count 8)
            , ("5 \"abcdef\"", Count 16)
            ]
        specEval
            "ᵚpj§Lũ"
            [ ("\"aaaaa\" 8", Count 1)
            , ("\"abcde\" 4", Count 8)
            , ("\"abcdef\" 5", Count 16)
            ]
    describe "q260266: The number of solutions to Hertzsprung's Problem" $ do
        specEval
            "↕∆Ƶ"
            [ ("1", Count 1)
            , ("2", Count 0)
            , ("3", Count 0)
            , ("4", Count 2)
            , ("5", Count 14)
            , ("6", Count 90)
            ]
    describe "q260302: Is it a plausible chess move?" $ do
        specEval
            "≈Z‼2M≡"
            [ ("[97,49] [97,52]", Check True)
            , ("[98,50] [100,51]", Check True)
            , ("[98,50] [101,51]", Check False)
            , ("[98,50] [99,51]", Check True)
            , ("[97,49] [104,56]", Check True)
            , ("[99,55] [103,51]", Check True)
            , ("[99,55] [103,50]", Check False)
            , ("[102,52] [98,56]", Check True)
            ]
    describe "q260370: Counting fading" $ do
        specEval
            "ᵏᵚ{ᵑ{įŋ+}f"
            [ ("[]", all_ ["0"])
            , ("[[1,1]]", all_ ["1"])
            , ("[[1,2],[2,1],[2,2],[2,3],[3,2]]", all_ ["2"])
            ]
    describe "q260472: Find Index of Rational Number in Calkin-Wilf Sequence" $ do
        specEval
            "ˡ{ŗ←£þ+_"
            [ ("1", all_ ["1"])
            , ("1/3", all_ ["4"])
            , ("4/3", all_ ["9"])
            , ("3/4", all_ ["14"])
            , ("53/37", all_ ["1081"])
            , ("37/53", all_ ["1990"])
            ]
    describe "q260804: Minkowski's ?(x) for rational x" $ do
        specEval
            "ᶦ{1%ŗ}kaC$∫←:_Ë£d§+ṇ$çlÐ"
            [ ("0/1", all_ ["[0,0]"])
            , ("1/1", all_ ["[1,0]"])
            , ("1/2", all_ ["[1,1]"])
            , ("-1/2", all_ ["[-1,1]"])
            , ("2/1", all_ ["[2,0]"])
            , ("1/3", all_ ["[1,2]"])
            , ("1/8", all_ ["[1,7]"])
            , ("2/5", all_ ["[3,3]"])
            , ("8/5", all_ ["[13,3]"])
            , ("58/27", all_ ["[1033,9]"])
            , ("30/73", all_ ["[399,10]"])
            , ("144/89", all_ ["[853,9]"])
            , ("-17/77", all_ ["[-767,13]"])
            , ("-17/99", all_ ["[-133,12]"])
            , ("355/113", all_ ["[12648447,22]"])
            , ("16000/1", all_ ["[16000,0]"])
            ]
    describe "q260811: Given 4 fence lengths, what's the largest rectangular yard you can make?" $ do
        specEval
            "o;*ṁ"
            [ ("[1,1,1,1]", all_ ["1"])
            , ("[1,2,3,4]", all_ ["3"])
            , ("[4,3,2,1]", all_ ["3"])
            , ("[90,1,2,1]", all_ ["2"])
            , ("[1,90,1,1]", all_ ["1"])
            , ("[44,51,50,36]", all_ ["1800"])
            , ("[3,3,3,3]", all_ ["9"])
            , ("[3,3,3,4]", all_ ["9"])
            , ("[3,4,3,4]", all_ ["12"])
            , ("[4,4,3,4]", all_ ["12"])
            , ("[4,4,4,4]", all_ ["16"])
            ]
        specEval
            "oĭ$∏"
            [ ("[1,1,1,1]", all_ ["1"])
            , ("[1,2,3,4]", all_ ["3"])
            , ("[4,3,2,1]", all_ ["3"])
            , ("[90,1,2,1]", all_ ["2"])
            , ("[1,90,1,1]", all_ ["1"])
            , ("[44,51,50,36]", all_ ["1800"])
            , ("[3,3,3,3]", all_ ["9"])
            , ("[3,3,3,4]", all_ ["9"])
            , ("[3,4,3,4]", all_ ["12"])
            , ("[4,4,3,4]", all_ ["12"])
            , ("[4,4,4,4]", all_ ["16"])
            ]
    describe "q260966: sum of a range of a sum of a range of n" $ do
        specEval
            "R∑R∑"
            [ ("1", all_ ["1"])
            , ("2", all_ ["6"])
            , ("3", all_ ["21"])
            , ("4", all_ ["55"])
            , ("5", all_ ["120"])
            , ("6", all_ ["231"])
            , ("7", all_ ["406"])
            , ("8", all_ ["666"])
            , ("9", all_ ["1035"])
            , ("10", all_ ["1540"])
            ]
    describe "q261325: Output endless powers of 2" $ do
        specEval
            "ŇË"
            [("", truncate_ ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"])]
    describe "q261861: Lowest digit addition generator" $ do
        specEval
            "ᵏ{:Ɗ∑+="
            [ ("0", all_ ["0"])
            , ("29", all_ ["19"])
            , ("216", all_ ["198"])
            ]
    describe "q261908: Last odd digit of power of 2" $ do
        specEval
            "Ë¢BÖ1Ĩ"
            [ ("1", nothing_)
            , ("2", nothing_)
            , ("3", nothing_)
            , ("4", first_ "1")
            , ("5", first_ "1")
            , ("6", nothing_)
            , ("7", first_ "2")
            , ("8", first_ "1")
            , ("9", first_ "1")
            , ("10", first_ "3")
            ]
    describe "q262032: Vertices of a regular dodecahedron" $ do
        specEval
            "7Ƃ89\\55:ŗÐçxŘ~?ŋ"
            [ ("", truncate_ ["[1,1,1]", "[1,1,-1]", "[1,-1,1]", "[1,-1,-1]"])
            , ("", Count 20)
            ]
    describe "q262140: XOR of independent Bernoulli variables" $ do
        specEval
            "Ä←_∏←_ä"
            [ ("[123/1000]", all_ ["123/1000"])
            , ("[123/1000, 1/2]", all_ ["1/2"])
            , ("[0,0,1,1,0,1]", all_ ["1"])
            , ("[0,0,1,1,0,1,1/2]", all_ ["1/2"])
            , ("[3/4,3/4]", all_ ["3/8"])
            , ("[3/4,3/4,3/4]", all_ ["9/16"])
            ]
    describe "q262159: Minimum partition with non-empty intersections" $ do
        specEval
            "Oᵖᵐ{Ťđṁ<}aş"
            [ ("[]", all_ ["[]"])
            , ("[[0,1]]", all_ ["[[[0,1]]]"])
            , ("[[0,1],[2,3]]", all_ ["[[[2,3]],[[0,1]]]"])
            , ("[[1,2],[0,3]]", all_ ["[[[1,2],[0,3]]]"])
            , ("[[0,2],[1,3]]", all_ ["[[[0,2],[1,3]]]"])
            , ("[[1,2],[3,4],[0,5]]", all_ ["[[[3,4],[0,5]],[[1,2]]]", "[[[1,2],[0,5]],[[3,4]]]"])
            , ("[[0,1],[4,5],[2,3]]", all_ ["[[[2,3]],[[4,5]],[[0,1]]]"])
            , ("[[4,5],[1,2],[0,3]]", all_ ["[[[1,2],[0,3]],[[4,5]]]"])
            , ("[[0,1],[2,5],[3,4]]", all_ ["[[[2,5],[3,4]],[[0,1]]]"])
            , ("[[0,2],[3,5],[1,4]]", all_ ["[[[3,5],[1,4]],[[0,2]]]", "[[[0,2],[1,4]],[[3,5]]]"])
            ]
    describe "q262347: Reversed Squares" $ do
        specEval
            "Ɗ¢dÐ√"
            [ ("1", Check True)
            , ("4", Check True)
            , ("9", Check True)
            , ("441", Check True)
            , ("1234567654321", Check True)
            , ("100", Check True)
            , ("3", Check False)
            , ("25", Check False)
            , ("1784", Check False)
            ]
    describe "q262512: Generate all linked chains" $ do
        specEval
            "\"-_\"ŇŧĉŁ'=ᵚcjt"
            [("", truncate_ ["-=_", "_=-", "--=_", "-=_=-", "-=__", "_=--", "_=-=_", "__=-"])]
    describe "q262518: Landmine Number I" $ do
        specEval
            "ᵉpttᵋ+*:,,$Ĉ"
            [ ("[1,8,3,7,1] 4", all_ ["2"])
            , ("[2,9,2,8,0,6,4] 4", all_ ["4"])
            , ("[3,3,3,3,3] 9", all_ ["6"])
            , ("[9,9,9,9,9,9,9,9,9,9] 100", all_ ["0"])
            , ("[1,2,3,4,5,6,7,8,9,0] 8", all_ ["4"])
            , ("[2,2,2,2,2] 4", all_ ["9"])
            , ("[0,0,0,0,0,0,0,0,0,0,0,0,0,0] 0", all_ ["36"])
            , ("[1,2,9,5,1] 10", all_ ["4"])
            , ("[1,2,3,1,2,3,1,2,3,1,2,3] 3", all_ ["11"])
            , ("[8,2,8,8,2,8,8,2,8] 16", all_ ["11"])
            ]
    describe "q262588: Measure the Diamond Road" $ do
        specEval
            "~pᵉloÐũ"
            [ ("[\"/\",\"\\\\\",\"/\",\"\\\\\"]", Count 2)
            , ("[\"/\\\\/\",\"\\\\/\\\\\",\"///\"]", Count 8)
            , ("[\"////\",\"\\\\/\\\\/\",\"/\\\\/\\\\\",\"//\\\\\\\\\"]", Count 12)
            , ("[\"/\\\\\",\"/\\\\/\",\"/\\\\/\\\\\",\"/\\\\/\\\\/\",\"/\\\\/\\\\/\\\\\",\"/\\\\/\\\\/\\\\/\\\\\"]", Count 8)
            , ("[\"/\\\\//\\\\//\\\\/\",\"\\\\/\\\\/\\\\\\\\//\\\\\",\"\\\\//\\\\/\\\\/\\\\/\",\"\\\\//\\\\//\\\\//\",\"/\\\\/\\\\/\\\\/\",\"\\\\/\",\"\\\\\\\\\\\\\",\"\\\\//\\\\\",\"\\\\\"]", Count 28)
            , ("[\"/\"]", Count 1)
            , ("[\"\\\\\",\"\\\\/\\\\\"]", Count 3)
            , ("[\"/\\\\\",\"\\\\/\\\\/\\\\/\"]", Count 8)
            , ("[\"/\\\\\",\"/\",\"\\\\/\\\\\",\"\\\\\"]", Count 5)
            , ("[\"////////////////////\"]", Count 20)
            , ("[\"//////////\",\"//////////\"]", Count 10)
            , ("[\"//////////\",\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"]", Count 20)
            , ("[\"\\\\\",\"/\\\\\",\"\\\\/\",\"/\\\\\",\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"]", Count 13)
            , ("[\"\\\\/\\\\\",\"/\\\\/\",\"//\\\\\",\"\\\\\\\\/\",\"/\\\\\",\"/\\\\\",\"/\",\"\\\\\"]", Count 10)
            , ("[\"/\\\\\",\"\\\\/\",\"/\\\\\",\"/\\\\\",\"\\\\/\",\"/\\\\\",\"/\\\\\",\"\\\\/\",\"\\\\/\",\"\\\\/\",\"/\\\\\",\"/\\\\\"]", Count 4)
            , ("[\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"\\\\\",\"/\"]", Count 2)
            ]
    describe "q262647: Sum of a range of a sum of a range of a sum of a range of a sum of a range of a sum of" $ do
        specEval
            "ᵑ{R∑"
            [ ("0", all_ ["0"])
            , ("1", all_ ["1"])
            , ("2", all_ ["6"])
            , ("3", all_ ["231"])
            , ("4", all_ ["1186570"])
            ]
    describe "q262809: Diagonalize a vector" $ do
        specEval
            "xᵐĦ*Ḟ"
            [ ("[]", all_ ["[]"])
            , ("[0]", all_ ["[[0]]"])
            , ("[1]", all_ ["[[1]]"])
            , ("[1,2,3]", all_ ["[[1,0,0],[0,2,0],[0,0,3]]"])
            , ("[1,0,2,3]", all_ ["[[1,0,0,0],[0,0,0,0],[0,0,2,0],[0,0,0,3]]"])
            ]
    describe "q262868: Chamber of Reflection" $ do
        specEval
            "Ðᵒ÷u#←"
            [ ("5 4 11", all_ ["4"])
            , ("1 1 10", all_ ["9"])
            , ("100 100 1", all_ ["0"])
            , ("3 2 9", all_ ["5"])
            , ("6 3 18", all_ ["5"])
            , ("1 1 100", all_ ["99"])
            , ("2398 2308 4", all_ ["0"])
            , ("500 10000 502", all_ ["1"])
            ]
    describe "q262985: Numbers that can be negated by reading backwards" $ do
        specEval
            "3Ňŧ←1cƀ_=3b"
            [("", truncate_ ["2", "8", "20", "26", "32", "56", "80", "104", "146", "164"])]
    describe "q263200: Print all Polynomials" $ do
        specEval
            "Ňƒ$ƥ←$yĦŋ"
            [("", truncate_ ["0", "[1]", "[-1]", "[0,1]", "[0,-1]", "[2]", "[-2]", "[0,0,1]", "[0,0,-1]"])]
    describe "q263308: Make a k-skip-j range" $ do
        specEval
            "+ᵚ%-±ç1Ĩ"
            [ ("1 1 11", all_ ["1", "3", "5", "7", "9", "11"])
            , ("2 13 19", all_ ["1", "2", "16", "17"])
            , ("2 13 16", all_ ["1", "2", "16"])
            , ("1 4 49", all_ ["1", "6", "11", "16", "21", "26", "31", "36", "41", "46"])
            , ("2 4 22", all_ ["1", "2", "7", "8", "13", "14", "19", "20"])
            , ("2 10 13", all_ ["1", "2", "13"])
            , ("5 15 10", all_ ["1", "2", "3", "4", "5"])
            , ("1 13 27", all_ ["1", "15"])
            , ("7 4 31", all_ ["1", "2", "3", "4", "5", "6", "7", "12", "13", "14", "15", "16", "17", "18", "23", "24", "25", "26", "27", "28", "29"])
            , ("99 99 1", all_ ["1"])
            ]
    describe "q263364: Compute this fractal matrix" $ do
        specEval
            "Ë:ᵒ&Þ←A±"
            [ ("1", all_ ["[[1,1],[1,0]]"])
            , ("2", all_ ["[[1,1,1,1],[1,0,1,0],[1,1,0,0],[1,0,0,1]]"])
            , ("3", all_ ["[[1,1,1,1,1,1,1,1],[1,0,1,0,1,0,1,0],[1,1,0,0,1,1,0,0],[1,0,0,1,1,0,0,1],[1,1,1,1,0,0,0,0],[1,0,1,0,0,1,0,1],[1,1,0,0,0,0,1,1],[1,0,0,1,0,1,1,1]]"])
            ]
    describe "q263438: Is this a powerful number?" $ do
        specEval
            "ƒƵ"
            [ ("1", Check True)
            , ("2", Check False)
            , ("3", Check False)
            , ("4", Check True)
            , ("5", Check False)
            , ("6", Check False)
            , ("7", Check False)
            , ("8", Check True)
            , ("9", Check True)
            ]
    describe "q263496: Can you perform swaps?" $ do
        specEval
            "Ťᵐ↕∫Ɔž∑≤"
            [ ("[[-1],[1]]", Check False)
            , ("[[-1,1],[-1,1]]", Check True)
            , ("[[-1,-1,-1,1,1,1],[-1,1,-1,1,-1,1]]", Check True)
            , ("[[-1,1,-1,1,1,1],[-1,-1,-1,1,-1,1]]", Check True)
            , ("[[1,1,1,-1,-1,-1],[-1,-1,-1,1,1,1]]", Check False)
            , ("[[1,1,1,1,1,1],[1,1,1,1,1,1]]", Check False)
            , ("[[1,1,1,1,1,1,1],[1,1,1,1,1,1,1]]", Check False)
            , ("[[-1,1,1,1,1,1,1],[-1,-1,-1,-1,-1,-1,1]]", Check False)
            ]
    describe "q263566: Calculating Transitive Closure" $ do
        specEval
            "~ᵉʸ{ˣ@j,u}∕~"
            [ ("[[],[],[]]", Count 0)
            , ("[[1],[0]]", Count 2)
            , ("[[1],[0],[]]", Count 2)
            , ("[[1],[0,2],[]]", Count 3)
            , ("[[2],[],[1,0],[2]]", Count 5)
            , ("[[1,2,3],[2,3],[3],[]]", Count 0)
            , ("[[1],[2],[3],[4],[5],[0]]", Count 30)
            ]
        specEval
            "xᵒĈ:ʸ{ˣᵐ∙+±}≈j∑"
            [ ("[[],[],[]]", all_ ["0"])
            , ("[[1],[0]]", all_ ["2"])
            , ("[[1],[0],[]]", all_ ["2"])
            , ("[[1],[0,2],[]]", all_ ["3"])
            , ("[[2],[],[1,0],[2]]", all_ ["5"])
            , ("[[1,2,3],[2,3],[3],[]]", all_ ["0"])
            , ("[[1],[2],[3],[4],[5],[0]]", all_ ["30"])
            ]
    describe "q263627: Is this set laminar?" $ do
        specEval
            "ᵒ{ᵋ∩?Ø?="
            [ ("[]", Check True)
            , ("[[1,2,3],[1,2],[1],[]]", Check True)
            , ("[[1,2,3,4],[1,2,3],[1,2],[3],[4]]", Check True)
            , ("[[1,2,3,4],[1,2],[3,4],[1],[3]]", Check True)
            , ("[[1,2,3,4],[1,3],[2,4],[1],[2],[3],[4],[]]", Check True)
            , ("[[1,2,3],[4,5],[7,8],[3],[5],[7]]", Check True)
            , ("[[1,2],[2,3]]", Check False)
            , ("[[1,2,3,4],[1,2,3],[1,2],[3,4]]", Check False)
            , ("[[1,2,3,4],[1,2],[3,4],[2,3],[1],[2],[3],[4]]", Check False)
            , ("[[1,2,3,4,5],[1,2,3],[4,5,6]]", Check False)
            ]
    describe "q263678: The all-high powerful numbers" $ do
        specEval
            "Ňᵖ{Rƒᵐ∏Ɔ<"
            [("", truncate_ ["1", "4", "8", "16", "32", "64", "128", "144", "216", "288", "432", "864"])]
    describe "q263774: Round up to a smoother number" $ do
        specEval
            "Ň+ᵖ{ʷ½≥"
            [ ("101 100", first_ "102")
            , ("201 100", first_ "204")
            , ("256 100", first_ "256")
            ]
    describe "q263910: Evenly spread values" $ do
        specEval
            "kŢᵉR→/+j"
            [("[1/10,2/10,14/10,23/10,24/10,25/10,32/10]", all_ ["[1/3,2/3,3/2,9/4,5/2,11/4,7/2]"])]
    describe "q264102: Iterate over all non-equivalent strings" $ do
        specEval
            "r:,↕ũ∆Zç∫:ux=¿'a+H"
            [ ("2", all_ ["abab"])
            , ("3", all_ ["abcabc", "abcacb", "abcbac", "abcbca", "abacbc"])
            ]
    describe "q264166: Count N-Rich Permutations of an Integer Sequence" $ do
        specEval
            "↕∆±ĉŜçṀ→="
            [ ("[1,2,3,4,5] 1", Count 1)
            , ("[1,2,3,4,5] 2", Count 69)
            , ("[1,2,3,4,5] 3", Count 41)
            , ("[1,2,3,4,5] 4", Count 8)
            , ("[1,2,3,4,5] 5", Count 1)
            , ("[1,2,2,3,3,4,5] 1", Count 4)
            , ("[1,2,2,3,3,4,5] 2", Count 2612)
            , ("[1,2,2,3,3,4,5] 3", Count 2064)
            , ("[1,2,2,3,3,4,5] 4", Count 336)
            , ("[1,2,2,3,3,4,5] 5", Count 24)
            , ("[1,2,2,3,3,4,5] 6", Count 0)
            , ("[1,2,2,3,3,4,5] 7", Count 0)
            ]
    describe "q264409: First odd then even indices" $ do
        specEval
            "ᶦ{ĭ,v≠"
            [ ("[1,2,3]", Count 2)
            , ("[1,2,3,4,3,2,1]", Count 3)
            , ("[1,2,3,4,5,6,7,8,9,10]", Count 6)
            , ("[1,2,3,4,5,6,7,8,9,10,11]", Count 10)
            , ("\"abab\"", Count 2)
            , ("\"Hello, World!\"", Count 12)
            , ("[1,0,1,1,0,0,0,1,1,1]", Count 6)
            , ("[1,0,0,1,1,1,0,0,0,0,1]", Count 10)
            , ("[1,1,1,1,1,1,1]", Count 1)
            ]
    describe "q264502: Otteretto Classic game scoring method" $ do
        specEval
            "pNᵉ#ĉ#*"
            [ ("\"A\"", all_ ["1"])
            , ("\"ABA\"", all_ ["1", "4", "9"])
            , ("\"DDDD\"", all_ ["1", "2", "3", "4"])
            , ("\"AABAA\"", all_ ["1", "2", "6", "12", "15"])
            , ("\"CABAC\"", all_ ["1", "4", "9", "16", "25"])
            , ("\"EBBBE\"", all_ ["1", "4", "6", "8", "15"])
            , ("\"DEBBBE\"", all_ ["1", "4", "9", "12", "15", "24"])
            , ("\"CCAABBA\"", all_ ["1", "2", "6", "8", "15", "18", "28"])
            ]
    describe "q264505: Repeat your program to print Fibonacci numbers" $ do
        specEval
            "ˣ+1I"
            [("", all_ ["1"])]
    describe "q264589: Multiply multivariate polynomials" $ do
        specEval
            "×"
            [ ("[1,1,1] [1,1,1]", all_ ["[1,2,3,2,1]"])
            , ("[[0,1],[1]] [[0,1],[1]]", all_ ["[[0,0,1],[0,2],[1]]"])
            , ("[[[0,1],[1]],[[1]]] [[[0,1],[-1]],[[1]]]", all_ ["[[[0,0,1],[0,0],[-1]],[[0,2],[0]],[[1]]]"])
            , ("[[],[[[[[[[[[1]]]]]]]]]] [[],[[[[[[[[[1]]]]]]]]]]", all_ ["[[],[],[[[[[[[[[1]]]]]]]]]]"])
            ]
    describe "q264624: Is it a canonical elementary CA rule number?" $ do
        specEval
            "¥+Ƃiƀ¬?:Jĭ,Ťđ,?ƃa≤"
            [ ("11", Check True)
            , ("62", Check True)
            , ("74", Check True)
            , ("76", Check True)
            , ("108", Check True)
            , ("39", Check False)
            , ("91", Check False)
            , ("103", Check False)
            , ("143", Check False)
            , ("151", Check False)
            ]
    describe "q264733: Convert a Gaussian integer to its positive form" $ do
        specEval
            "±đZ≠nŘA"
            [ ("[0,0]", all_ ["[0,0]"])
            , ("[5,0]", all_ ["[5,0]"])
            , ("[0,2]", all_ ["[2,0]"])
            , ("[1,1]", all_ ["[1,1]"])
            , ("[1,-1]", all_ ["[1,1]"])
            , ("[3,-5]", all_ ["[5,3]"])
            , ("[-3,5]", all_ ["[5,3]"])
            , ("[3,5]", all_ ["[3,5]"])
            , ("[-3,-5]", all_ ["[3,5]"])
            ]
    describe "q264903: Order by Earliest Lower Digit" $ do
        specEval
            "ᶾ-¥b±"
            [ ("\"999\" \"999\"", all_ ["0"])
            , ("\"116\" \"115\"", all_ ["-1"])
            , ("\"115\" \"116\"", all_ ["1"])
            , ("\"789\" \"870\"", all_ ["1"])
            , ("\"3333\" \"33\"", all_ ["0"])
            , ("\"33\" \"3333\"", all_ ["0"])
            , ("\"2100\" \"20\"", all_ ["-1"])
            , ("\"1200\" \"19\"", all_ ["1"])
            , ("\"20\" \"2100\"", all_ ["1"])
            , ("\"19\" \"1200\"", all_ ["-1"])
            , ("\"1234\" \"1234\"", all_ ["0"])
            , ("\"5432\" \"4321\"", all_ ["-1"])
            , ("\"5432\" \"5678\"", all_ ["1"])
            , ("\"998\" \"99\"", all_ ["0"])
            , ("\"1233999999\" \"12345000\"", all_ ["1"])
            ]
    describe "q264918: Sum of consecutive nth powers" $ do
        specEval
            "←ᶠ{ᵈqEů∑="
            [ ("1", all_ ["[]"])
            , ("2", all_ ["[]"])
            , ("3", all_ ["[]"])
            , ("4", all_ ["[2]"])
            , ("5", all_ ["[]"])
            , ("6", all_ ["[2]"])
            , ("7", all_ ["[2]"])
            , ("8", all_ ["[2]"])
            , ("9", all_ ["[3]"])
            , ("10", all_ ["[]"])
            ]
        specEval
            "←ᶠ{ƵBƶ:o="
            [ ("1", all_ ["[]"])
            , ("2", all_ ["[]"])
            , ("3", all_ ["[]"])
            , ("4", all_ ["[2]"])
            , ("5", all_ ["[]"])
            , ("6", all_ ["[2]"])
            , ("7", all_ ["[2]"])
            , ("8", all_ ["[2]"])
            , ("9", all_ ["[3]"])
            , ("10", all_ ["[]"])
            ]
    describe "q265088: Longest Consecutive Sequence" $ do
        specEval
            "ˡ{N→v∩"
            [ ("[100,4,200,1,3,2]", all_ ["4"])
            , ("[1,2,3,4,5]", all_ ["5"])
            , ("[10,9,8,7,6,5]", all_ ["6"])
            , ("[1,1,1,2,3]", all_ ["3"])
            , ("[5]", all_ ["1"])
            , ("[7,3,9,5,4,2,8]", all_ ["4"])
            ]
    describe "q265382: Find the largest sum such that no two elements are touching" $ do
        specEval
            "Jĭ?∑hÅ"
            [ ("[1,2,3,4]", all_ ["6"])
            , ("[1,2,3,4,5]", all_ ["9"])
            , ("[2,2,1,1,2,1,1,2]", all_ ["7"])
            , ("[3,1,4,1,5,9,2]", all_ ["16"])
            , ("[9,8,7,9,9,8]", all_ ["26"])
            ]
    describe "q265768: Transpose a multidimensional array" $ do
        specEval
            "ʷ∑→ᵉbD"
            [ ("[[1]]", all_ ["[[1]]"])
            , ("[[[[1,2]]]]", all_ ["[[[[1],[2]]]]"])
            , ("[[1,2,3],[4,5,6]]", all_ ["[[1,4],[2,5],[3,6]]"])
            , ("[[[1,2],[3,4]],[[5,6],[7,8]]]", all_ ["[[[1,5],[2,6]],[[3,7],[4,8]]]"])
            , ("[[[[9]],[[10]]]]", all_ ["[[[[9]]],[[[10]]]]"])
            , ("[[[1,2,3],[4,5,6]],[[7,8,9],[10,11,12]]]", all_ ["[[[1,7],[2,8],[3,9]],[[4,10],[5,11],[6,12]]]"])
            ]
    describe "q265869: Diagonal Binary Sequence" $ do
        specEval
            "ᶦ{Ƃ2ŗɔƃ3M"
            [ ("1", truncate_ ["1", "3", "5", "9", "17", "33", "65", "129", "257", "513"])
            , ("2", truncate_ ["2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"])
            , ("3", truncate_ ["3", "5", "9", "17", "33", "65", "129", "257", "513", "1025"])
            , ("6", truncate_ ["6", "10", "18", "34", "66", "130", "258", "514", "1026", "2050"])
            , ("7", truncate_ ["7", "11", "19", "35", "67", "131", "259", "515", "1027", "2051"])
            , ("12", truncate_ ["12", "20", "36", "68", "132", "260", "516", "1028", "2052", "4100"])
            , ("31", truncate_ ["31", "47", "79", "143", "271", "527", "1039", "2063", "4111", "8207"])
            , ("89", truncate_ ["89", "153", "281", "537", "1049", "2073", "4121", "8217", "16409", "32793"])
            , ("111", truncate_ ["111", "175", "303", "559", "1071", "2095", "4143", "8239", "16431", "32815"])
            , ("1000", truncate_ ["1000", "1512", "2536", "4584", "8680", "16872", "33256", "66024", "131560", "262632"])
            ]
    describe "q265918: Divisor chain counts (1 3 3 7 ...)" $ do
        specEval
            "ĎSᵉti¦"
            [ ("1", Count 1)
            , ("2", Count 3)
            , ("3", Count 3)
            , ("4", Count 7)
            , ("5", Count 3)
            , ("6", Count 11)
            , ("7", Count 3)
            , ("8", Count 15)
            , ("9", Count 7)
            ]
    describe "q266049: How many umbrellas to cover the beach?" $ do
        specEval
            "ᴶ{x:ᵒ≈>~}aş#"
            [ ("[9,2,1,3,2,4,2,1] 1", first_ "1")
            , ("[1,1,1,1,1,1,1,1] 1", first_ "8")
            , ("[2,1,4,1,4,1,1,3,1] 1", first_ "2")
            , ("[5,1,3,1,3,1,1,1,6] 1", first_ "2")
            , ("[4,1,1,3,1,1] 1", first_ "2")
            ]
    describe "q266185: Expected number of rounds for this labeling scheme" $ do
        specEval
            "→r$ÇƆ/←ŗ0ɔ$ᵑ∆"
            [ ("1 1", all_ ["[1]"])
            , ("4 2", all_ ["[19/5]"])
            , ("6 2", all_ ["[97/14]"])
            , ("7 4", all_ ["[257/68]"])
            , ("7 7", all_ ["[1]"])
            , ("8 1", all_ ["[761/35]"])
            , ("8 5", all_ ["[951/275]"])
            , ("10 3", all_ ["[8241679/911064]"])
            , ("12 7", all_ ["[69968/16611]"])
            , ("20 2", all_ ["[645315821032049/18278449721532]"])
            ]
    describe "q266332: Compute the conjugate of a partition" $ do
        specEval
            "1D∑"
            [ ("[5,2,1]", all_ ["[3,2,1,1,1]"])
            , ("[4,3,1]", all_ ["[3,2,2,1]"])
            , ("[4,2,2]", all_ ["[3,3,1,1]"])
            , ("[3,3,2]", all_ ["[3,3,2]"])
            ]
        specEval
            "rjŢ"
            [ ("[5,2,1]", all_ ["[3,2,1,1,1]"])
            , ("[4,3,1]", all_ ["[3,2,2,1]"])
            , ("[4,2,2]", all_ ["[3,3,1,1]"])
            , ("[3,3,2]", all_ ["[3,3,2]"])
            ]
    describe "q266479: Find the fairest partition of a list" $ do
        specEval
            "ŋ∑Aå"
            [ ("[1,2,3]", all_ ["0"])
            , ("[2,3,5,7,11]", all_ ["0"])
            , ("[13,17,19,23]", all_ ["0"])
            , ("[1,2,3,4]", all_ ["0"])
            , ("[2,2,2,3,3]", all_ ["0"])
            , ("[1,2,3,4,5]", all_ ["1"])
            , ("[1,2,3,4,5,6]", all_ ["1"])
            , ("[1,1,2,5]", all_ ["1"])
            , ("[1,3,9,27]", all_ ["14"])
            ]
    describe "q266561: aaabbabbc" $ do
        specEval
            "ĕ<2ŠʳXH\"c\"I"
            [ ("\"cbbbaab\"", all_ ["aa"])
            , ("\"bbbaabc\"", all_ ["aa"])
            , ("\"baaacbb\"", all_ ["ab"])
            , ("\"bbcaaba\"", all_ ["ab"])
            , ("\"bbcabaa\"", all_ ["ba"])
            , ("\"abcaabb\"", all_ ["ba"])
            , ("\"bacaaab\"", all_ ["bb"])
            , ("\"aacabba\"", all_ ["bb"])
            , ("\"bbabaaba\"", all_ ["c"])
            , ("\"aaabbabb\"", all_ ["c"])
            ]
    describe "q266705: Monotone sequence beatitude" $ do
        specEval
            "∆±ɱ+"
            [ ("[7,4,3,2]", all_ ["-2"])
            , ("[6,5,5]", all_ ["-1"])
            , ("[0,0,0]", all_ ["0"])
            , ("[-1,2,2,2,4]", all_ ["1"])
            , ("[0,1,2]", all_ ["2"])
            ]
    describe "q266993: Rudin-Shapiro sequence" $ do
        specEval
            "Ä&Þ£E"
            [ ("0", all_ ["1"])
            , ("1", all_ ["1"])
            , ("2", all_ ["1"])
            , ("3", all_ ["-1"])
            , ("4", all_ ["1"])
            , ("5", all_ ["1"])
            , ("6", all_ ["-1"])
            , ("7", all_ ["1"])
            , ("8", all_ ["1"])
            , ("9", all_ ["1"])
            ]
    describe "q267099: Which skill to train?" $ do
        specEval
            "õõ2+$ŗ→Eõl"
            [ ("[3,2,4,1,2]", all_ ["4"])
            , ("[7,19,12,20,14]", all_ ["3"])
            , ("[13,12,19,9,20]", all_ ["0"])
            , ("[13,18,12,12,14]", all_ ["4"])
            , ("[18,19,18,16,13]", all_ ["1"])
            , ("[14,14,19,17,11]", all_ ["2"])
            ]
    describe "q267346: Night hike partitioning" $ do
        specEval
            "Ooᵐᵗžũ"
            [ ("[1]", all_ ["[[1]]"])
            , ("[0,0,0,0,0,1]", all_ ["[[0,0,0,0,0,1]]"])
            , ("[0,0,1,1]", all_ ["[[0,0,1,1]]", "[[0,0,1],[1]]", "[[0,1],[0,1]]"])
            , ("[0,0,1,1,1]", all_ ["[[0,0,1,1,1]]", "[[0,0,1,1],[1]]", "[[0,1],[0,1,1]]", "[[0,0,1],[1,1]]", "[[0,0,1],[1],[1]]", "[[0,1],[0,1],[1]]"])
            , ("[1,1,1,1,1]", all_ ["[[1,1,1,1,1]]", "[[1],[1,1,1,1]]", "[[1,1],[1,1,1]]", "[[1],[1],[1,1,1]]", "[[1],[1,1],[1,1]]", "[[1],[1],[1],[1,1]]", "[[1],[1],[1],[1],[1]]"])
            ]
    describe "q267491: Iteratively sort a list" $ do
        specEval
            "oOʲů"
            [ ("[1,5,2,2,8,3,5,2,9]", first_ "[1,2,3,5,8,9,2,5,2]")
            , ("[8,5]", first_ "[5,8]")
            , ("[2,2,2]", first_ "[2,2,2]")
            ]
    describe "q268592: Test whether a sequence is bitonic" $ do
        specEval
            "∆±ĉđ"
            [ ("[1,3,5,7,6,4,2]", Check True)
            , ("[99,4,8,16]", Check True)
            , ("[100000,10000,1000,100,10,1,11,111,1111,11111,111111]", Check True)
            , ("[2,1,3,4,5,6]", Check True)
            , ("[1,2,3,4,5,6,7]", Check False)
            , ("[7,6,5,4,3,2,1]", Check False)
            , ("[1,4,7,6,2,3,5]", Check False)
            , ("[99,88,66,77,55,44,33,22,11]", Check False)
            , ("[1,9,2,8,3,7,4,6,5]", Check False)
            ]
    describe "q268620: Am I vaccinated?" $ do
        specEval
            "'B∕uo"
            [ ("\"ABCWYB\"", all_ ["ABCWY"])
            , ("\"ABCWY\"", all_ ["ACWY"])
            , ("\"B\"", all_ ["[]"])
            , ("\"BB\"", all_ ["B"])
            , ("\"ACWYBB\"", all_ ["ABCWY"])
            , ("\"ACWYB\"", all_ ["ACWY"])
            ]
    describe "q268769: Can their first appearance happen together?" $ do
        specEval
            "ᶜ$ᵗ{iq=}s="
            [ ("\"01\" \"1\"", Check True)
            , ("\"00\" \"0\"", Check False)
            , ("\"01\" \"11\"", Check False)
            , ("\"11\" \"11\"", Check True)
            , ("\"0101\" \"1\"", Check False)
            , ("\"111\" \"11\"", Check False)
            ]
    describe "q268918: How many Carlitz compositions are there?" $ do
        specEval
            "řJĉᵐz"
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 1)
            , ("3", Count 3)
            , ("4", Count 4)
            , ("5", Count 7)
            , ("6", Count 14)
            , ("7", Count 23)
            , ("8", Count 39)
            , ("9", Count 71)
            , ("10", Count 124)
            ]
    describe "q269087: Counting Collinear Points" $ do
        specEval
            "≈đG←"
            [ ("[5,10] [10,5]", all_ ["4"])
            , ("[-8,5] [0,5]", all_ ["7"])
            , ("[-3,-3] [2,2]", all_ ["4"])
            ]
    describe "q269204: Divmod continuously until the remainder is 1 or 0, then get the remainder" $ do
        specEval
            "ʷ{Ƶþ"
            [ ("4 15", all_ ["0"])
            , ("7 102", all_ ["1"])
            , ("2 20", all_ ["0"])
            , ("0 10", all_ ["0"])
            , ("1 11", all_ ["1"])
            , ("7 99", all_ ["1"])
            , ("45 45", all_ ["0"])
            , ("30 31", all_ ["1"])
            , ("7 999", all_ ["0"])
            , ("6 999", all_ ["1"])
            , ("7 88", all_ ["0"])
            , ("999 1", all_ ["1"])
            , ("7 98", all_ ["0"])
            , ("998 999", all_ ["1"])
            ]
    describe "q269428: Doubleouble Talkalk" $ do
        specEval
            "Jtđ="
            [ ("\"SSS\"", Check True)
            , ("\"SNYYY\"", Check True)
            , ("\"SNYY\"", Check True)
            , ("\"SNNYY\"", Check True)
            , ("\"SNYNY\"", Check True)
            , ("\"FALSYTESTCASES\"", Check False)
            , ("\"FALSYTESTCASESXFALSYTESTCASES\"", Check False)
            , ("\"FALSYTESTCASESFALSYTESTCASES\"", Check False)
            , ("\"SMARTIEATIE\"", Check False)
            ]
    describe "q269712: Modular Equivalence" $ do
        specEval
            "ᵋ*+-Ď~2>"
            [ ("3 4", all_ ["5"])
            , ("5 8", all_ ["3", "9", "27"])
            , ("29 9", all_ ["223"])
            , ("26 4", all_ ["37", "74"])
            , ("13 11", all_ ["7", "17", "119"])
            , ("6258 571", all_ ["463", "7703", "3566489"])
            ]
    describe "q269857: Counting rankings" $ do
        specEval
            "RJ:ᵐhmj↕ũh="
            [ ("1 1", Count 1)
            , ("2 1", Count 2)
            , ("2 2", Count 1)
            , ("3 1", Count 6)
            , ("3 2", Count 4)
            , ("3 3", Count 3)
            , ("4 1", Count 26)
            , ("4 2", Count 18)
            , ("4 3", Count 18)
            , ("4 4", Count 13)
            ]
    describe "q269954: Remove falsy rows and columns" $ do
        specEval
            "Z:Ť‼Ť$ḟ"
            [ ("[[1,2,3],[4,5,6]]", all_ ["[[1,2,3],[4,5,6]]"])
            , ("[[1,2,3],[4,5,0]]", all_ ["[[1,2]]"])
            , ("[[3,6,19],[4,0,18],[2,19,3]]", all_ ["[[3,19],[2,3]]"])
            , ("[[5,3,2,4,1],[3,2,0,4,7],[7,1,9,8,2],[3,2,1,5,7],[6,4,6,1,2],[9,3,2,4,0]]", all_ ["[[5,3,4],[7,1,8],[3,2,5],[6,4,1]]"])
            , ("[[4,9,4,1],[2,0,6,0],[3,4,1,2],[9,7,8,5],[3,5,2,0]]", all_ ["[[4,4],[3,1],[9,8]]"])
            ]
    describe "q270154: Strings without twin letters" $ do
        specEval
            "ŧĉᵐz"
            [ ("3 \"ABC\"", all_ ["ABA", "ABC", "ACA", "ACB", "BAB", "BAC", "BCA", "BCB", "CAB", "CAC", "CBA", "CBC"])
            , ("3 \"AB\"", all_ ["ABA", "BAB"])
            , ("3 \"ABCD\"", truncate_ ["ABA", "ABC", "ABD", "ACA", "ACB", "ACD", "ADA", "ADB", "ADC", "BAB"])
            , ("2 \"OKAY\"", all_ ["OK", "OA", "OY", "KO", "KA", "KY", "AO", "AK", "AY", "YO", "YK", "YA"])
            , ("3 \"CODEGLF\"", truncate_ ["COC", "COD", "COE", "COG", "COL", "COF", "CDC", "CDO", "CDE", "CDG"])
            , ("4 \"NFKD\"", truncate_ ["NFNF", "NFNK", "NFND", "NFKN", "NFKF", "NFKD", "NFDN", "NFDF", "NFDK", "NKNF"])
            , ("5 \"JOHN\"", truncate_ ["JOJOJ", "JOJOH", "JOJON", "JOJHJ", "JOJHO", "JOJHN", "JOJNJ", "JOJNO", "JOJNH", "JOHJO"])
            ]
    describe "q271363: Is it a tetrate of two?" $ do
        specEval
            "0ʷ{v<Ë}="
            [ ("1", Check True)
            , ("2", Check True)
            , ("4", Check True)
            , ("16", Check True)
            , ("65536", Check True)
            , ("3", Check False)
            , ("8", Check False)
            , ("27", Check False)
            , ("81", Check False)
            , ("256", Check False)
            ]
    describe "q271436: Reconstruct a list from its prefixes" $ do
        specEval
            "Ŝoç∆"
            [ ("[[1],[1,2],[1,2,3]]", all_ ["[1,2,3]"])
            , ("[[2,1],[1],[3,1,2]]", all_ ["[1,2,3]"])
            , ("[[6,4,8],[7,4,8,6],[6,4],[6]]", all_ ["[6,4,8,7]"])
            , ("[[6,2,3,1],[1,2],[1],[6,1,2],[2,2,3,1,6]]", all_ ["[1,2,6,3,2]"])
            , ("[[2],[30,2,27,40],[27,40,2,89,30],[2,30,27],[27,2]]", all_ ["[2,27,30,40,89]"])
            , ("[[8,4],[4,8,8,4],[4,4,8],[4]]", all_ ["[4,8,4,8]"])
            , ("[[22,98,62,80],[80,98],[22,98,10,62,80,87,2],[98],[62,80,98],[22,98,2,62,80],[2,22,98,62,87,80]]", all_ ["[98,80,62,22,2,87,10]"])
            , ("[[43,84,56,19],[56,43],[43,56,19],[43]]", all_ ["[43,56,19,84]"])
            ]
    describe "q271505: Reconstruct a list of strings from its prefixes" $ do
        specEval
            "↕ᵉtiᶻ{∕z"
            [ ("[[],[\"lorem\"],[\"lorem\",\"ipsum\"],[\"lorem\",\"ipsum\",\"dolor\"]]", all_ ["[\"lorem\",\"ipsum\",\"dolor\"]"])
            , ("[[\"ipsum\",\"lorem\"],[\"lorem\"],[\"dolor\",\"lorem\",\"ipsum\"],[]]", all_ ["[\"lorem\",\"ipsum\",\"dolor\"]"])
            , ("[[\"consectetuer\",\"sit\",\"elit\"],[\"adipiscing\",\"sit\",\"elit\",\"consectetuer\"],[\"consectetuer\",\"sit\"],[\"consectetuer\"],[]]", all_ ["[\"consectetuer\",\"sit\",\"elit\",\"adipiscing\"]"])
            , ("[[\"ipsum\"],[\"nunc\",\"ipsum\",\"justo\",\"in\"],[\"justo\",\"in\",\"ipsum\",\"molestie\",\"nunc\"],[\"ipsum\",\"nunc\",\"justo\"],[\"justo\",\"ipsum\"],[]]", all_ ["[\"ipsum\",\"justo\",\"nunc\",\"in\",\"molestie\"]"])
            , ("[[\"elit\",\"sit\"],[\"sit\",\"elit\",\"elit\",\"sit\"],[\"sit\",\"sit\",\"elit\"],[\"sit\"],[]]", all_ ["[\"sit\",\"elit\",\"sit\",\"elit\"]"])
            ]
        specEval
            "Ŝoç∆ᵐᶠZH"
            [ ("[[\"lorem\"],[\"lorem\",\"ipsum\"],[\"lorem\",\"ipsum\",\"dolor\"]]", all_ ["[\"lorem\",\"ipsum\",\"dolor\"]"])
            , ("[[\"ipsum\",\"lorem\"],[\"lorem\"],[\"dolor\",\"lorem\",\"ipsum\"]]", all_ ["[\"lorem\",\"ipsum\",\"dolor\"]"])
            , ("[[\"consectetuer\",\"sit\",\"elit\"],[\"adipiscing\",\"sit\",\"elit\",\"consectetuer\"],[\"consectetuer\",\"sit\"],[\"consectetuer\"]]", all_ ["[\"consectetuer\",\"sit\",\"elit\",\"adipiscing\"]"])
            , ("[[\"ipsum\"],[\"nunc\",\"ipsum\",\"justo\",\"in\"],[\"justo\",\"in\",\"ipsum\",\"molestie\",\"nunc\"],[\"ipsum\",\"nunc\",\"justo\"],[\"justo\",\"ipsum\"]]", all_ ["[\"ipsum\",\"justo\",\"nunc\",\"in\",\"molestie\"]"])
            , ("[[\"elit\",\"sit\"],[\"sit\",\"elit\",\"elit\",\"sit\"],[\"sit\",\"sit\",\"elit\"],[\"sit\"]]", all_ ["[\"sit\",\"elit\",\"sit\",\"elit\"]"])
            ]
    describe "q271522: Can I follow this recipe?" $ do
        specEval
            "ᶦ{ĕ~f}z"
            [ ("[[9,2,3]]", Check True)
            , ("[[],[]]", Check False)
            , ("[[2],[9]]", Check True)
            , ("[[],[3],[2,6],[1,2,4],[1,2,3]]", Check True)
            , ("[[1,2,3],[],[2,4],[1,2,4],[3]]", Check False)
            , ("[[],[],[9]]", Check False)
            , ("[[1,2,3],[],[2,4],[1,2,4],[3],[9]]", Check False)
            ]
    describe "q271970: Next cyclic suffix" $ do
        specEval
            "Ṁ→↔$ŧᵖ{,:xŘṀ="
            [ ("[9] 3", first_ "[9,9,9]")
            , ("[4,2,1] 3", first_ "[4,2,1]")
            , ("[9,0,9] 2", first_ "[0,8]")
            , ("[9,8,0,9,8] 2", first_ "[0,8]")
            , ("[9,8,0,9,8] 3", first_ "[0,9,7]")
            , ("[9,8,0,9,8] 4", first_ "[0,9,8,0]")
            , ("[9,9,0,9] 1", first_ "[8]")
            , ("[5,4] 5", first_ "[5,4,5,4,4]")
            , ("[10,6,2] 4", first_ "[10,6,2,9]")
            ]
    describe "q272425: Fixed Repeating Output" $ do
        specEval
            "Ħ¬"
            [ ("5", all_ ["[1,1,1,1,1,0]"])
            , ("2", all_ ["[1,1,0]"])
            , ("0", all_ ["[0]"])
            ]
    describe "q272727: Weave two lists, cycling if necessary" $ do
        specEval
            "ᵐ#:x+Ṁ$ᵒ%ᵐᶻ@ji"
            [ ("[[1],[2]]", all_ ["[1,2,1]"])
            , ("[[1,1],[2]]", all_ ["[1,2,1]"])
            , ("[[1,2,3],[6]]", all_ ["[1,6,2,6,3]"])
            , ("[[6],[1,2,3]]", all_ ["[6,1,6,2,6,3,6]"])
            , ("[[10,10],[42,42]]", all_ ["[10,42,10,42,10]"])
            , ("[[5,5],[5,5,5]]", all_ ["[5,5,5,5,5,5,5]"])
            , ("[[1,2,3,4,5],[6,7,8,9]]", all_ ["[1,6,2,7,3,8,4,9,5]"])
            , ("[[1,2,3,4,5],[7,8]]", all_ ["[1,7,2,8,3,7,4,8,5]"])
            , ("[[1,2],[6,7,8,9]]", all_ ["[1,6,2,7,1,8,2,9,1]"])
            , ("[[1,2,3],[7,8,9]]", all_ ["[1,7,2,8,3,9,1]"])
            ]
    describe "q272771: Is it a cartesian product?" $ do
        specEval
            "ŤđᵃSᵒÐj↕="
            [ ("[[1,1]]", Check True)
            , ("[[1,1],[1,2]]", Check True)
            , ("[[1,1],[1,1],[2,1]]", Check True)
            , ("[[1,4],[1,6],[2,4],[7,4],[7,6],[2,6]]", Check True)
            , ("[[4,7],[4,6],[4,5],[4,4]]", Check True)
            , ("[[1,2],[3,4]]", Check False)
            , ("[[1,2],[2,2],[3,2],[1,3],[2,3]]", Check False)
            , ("[[1,1],[1,1],[1,2],[2,1],[2,2]]", Check False)
            , ("[[1,4],[7,4],[9,6]]", Check False)
            , ("[[6,1],[1,6]]", Check False)
            ]
        specEval
            "↕ᴶŤŤđ≡¿ᵐ≡"
            [ ("[[1,1]]", Check True)
            , ("[[1,1],[1,2]]", Check True)
            , ("[[1,1],[1,1],[2,1]]", Check True)
            , ("[[1,4],[1,6],[2,4],[7,4],[7,6],[2,6]]", Check True)
            , ("[[4,7],[4,6],[4,5],[4,4]]", Check True)
            , ("[[1,2],[3,4]]", Check False)
            , ("[[1,2],[2,2],[3,2],[1,3],[2,3]]", Check False)
            , ("[[1,1],[1,1],[1,2],[2,1],[2,2]]", Check False)
            , ("[[1,4],[7,4],[9,6]]", Check False)
            , ("[[6,1],[1,6]]", Check False)
            ]
    describe "q273467: Looping counter extended" $ do
        specEval
            "Ňr26%→Ɔ64+$∑→řH"
            [("", truncate_ ["A", "BB", "CCCC", "DDDDDDD", "EEEEEEEEEEE", "FFFFFFFFFFFFFFFF"])]
    describe "q273479: Is this a valid PZN?" $ do
        specEval
            "7R¢ɔ∙11¦"
            [ ("[0,0,0,0,0,0,0,0]", Check True)
            , ("[1,3,7,2,4,8,5,7]", Check True)
            , ("[0,8,1,0,8,7,5,4]", Check True)
            , ("[0,4,8,6,1,8,8,0]", Check True)
            , ("[1,6,0,8,6,3,3,4]", Check True)
            , ("[0,1,3,4,9,0,3,5]", Check True)
            , ("[0,0,0,0,0,0,0,1]", Check False)
            , ("[0,0,0,0,0,0,0,2]", Check False)
            , ("[0,6,7,6,5,7,7,4]", Check False)
            , ("[0,8,1,0,8,7,5,9]", Check False)
            , ("[0,0,1,0,0,0,1,0]", Check False)
            ]
    describe "q273620: 9-16-25 2D Matrix" $ do
        specEval
            "ᵃ#*R↕v#Š:ᵐ∑ᵈv=¿:∑ᵈ§=¿"
            [("[12,18,15] [9,18,18]", first_ "[[1,4,7],[3,6,9],[5,8,2]]")]
    describe "q273875: Segments of a string, doubling in length" $ do
        specEval
            "JxËᶻL"
            [ ("\"a\"", all_ ["[\"a\"]"])
            , ("\"abc\"", all_ ["[\"a\",\"bc\"]"])
            , ("\"abcdefg\"", all_ ["[\"a\",\"bc\",\"defg\"]"])
            , ("\"abcdefghijklmno\"", all_ ["[\"a\",\"bc\",\"defg\",\"hijklmno\"]"])
            ]
    describe "q273990: Is this a Hadamard matrix?" $ do
        specEval
            "Sđ∙Z"
            [ ("[[1]]", Check False)
            , ("[[1,1],[1,-1]]", Check False)
            , ("[[1,-1],[-1,1]]", Check True)
            , ("[[1,1,1,1],[1,-1,1,-1],[1,1,-1,-1],[1,-1,-1,1]]", Check False)
            , ("[[1,-1,1,-1],[-1,1,-1,1],[1,-1,1,-1],[-1,1,-1,1]]", Check True)
            ]
        specEval
            "ᵒ∙jZ‼*"
            [ ("[[1]]", Check True)
            , ("[[1,1],[1,-1]]", Check True)
            , ("[[1,-1],[-1,1]]", Check False)
            , ("[[1,1,1,1],[1,-1,1,-1],[1,1,-1,-1],[1,-1,-1,1]]", Check True)
            , ("[[1,-1,1,-1],[-1,1,-1,1],[1,-1,1,-1],[-1,1,-1,1]]", Check False)
            ]
    describe "q274161: Sorting with a deque" $ do
        specEval
            "pƆᵗ≤ᵗ≥"
            [ ("[1]", Check False)
            , ("[1,2,3,4]", Check False)
            , ("[4,3,2,1]", Check False)
            , ("[10,10,10,10]", Check False)
            , ("[3,4,2,5,1,6]", Check False)
            , ("[4,3,5,2,6,1]", Check False)
            , ("[5,4,4,5,3,5,3,6,2,6]", Check False)
            , ("[1,3,2]", Check True)
            , ("[3,1,2]", Check True)
            , ("[4,3,2,3,2,1]", Check True)
            , ("[1,2,3,2,3,4]", Check True)
            , ("[4,5,4,3,4,5]", Check True)
            ]
    describe "q274358: Double dequer sort" $ do
        specEval
            "o$2ᵑ{Jĭᵃjᵈ↔,}="
            [ ("[1]", Check True)
            , ("[1,2,3,4]", Check True)
            , ("[2,4,3,1]", Check True)
            , ("[4,3,2,1]", Check True)
            , ("[10,10,10,10]", Check True)
            , ("[3,4,2,5,1,6]", Check True)
            , ("[4,3,5,2,6,1]", Check True)
            , ("[5,4,4,5,3,5,3,6,2,6]", Check True)
            , ("[1,3,2]", Check True)
            , ("[3,4,5,1,2,3]", Check True)
            , ("[5,3,4,1,2]", Check False)
            , ("[7,5,6,3,4,1,2]", Check False)
            , ("[7,8,9,3,4,5,1,2,3]", Check False)
            ]
    describe "q274448: Factoriadic Fraction Addition" $ do
        specEval
            "+ʸ{CU$x3+þç++;ž¿Ø?"
            [ ("[] []", first_ "[]")
            , ("[] [1]", first_ "[1]")
            , ("[0,2] [-1]", first_ "[-1,2]")
            , ("[0,2] [0,2]", first_ "[1,1]")
            , ("[0,1,2] [0,1,2]", first_ "[1]")
            ]
    describe "q274690: Cubic Concatenation" $ do
        specEval
            "*∙Ɗᴶɗ="
            [ ("[1,5,3]", Check True)
            , ("[2,2,13]", Check True)
            , ("[4,0,7]", Check True)
            , ("[10,0,0]", Check True)
            , ("[10,0,1]", Check True)
            , ("[22,18,59]", Check True)
            , ("[98,28,27]", Check True)
            , ("[166,500,333]", Check True)
            , ("[828,538,472]", Check True)
            , ("[1,2,3]", Check False)
            , ("[4,5,6]", Check False)
            , ("[6,0,0]", Check False)
            , ("[166,500,334]", Check False)
            , ("[200,0,200]", Check False)
            ]
    describe "q274829: Is there mutable aliasing in this list of variable references?" $ do
        specEval
            "ĕ∏Hᵗf"
            [ ("[]", Check False)
            , ("[[\"a\",1]]", Check False)
            , ("[[\"c\",0],[\"d\",0]]", Check False)
            , ("[[\"c\",0],[\"g\",1],[\"c\",0],[\"c\",0]]", Check False)
            , ("[[\"f\",1],[\"e\",0],[\"e\",0],[\"d\",1]]", Check False)
            , ("[[\"a\",0],[\"a\",0],[\"a\",0],[\"a\",0],[\"a\",0],[\"a\",0],[\"a\",0]]", Check False)
            , ("[[\"b\",1],[\"b\",0]]", Check True)
            , ("[[\"b\",1],[\"b\",1]]", Check True)
            , ("[[\"a\",1],[\"d\",0],[\"d\",1]]", Check True)
            , ("[[\"a\",0],[\"d\",1],[\"d\",0],[\"a\",0]]", Check True)
            , ("[[\"f\",0],[\"e\",0],[\"e\",1],[\"d\",0]]", Check True)
            , ("[[\"a\",0],[\"a\",0],[\"a\",0],[\"a\",1],[\"a\",0],[\"a\",0],[\"a\",0]]", Check True)
            , ("[[\"g\",1],[\"g\",1],[\"g\",1],[\"g\",1],[\"g\",1],[\"g\",1],[\"g\",1]]", Check True)
            , ("[[\"a\",0],[\"a\",1],[\"a\",0],[\"g\",1],[\"g\",0],[\"g\",1]]", Check True)
            ]
