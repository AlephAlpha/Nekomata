{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eval (testEval) where

import Control.Monad
import Data.Either (isRight)
import Nekomata.Eval
import Test.Hspec

specEval :: String -> [(String, Result)] -> Spec
specEval code testCases = context code $ do
    it "should compile" $ do
        code `shouldSatisfy` isRight . compile
    it ("should be " ++ show (length code) ++ " bytes") $ do
        length code `shouldBe` length code
    let Right f = compile code
    forM_ testCases $ \(input, output) -> do
        it (input ++ " -> " ++ show output) $ do
            let Right input' = readInput input
            let runtime = initRuntime input'
            let result = snd $ runFunction f runtime
            case output of
                All xs -> allResults result `shouldBe` xs
                Truncated xs ->
                    take (length xs) (allResults result) `shouldBe` xs
                First x -> firstResult result `shouldBe` x
                Count n -> countResults result `shouldBe` n
                Check b -> checkResult result `shouldBe` b

testEval :: Spec
testEval = describe "Evaluation" $ do
    describe "q69: Golf you a quine for great good!" $ do
        specEval
            "\"ᵉĝ,\"ᵉĝ,"
            [("", All ["\"ᵉĝ,\"ᵉĝ,"])]
    describe "q85: Fibonacci function or sequence" $ do
        specEval
            "1:ᶦ{$ᵉ+"
            [("", Truncated ["1", "1", "2", "3", "5", "8", "13", "21", "34", "55"])]
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
            "Jᵐ#3<"
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
            "qNᵗz:↔=ũ"
            [ ("\"12131331\"", All ["121", "131", "313", "1331", "33"])
            , ("\"3333\"", All ["33", "333", "3333"])
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
            "ˡ{1>ᵉ½3*→I"
            [ ("2", All ["1"])
            , ("16", All ["4"])
            , ("5", All ["5"])
            , ("7", All ["16"])
            ]
    describe "q12902: Run Length Decoding" $ do
        specEval
            "ĭᵐĜy"
            [("\":144,1'1\"", All [":4444,'"])]
    describe "q38325: Minimum excluded number" $ do
        specEval
            "ᵏf"
            [ ("[1]", All ["0"])
            , ("[0]", All ["1"])
            , ("[2,0]", All ["1"])
            , ("[3,1,0,1,3,3]", All ["2"])
            , ("[]", All ["0"])
            , ("[1,2,3]", All ["0"])
            , ("[5,4,1,5,4,8,2,1,5,4,0,7,7]", All ["3"])
            , ("[3,2,1,0]", All ["4"])
            , ("[0,0,1,1,2,2,3]", All ["4"])
            , ("[1,0,7,6,3,11,15,1,9,2,3,1,5,2,3,4,6,8,1,18]", All ["10"])
            ]
    describe "q42529: Mode (most common element) of a list" $ do
        specEval
            "ŢṂ"
            [("[4,3,1,0,6,1,6,4,4,0,3,1,7,7,3,4,1,1,2,8]", All ["1"])]
    describe "q50020: List Sophie Germain primes" $ do
        specEval
            "Ƥ←½Q"
            [("", Truncated ["2", "3", "5", "11", "23", "29", "41", "53", "83", "89"])]
    describe "q50240: XOR multiplication" $ do
        specEval
            "ᵃƂ×Öƃ"
            [ ("0 1", All ["0"])
            , ("1 2", All ["2"])
            , ("9 0", All ["0"])
            , ("6 1", All ["6"])
            , ("3 3", All ["5"])
            , ("2 5", All ["10"])
            , ("7 9", All ["63"])
            , ("13 11", All ["127"])
            , ("5 17", All ["85"])
            , ("14 13", All ["70"])
            , ("19 1", All ["19"])
            , ("63 63", All ["1365"])
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
            [("", All ["Hello, World!"])]
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
            [ ("[]", All ["[]"])
            , ("[-1,0,1]", All ["[]"])
            , ("[1,1]", All ["[1]"])
            , ("[3,0,0,1,1,0,5,3]", All ["[0,1,3]"])
            , ("[-34,0,1,-34,4,8,4]", All ["[-34,4]"])
            ]
    describe "q61808: Lossy Sorting (Implement Dropsort)" $ do
        specEval
            "pƆᵖ≤"
            [ ("[1,2,5,4,3,7]", All ["1", "2", "5", "7"])
            , ("[10,-1,12]", All ["10", "12"])
            , ("[-7,-8,-5,0,-1,1]", All ["-7", "-5", "0", "1"])
            , ("[9,8,7,6,5]", All ["9"])
            , ("[10,13,17,21]", All ["10", "13", "17", "21"])
            , ("[10,10,10,9,10]", All ["10", "10", "10", "10"])
            ]
    describe "q62732: Implement a Truth-Machine" $ do
        specEval
            "ᶦP"
            [ ("0", All ["0"])
            , ("1", Truncated ["1", "1", "1", "1", "1", "1", "1", "1", "1", "1"])
            ]
    describe "q62752: Longest Common Prefix of 2 Strings" $ do
        specEval
            "ᵃp=al"
            [ ("\"global\" \"glossary\"", All ["glo"])
            , ("\"department\" \"depart\"", All ["depart"])
            , ("\"glove\" \"dove\"", All ["[]"])
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
            [ ("[1,0,2,0,7,7,7,0,5,0,0,0,9]", All ["1", "1", "2", "2", "7", "7", "7", "7", "5", "5", "5", "5", "9"])
            , ("[1,0,0,0,0,0]", All ["1", "1", "1", "1", "1", "1"])
            , ("[-1,0,5,0,0,-7]", All ["-1", "-1", "5", "5", "5", "-7"])
            , ("[23,0,0,-42,0,0,0]", All ["23", "23", "23", "-42", "-42", "-42", "-42"])
            , ("[1,2,3,4]", All ["1", "2", "3", "4"])
            , ("[-1234]", All ["-1234"])
            ]
    describe "q66127: Catalan Numbers" $ do
        specEval
            "Ä$Ç$→/"
            [ ("0", All ["1"])
            , ("1", All ["1"])
            , ("2", All ["2"])
            , ("3", All ["5"])
            , ("4", All ["14"])
            , ("5", All ["42"])
            , ("6", All ["132"])
            , ("7", All ["429"])
            , ("8", All ["1430"])
            , ("9", All ["4862"])
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
            [ ("1", All ["1"])
            , ("2", All ["12"])
            , ("3", All ["123"])
            , ("7", All ["1234567"])
            , ("9", All ["123456789"])
            , ("10", All ["10123456789"])
            , ("15", All ["101111111223344556789"])
            , ("34", All ["10001111111111111222222222222223333333334444555666777888999"])
            , ("42", All ["100001111111111111122222222222222233333333333333444444455556666777788889999"])
            ]
    describe "q70365: Construct the Identity Matrix" $ do
        specEval
            "ᵒ-¬"
            [ ("1", All ["[[1]]"])
            , ("2", All ["[[1,0],[0,1]]"])
            , ("3", All ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", All ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
            ]
    describe "q70558: Hypercube elements" $ do
        specEval
            "→rᵉÇË*"
            [ ("0", All ["[1]"])
            , ("1", All ["[1,2]"])
            , ("3", All ["[1,6,12,8]"])
            , ("10", All ["[1,20,180,960,3360,8064,13440,15360,11520,5120,1024]"])
            , ("12", All ["[1,24,264,1760,7920,25344,59136,101376,126720,112640,67584,24576,4096]"])
            ]
        specEval
            "2ᵚR1cʳ×"
            [ ("0", All ["1"])
            , ("1", All ["[1,2]"])
            , ("3", All ["[1,6,12,8]"])
            , ("10", All ["[1,20,180,960,3360,8064,13440,15360,11520,5120,1024]"])
            , ("12", All ["[1,24,264,1760,7920,25344,59136,101376,126720,112640,67584,24576,4096]"])
            ]
    describe "q70837: Say What You See" $ do
        specEval
            "1Uᶦ{Y$Ĭ}ɗ"
            [("", Truncated ["1", "11", "21", "1211", "111221", "312211", "13112221", "1113213211", "31131211131221", "13211311123113112211"])]
    describe "q71833: How even is a number?" $ do
        specEval
            "ˡ½"
            [ ("14", All ["1"])
            , ("20", All ["2"])
            , ("94208", All ["12"])
            , ("7", All ["0"])
            , ("-4", All ["2"])
            ]
    describe "q74273: Output all strings" $ do
        specEval
            "Ňŧ"
            [("\"ab\"", Truncated ["[]", "a", "b", "aa", "ab", "ba", "bb", "aaa", "aab", "aba", "abb"])]
    describe "q77270: Greatest Common Divisor" $ do
        specEval
            "G"
            [ ("0 2", All ["2"])
            , ("6 0", All ["6"])
            , ("30 42", All ["6"])
            , ("15 14", All ["1"])
            , ("7 7", All ["7"])
            , ("69 25", All ["1"])
            , ("21 12", All ["3"])
            , ("169 123", All ["1"])
            , ("20 142", All ["2"])
            , ("101 202", All ["101"])
            ]
        specEval
            "ʷ{$Zᵉ%"
            [ ("0 2", All ["2"])
            , ("6 0", All ["6"])
            , ("30 42", All ["6"])
            , ("15 14", All ["1"])
            , ("7 7", All ["7"])
            , ("69 25", All ["1"])
            , ("21 12", All ["3"])
            , ("169 123", All ["1"])
            , ("20 142", All ["2"])
            , ("101 202", All ["101"])
            ]
        specEval
            "ṀRᶠ¦Ṁ"
            [ ("[0,2]", All ["2"])
            , ("[6,0]", All ["6"])
            , ("[30,42]", All ["6"])
            , ("[15,14]", All ["1"])
            , ("[7,7]", All ["7"])
            , ("[69,25]", All ["1"])
            , ("[21,12]", All ["3"])
            , ("[169,123]", All ["1"])
            , ("[20,142]", All ["2"])
            , ("[101,202]", All ["101"])
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
            [ ("[1,1,1,2,2,1,1,1,1,2,2,2,1,1,1]", All ["[2,2]"])
            , ("[3,3,3,4,4,4,4,5,5,4,4,3,3,4,4]", All ["[5,5]", "[4,4]", "[3,3]", "[4,4]"])
            , ("[1,1,2,2,3,3,4]", All ["[4]"])
            , ("[1]", All ["[1]"])
            , ("[1,1,10,10,10,100,100]", All ["[1,1]", "[100,100]"])
            ]
    describe "q79483: Continued Fraction of a Rational Number" $ do
        specEval
            "ᶦ{1%ŗ}k"
            [ ("860438", All ["860438"])
            , ("3245/1000", All ["3", "4", "12", "4"])
            , ("-42/10", All ["-5", "1", "4"])
            , ("-1147802/10000", All ["-115", "4", "1", "1", "4", "1", "1", "5", "1", "1", "4"])
            , ("0/11", All ["0"])
            , ("1/42", All ["0", "42"])
            , ("2/7", All ["0", "3", "2"])
            , ("-18/17056", All ["-1", "1", "946", "1", "1", "4"])
            , ("-17056/18", All ["-948", "2", "4"])
            ]
    describe "q82497: Pseudofactorial" $ do
        specEval
            "Rʳg"
            [ ("1", All ["1"])
            , ("2", All ["2"])
            , ("3", All ["6"])
            , ("4", All ["12"])
            , ("5", All ["60"])
            , ("6", All ["60"])
            , ("7", All ["420"])
            ]
    describe "q82604: Approximation of e" $ do
        specEval
            "rFŗ∑"
            [ ("1", All ["1"])
            , ("2", All ["2"])
            , ("3", All ["5/2"])
            , ("4", All ["8/3"])
            , ("5", All ["65/24"])
            , ("6", All ["163/60"])
            , ("7", All ["1957/720"])
            , ("8", All ["685/252"])
            , ("9", All ["109601/40320"])
            , ("10", All ["98641/36288"])
            ]
    describe "q83377: Write a program to elasticize strings" $ do
        specEval
            "#Ry"
            [ ("\"Why\"", All ["Whhyyy"])
            , ("\"SKype\"", All ["SKKyyyppppeeeee"])
            , ("\"LobbY\"", All ["LoobbbbbbbYYYYY"])
            , ("\"A and B\"", All ["A  aaannnnddddd      BBBBBBB"])
            ]
    describe "q83533: Calculate Euler's totient function" $ do
        specEval
            "RG1Ĉ"
            [ ("1", All ["1"])
            , ("2", All ["1"])
            , ("3", All ["2"])
            , ("8", All ["4"])
            , ("9", All ["6"])
            , ("26", All ["12"])
            , ("44", All ["20"])
            , ("105", All ["48"])
            ]
    describe "q84519: Implement Takewhile" $ do
        specEval
            "Ö∫¬∑T"
            [ ("[14,42,2324,97090,4080622,171480372]", All ["[14,42,2324,97090,4080622,171480372]"])
            , ("[42,14,42,2324]", All ["[42,14,42,2324]"])
            , ("[7,14,42]", All ["[]"])
            , ("[]", All ["[]"])
            , ("[171480372,13,14,42]", All ["[171480372]"])
            , ("[42,14,42,43,41,4080622,171480372]", All ["[42,14,42]"])
            ]
        specEval
            "pᵖ½al"
            [ ("[14,42,2324,97090,4080622,171480372]", All ["[14,42,2324,97090,4080622,171480372]"])
            , ("[42,14,42,2324]", All ["[42,14,42,2324]"])
            , ("[7,14,42]", All ["[]"])
            , ("[]", All ["[]"])
            , ("[171480372,13,14,42]", All ["[171480372]"])
            , ("[42,14,42,43,41,4080622,171480372]", All ["[42,14,42]"])
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
            [("", Truncated ["2", "3", "7", "43", "1807", "3263443", "10650056950807"])]
    describe "q91420: Excessive Integers" $ do
        specEval
            "ƒ←∑"
            [ ("1", All ["0"])
            , ("2", All ["0"])
            , ("3", All ["0"])
            , ("4", All ["1"])
            , ("5", All ["0"])
            , ("6", All ["0"])
            , ("7", All ["0"])
            , ("8", All ["2"])
            , ("9", All ["1"])
            , ("10", All ["0"])
            , ("11", All ["0"])
            , ("12", All ["1"])
            , ("13", All ["0"])
            , ("14", All ["0"])
            , ("15", All ["0"])
            ]
    describe "q93261: StringgnirtSStringgnirtSStringgnirtS" $ do
        specEval
            "xᵐᵑ↔j"
            [ ("\"a\"", All ["a"])
            , ("\"abcd\"", All ["abcddcbaabcddcba"])
            , ("\"OK!\"", All ["OK!!KOOK!"])
            , ("\"4815162342\"", All ["4815162342243261518448151623422432615184481516234224326151844815162342243261518448151623422432615184"])
            , ("\"PPCG\"", All ["PPCGGCPPPPCGGCPP"])
            , ("\"42\"", All ["4224"])
            ]
    describe "q94028: Find the largest number that's adjacent to a zero" $ do
        specEval
            "qᵗZđ+aṀ"
            [ ("[1,4,3,6,0,3,7,0]", All ["7"])
            , ("[9,4,9,0,9,0,9,15,-2]", All ["9"])
            , ("[-4,-6,-2,0,-9]", All ["-2"])
            , ("[-11,0,0,0,0,0,-12,10]", All ["0"])
            , ("[0,20]", All ["20"])
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
    describe "q94348: Prime counting function" $ do
        specEval
            "ƥ"
            [ ("1", All ["0"])
            , ("2", All ["1"])
            , ("5", All ["3"])
            ]
        specEval
            "Fƒ#"
            [ ("1", All ["0"])
            , ("2", All ["1"])
            , ("5", All ["3"])
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
            [ ("[7,2]", All ["14"])
            , ("[8,1]", All ["8"])
            , ("[6,4,8]", All ["24"])
            , ("[8,2,1,10]", All ["40"])
            , ("[9,6,2,1,5]", All ["90"])
            , ("[5,5,7,1,1]", All ["35"])
            , ("[4,13,8,8,11,1]", All ["1144"])
            , ("[7,2,2,11,11,8,5]", All ["3080"])
            , ("[1,6,10,3,4,10,7]", All ["420"])
            , ("[5,2,9,10,3,4,4,4,7]", All ["1260"])
            , ("[9,7,10,9,7,8,5,10,1]", All ["2520"])
            ]
    describe "q95409: 2048-like array shift" $ do
        specEval
            "ĉ~ĭ+↔aj"
            [ ("[]", All ["[]"])
            , ("[2,2,4,4]", All ["[4,8]"])
            , ("[2,2,2,4,4,8]", All ["[2,4,8,8]"])
            , ("[2,2,2,2]", All ["[4,4]"])
            , ("[4,4,2,8,8,2]", All ["[8,2,16,2]"])
            , ("[1024,1024,512,512,256,256]", All ["[2048,1024,512]"])
            , ("[3,3,3,1,1,7,5,5,5,5]", All ["[3,6,2,7,10,10]"])
            ]
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
            [ ("[]", All ["0"])
            , ("[0]", All ["0"])
            , ("[1]", All ["1"])
            , ("[0,1,1,0,0]", All ["0"])
            , ("[1,1,1,0,1]", All ["1"])
            , ("[1,1,0,1,1]", All ["2"])
            , ("[0,0,1,1,1]", All ["3"])
            , ("[1,1,1,1,1,1]", All ["6"])
            ]
    describe "q101389: Increment an Array" $ do
        specEval
            "Ṁ←ɔ:ṁĨËƂ+"
            [ ("[1]", First $ Just "[1,1]")
            , ("[2]", First $ Just "[2,1]")
            , ("[1,1]", First $ Just "[1,1,1]")
            , ("[3,3,3,3,3]", First $ Just "[3,3,3,3,3,1]")
            , ("[1,2]", First $ Just "[2,2]")
            , ("[2,1]", First $ Just "[2,2]")
            , ("[3,1,1]", First $ Just "[3,2,1]")
            , ("[3,4,9,3]", First $ Just "[4,4,9,3]")
            ]
    describe "q103624: Find the sum of all numbers below n that are a multiple of some set of numbers" $ do
        specEval
            "ᵒ%ᵐ∏¬x∙"
            [ ("[2] 50", All ["600"])
            , ("[3,5] 10", All ["23"])
            , ("[4,2] 28", All ["182"])
            , ("[7,5] 19", All ["51"])
            , ("[2,3,5] 50", All ["857"])
            ]
        specEval
            "ᶠ{$~¦}∑"
            [ ("50 [2]", First $ Just "600")
            , ("10 [3,5]", First $ Just "23")
            , ("28 [4,2]", First $ Just "182")
            , ("19 [7,5]", First $ Just "51")
            , ("50 [2,3,5]", First $ Just "857")
            ]
    describe "q103756: Big numbers: Ultrafactorials" $ do
        specEval
            "→rF:E∑"
            [ ("0", All ["1"])
            , ("1", All ["2"])
            , ("2", All ["6"])
            , ("3", All ["46662"])
            ]
    describe "q104665: Coprimes up to N" $ do
        specEval
            "rG1Ĩ"
            [ ("2", All ["1"])
            , ("3", All ["1", "2"])
            , ("6", All ["1", "5"])
            , ("10", All ["1", "3", "7", "9"])
            , ("20", All ["1", "3", "7", "9", "11", "13", "17", "19"])
            , ("25", All ["1", "2", "3", "4", "6", "7", "8", "9", "11", "12", "13", "14", "16", "17", "18", "19", "21", "22", "23", "24"])
            , ("30", All ["1", "7", "11", "13", "17", "19", "23", "29"])
            ]
    describe "q105861: Can this number be written in (3^x) - 1 format?" $ do
        specEval
            "3D←P∑"
            [ ("2", All ["1"])
            , ("26", All ["3"])
            , ("1024", All [])
            ]
    describe "q106149: Compute the Median" $ do
        specEval
            ",o;↔ᶻÐlµ"
            [ ("[1,2,3,4,5,6,7,8,9]", All ["5"])
            , ("[1,4,3,2]", All ["5/2"])
            , ("[3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,-5,100000,13/10,7/5]", All ["3/2"])
            , ("[3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,3/2,-5,100000,13/10,7/5]", All ["3/2"])
            ]
    describe "q106656: Bit run rundown" $ do
        specEval
            "ƂYṀ"
            [ ("6", All ["2"])
            , ("16", All ["4"])
            , ("893", All ["5"])
            , ("1337371", All ["6"])
            , ("1", All ["1"])
            , ("9965546", All ["7"])
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
            [ ("1", All ["[1]"])
            , ("2", All ["[[2,2],[2,2]]"])
            , ("3", All ["[[[3,3,3],[3,3,3],[3,3,3]],[[3,3,3],[3,3,3],[3,3,3]],[[3,3,3],[3,3,3],[3,3,3]]]"])
            ]
    describe "q118444: Stay away from zero" $ do
        specEval
            "1M"
            [ ("0", All ["1"])
            , ("1", All ["1"])
            , ("2", All ["2"])
            , ("3", All ["3"])
            , ("4", All ["4"])
            , ("5", All ["5"])
            , ("6", All ["6"])
            , ("7", All ["7"])
            ]
    describe "q118597: Halve the falses" $ do
        specEval
            "ĭZĬ‼"
            [ ("[1,0,0,1,0,0,1]", All ["[1,0,1,0,1]"])
            , ("[1,1,0,0,1,1,0,0,1]", All ["[1,1,0,1,1,0,1]"])
            , ("[1,1,0,0,1,1,1,0,0,1,1]", All ["[1,1,0,1,1,1,0,1,1]"])
            , ("[1,1,1]", All ["[1,1,1]"])
            , ("[0,0,1]", All ["[0,1]"])
            , ("[0,0]", All ["[0]"])
            , ("[1,1,1,0,0,0,0,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,1,1,0,0,1,0,0]", All ["[1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0]"])
            ]
        specEval
            "¬∫½ᶻ¿‼"
            [ ("[1,0,0,1,0,0,1]", All ["[1,0,1,0,1]"])
            , ("[1,1,0,0,1,1,0,0,1]", All ["[1,1,0,1,1,0,1]"])
            , ("[1,1,0,0,1,1,1,0,0,1,1]", All ["[1,1,0,1,1,1,0,1,1]"])
            , ("[1,1,1]", All ["[1,1,1]"])
            , ("[0,0,1]", All ["[0,1]"])
            , ("[0,0]", All ["[0]"])
            , ("[1,1,1,0,0,0,0,1,1,1,1,0,0,1,0,0,1,1,0,0,1,1,1,1,0,0,1,0,0]", All ["[1,1,1,0,0,1,1,1,1,0,1,0,1,1,0,1,1,1,1,0,1,0]"])
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
            [ ("1", All ["1"])
            , ("2", All ["3"])
            , ("3", All ["3"])
            , ("4", All ["9"])
            , ("5", All ["5"])
            , ("6", All ["9"])
            , ("7", All ["7"])
            , ("8", All ["27"])
            , ("9", All ["9"])
            , ("10", All ["15"])
            ]
    describe "q119854: Raise integer x to power x, without exponentiation built-ins" $ do
        specEval
            "ř∏"
            [ ("2", All ["4"])
            , ("3", All ["27"])
            , ("5", All ["3125"])
            , ("6", All ["46656"])
            , ("10", All ["10000000000"])
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
            "R∫$Ĩ"
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
            [ ("[\"abc\",\"123\"]", All ["a1", "a2", "a3", "b1", "b2", "b3", "c1", "c2", "c3"])
            , ("[\"aa\",\"aba\"]", All ["aa", "ab", "aa", "aa", "ab", "aa"])
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
            [ ("1", All ["[[1]]"])
            , ("2", All ["[[1,0],[0,1]]"])
            , ("3", All ["[[1,0,1],[0,1,0],[1,0,1]]"])
            , ("4", All ["[[1,0,1,0],[0,1,0,1],[1,0,1,0],[0,1,0,1]]"])
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
            [ ("1", All ["1"])
            , ("2", All ["2"])
            , ("3", All ["3"])
            , ("4", All ["8"])
            , ("5", All ["5"])
            , ("6", All ["36"])
            , ("7", All ["7"])
            , ("8", All ["64"])
            , ("9", All ["27"])
            , ("10", All ["100"])
            , ("12", All ["1728"])
            , ("14", All ["196"])
            , ("24", All ["331776"])
            , ("25", All ["125"])
            , ("28", All ["21952"])
            , ("30", All ["810000"])
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
            [ ("[2,3,3,1,5,2]", First $ Just "3")
            , ("[2,4,3,5,1]", First Nothing)
            ]
    describe "q136887: Fold a List in Half" $ do
        specEval
            ";ᶜç↔ᶻ+"
            [ ("[1,2,3,4,5,6,7,8]", All ["[9,9,9,9]"])
            , ("[1,2,3,4,5,6,7]", All ["[8,8,8,4]"])
            ]
    describe "q138510: Running second maximum of a list" $ do
        specEval
            "poil"
            [ ("[1,5,2,3,5,9,5,8]", All ["1", "2", "3", "5", "5", "5", "8"])
            , ("[1,1,2,2,3,3,4]", All ["1", "1", "2", "2", "3", "3"])
            , ("[2,1,0,-1,0,1,2]", All ["1", "1", "1", "1", "1", "2"])
            ]
    describe "q138982: Divisibility Streak" $ do
        specEval
            "ᵏ{ᵉ+→%Z"
            [ ("2", All ["1"])
            , ("3", All ["2"])
            , ("4", All ["1"])
            , ("5", All ["2"])
            , ("6", All ["1"])
            , ("7", All ["3"])
            , ("8", All ["1"])
            , ("9", All ["2"])
            , ("10", All ["1"])
            , ("2521", All ["10"])
            ]
    describe "q141949: Count edits accounting for grace period" $ do
        specEval
            "ˡ{C4+>‼"
            [ ("[0]", All ["1"])
            , ("[0,3,5,7]", All ["2"])
            , ("[0,3,4,7,9,10,11,12]", All ["3"])
            , ("[0,30,120]", All ["3"])
            , ("[0,4,8,12,16]", All ["3"])
            , ("[0,4,8,12,16,20]", All ["3"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]", All ["4"])
            , ("[0,5,10,15,20]", All ["5"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]", All ["5"])
            , ("[0,1,4,5,9,11,12,14,16,18,23,24,26,28,29,30]", All ["6"])
            ]
    describe "q142071: Find the sum of the divisors of N" $ do
        specEval
            "Ď∑"
            [ ("7", All ["8"])
            , ("15", All ["24"])
            , ("20", All ["42"])
            , ("1", All ["1"])
            , ("5", All ["6"])
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
            "∆A2<"
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
        specEval
            "∆:±="
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
    describe "q144233: How many Wazirs can be placed on an N×N Chessboard?" $ do
        specEval
            "*äK"
            [ ("7", All ["25"])
            , ("8", All ["32"])
            , ("100", All ["5000"])
            ]
    describe "q145518: Square pyramidal numbers" $ do
        specEval
            "R:∙"
            [ ("0", All ["0"])
            , ("4", All ["30"])
            , ("5", All ["55"])
            ]
    describe "q146059: Is my triangle right?" $ do
        specEval
            "*ĕ$∑="
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
            [ ("[\"Here are some lines\",\"of text for you\",\"to make a\",\"boustrophedon\"]", All ["[\"Here are some lines\",\"uoy rof txet fo\",\"to make a\",\"nodehportsuob\"]"])
            , ("[\"My boustrophedon\"]", All ["[\"My boustrophedon\"]"])
            , ("[]", All ["[]"])
            , ("[\"Some text\",[],\"More text\",[],[],\"Last bit of text\"]", All ["[\"Some text\",[],\"More text\",[],[],\"txet fo tib tsaL\"]"])
            ]
    describe "q152114: Output the hours at 90 degrees" $ do
        specEval
            "258Ɗ+12%→"
            [ ("1", All ["[4,7,10]"])
            , ("2", All ["[5,8,11]"])
            , ("3", All ["[6,9,12]"])
            , ("4", All ["[7,10,1]"])
            , ("5", All ["[8,11,2]"])
            , ("6", All ["[9,12,3]"])
            , ("7", All ["[10,1,4]"])
            , ("8", All ["[11,2,5]"])
            , ("9", All ["[12,3,6]"])
            , ("10", All ["[1,4,7]"])
            , ("11", All ["[2,5,8]"])
            , ("12", All ["[3,6,9]"])
            ]
        specEval
            "12Rᶠ{-Z3¦"
            [ ("1", All ["[4,7,10]"])
            , ("2", All ["[5,8,11]"])
            , ("3", All ["[6,9,12]"])
            , ("4", All ["[1,7,10]"])
            , ("5", All ["[2,8,11]"])
            , ("6", All ["[3,9,12]"])
            , ("7", All ["[1,4,10]"])
            , ("8", All ["[2,5,11]"])
            , ("9", All ["[3,6,12]"])
            , ("10", All ["[1,4,7]"])
            , ("11", All ["[2,5,8]"])
            , ("12", All ["[3,6,9]"])
            ]
    describe "q153783: The first n numbers without consecutive equal binary digits" $ do
        specEval
            "RË3÷"
            [ ("1", All ["[0]"])
            , ("18", All ["[0,1,2,5,10,21,42,85,170,341,682,1365,2730,5461,10922,21845,43690,87381]"])
            ]
    describe "q162254: Generate a Walsh Matrix" $ do
        specEval
            "Ë:ᵒ{&Ƃ∑£E"
            [ ("0", All ["[[1]]"])
            , ("1", All ["[[1,1],[1,-1]]"])
            , ("2", All ["[[1,1,1,1],[1,-1,1,-1],[1,1,-1,-1],[1,-1,-1,1]]"])
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
            "ĉᵐ∫j"
            [ ("[0,1,1,1,0,1,1,0,0,0,1,1,1,1,1,1]", All ["[0,1,2,3,0,1,2,0,0,0,1,2,3,4,5,6]"])
            , ("[0,1,1,0,1,0,1,1,1,1,1,0,1,0,1,1,0,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1]", All ["[0,1,2,0,1,0,1,2,3,4,5,0,1,0,1,2,0,1,2,3,4,5,6,0,1,0,1,2,3,4,5,6,7,8]"])
            , ("[1,1,1,1,1,1,1,1,1,1,1,1,0,1]", All ["[1,2,3,4,5,6,7,8,9,10,11,12,0,1]"])
            ]
    describe "q169724: Is this number evil?" $ do
        specEval
            "Ƃ∑½"
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
            [ ("[2,3,4,5,6]", All ["12"])
            , ("[7,2]", All ["14"])
            , ("[2,3,3]", All ["6"])
            , ("[3,3,3]", All ["9"])
            , ("[1,1,1,1,2,2]", All ["2"])
            , ("[6,200,10,120]", All ["1200"])
            , ("[2,3,4,5,6,7,8,8]", All ["24"])
            , ("[5,2,9,10,3,4,4,4,7]", All ["20"])
            , ("[9,7,10,9,7,8,5,10,1]", All ["63", "90", "70"])
            ]
    describe "q170695: Construct a line graph / conjugate graph" $ do
        specEval
            "Sᵖ{đ∩z}ᵐj"
            [ ("[]", All [])
            , ("[[\"0\",\"1\"]]", All [])
            , ("[[\"0\",\"1\"],[\"1\",\"2\"]]", All ["[\"01\",\"12\"]"])
            , ("[[\"a\",\"b\"],[\"b\",\"c\"],[\"c\",\"a\"]]", All ["[\"ab\",\"bc\"]", "[\"ab\",\"ca\"]", "[\"bc\",\"ca\"]"])
            ]
    describe "q177221: String rotation - output string repeatedly moving first character to the end" $ do
        specEval
            ";$N,"
            [ ("\"john\"", All ["ohnj", "hnjo", "njoh", "john"])
            , ("\"heehee\"", All ["eeheeh", "eheehe", "heehee", "eeheeh", "eheehe", "heehee"])
            ]
        specEval
            "xŘ↔"
            [ ("\"john\"", All ["[\"njoh\",\"hnjo\",\"ohnj\",\"john\"]"])
            , ("\"heehee\"", All ["[\"eheehe\",\"eeheeh\",\"heehee\",\"eheehe\",\"eeheeh\",\"heehee\"]"])
            ]
    describe "q175248: The inverse Collatz Conjecture" $ do
        specEval
            "ᶦ{Z:←½$3*→I"
            [ ("0", All ["0"])
            , ("1", All ["1", "0"])
            , ("2", All ["2", "7", "3", "1", "0"])
            , ("3", All ["3", "1", "0"])
            , ("10", All ["10", "31", "15", "7", "3", "1", "0"])
            , ("14", All ["14", "43", "21", "10", "31", "15", "7", "3", "1", "0"])
            ]
    describe "q179464: Covering a Skyline with brush strokes" $ do
        specEval
            "ç-P‼∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", All ["9"])
            , ("[5,8]", All ["8"])
            , ("[1,1,1,1]", All ["1"])
            , ("[]", All ["0"])
            , ("[0,0]", All ["0"])
            , ("[2]", All ["2"])
            , ("[2,0,2]", All ["4"])
            , ("[10,9,8,9]", All ["11"])
            ]
        specEval
            "çM-_∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", All ["9"])
            , ("[5,8]", All ["8"])
            , ("[1,1,1,1]", All ["1"])
            , ("[]", All ["0"])
            , ("[0,0]", All ["0"])
            , ("[2]", All ["2"])
            , ("[2,0,2]", All ["4"])
            , ("[10,9,8,9]", All ["11"])
            ]
    describe "q180302: Count repetitions of an array" $ do
        specEval
            "u∕u#"
            [ ("[1,10,16,4,8,10,9,19,2,15,18,19,10,9,17,15,19,5,13,20]", All ["4"])
            , ("[11,8,6,15,9,19,2,2,4,19,14,19,13,12,16,13,0,5,0,8]", All ["5"])
            , ("[9,7,8,16,3,9,20,19,15,6,8,4,18,14,19,12,12,16,11,19]", All ["5"])
            , ("[10,17,17,7,2,18,7,13,3,10,1,5,15,4,6,0,19,4,17,0]", All ["5"])
            , ("[12,7,17,13,5,3,4,15,20,15,5,18,18,18,4,8,15,13,11,13]", All ["5"])
            , ("[0,3,6,1,5,2,16,1,6,3,12,1,16,5,4,5,6,17,4,8]", All ["6"])
            , ("[11,19,2,3,11,15,19,8,2,12,12,20,13,18,1,11,19,7,11,2]", All ["4"])
            , ("[6,4,11,14,17,3,17,11,2,16,14,1,2,1,15,15,12,10,11,13]", All ["6"])
            , ("[0,19,2,0,10,10,16,9,19,9,15,0,10,18,0,17,18,18,0,9]", All ["5"])
            , ("[1,19,17,17,0,2,14,10,10,12,5,14,16,7,15,15,18,11,17,7]", All ["5"])
            ]
        specEval
            "Ţ~1>"
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
            [ ("80", First $ Just "79")
            , ("100", First $ Just "101")
            , ("5", First $ Just "5")
            , ("9", First $ Just "7")
            , ("532", First $ Just "523")
            , ("1", First $ Just "2")
            ]
    describe "q186881: First occurrence in the Sixers sequence" $ do
        specEval
            "Ň6*ƊajĭÐɗ$Ĩ"
            [ ("0", First $ Just "241")
            , ("17", First $ Just "297")
            , ("36", First $ Just "80")
            , ("55", First $ Just "128")
            , ("82", First $ Just "2")
            , ("95", First $ Just "557")
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
            [("[4,4,4,7,7,9,9,9,9,2,2,2,4,4]", All ["1", "1", "1", "2", "2", "3", "3", "3", "3", "4", "4", "4", "5", "5"])]
    describe "q195592: Average Two Letters" $ do
        specEval
            "µkH"
            [ ("[\"A\",\"C\"]", All ["B"])
            , ("[\"a\",\"z\"]", All ["m"])
            , ("[\"d\",\"j\"]", All ["g"])
            , ("[\"B\",\"e\"]", All ["S"])
            , ("[\"Z\",\"a\"]", All ["]"])
            ]
    describe "q196683: Round up my number" $ do
        specEval
            "/K*"
            [ ("3 1", All ["3"])
            , ("3 5", All ["6"])
            , ("3 9", All ["9"])
            , ("5 12", All ["15"])
            ]
    describe "q199290: Reversed Iota's" $ do
        specEval
            "RRᵐ↔"
            [("4", All ["[[1],[2,1],[3,2,1],[4,3,2,1]]"])]
        specEval
            "RpN↔"
            [("4", All ["[1]", "[2,1]", "[3,2,1]", "[4,3,2,1]"])]
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
    describe "q203797: Generate list of numbers and their negative counterparts" $ do
        specEval
            "ïᶜ_"
            [ ("9 6", All ["[6,7,8,9]", "[-6,-7,-8,-9]"])
            , ("6 6", All ["[6]", "[-6]"])
            ]
    describe "q206853: Find the perfect square!" $ do
        specEval
            "Ď√‼Ṁ"
            [ ("4", All ["2"])
            , ("9", All ["3"])
            , ("12", All ["2"])
            , ("13", All ["1"])
            , ("108", All ["6"])
            ]
    describe "q207736: The shortest way to find one unique value when all other values are the same" $ do
        specEval
            "Ţṃ"
            [ ("[1,1,1,2,1,1]", All ["2"])
            , ("[3,5,5,5,5]", All ["3"])
            , ("[9,2,9,9,9,9,9]", All ["2"])
            , ("[4,4,4,6]", All ["6"])
            , ("[5,8,8]", All ["5"])
            , ("[8,5,8]", All ["5"])
            , ("[8,8,5]", All ["5"])
            ]
        specEval
            "ĕᵖf"
            [ ("[1,1,1,2,1,1]", All ["2"])
            , ("[3,5,5,5,5]", All ["3"])
            , ("[9,2,9,9,9,9,9]", All ["2"])
            , ("[4,4,4,6]", All ["6"])
            , ("[5,8,8]", All ["5"])
            , ("[8,5,8]", All ["5"])
            , ("[8,8,5]", All ["5"])
            ]
        specEval
            "oĉ~z"
            [ ("[1,1,1,2,1,1]", All ["2"])
            , ("[3,5,5,5,5]", All ["3"])
            , ("[9,2,9,9,9,9,9]", All ["2"])
            , ("[4,4,4,6]", All ["6"])
            , ("[5,8,8]", All ["5"])
            , ("[8,5,8]", All ["5"])
            , ("[8,8,5]", All ["5"])
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
            [ ("100 1", All ["21"])
            , ("200 11", All ["138"])
            , ("678 123", All ["182"])
            ]
    describe "q216734: Jelly's Untruth" $ do
        specEval
            "ËƂ∑±"
            [ ("[0,2,4,5]", All ["[1,0,1,0,1,1]"])
            , ("[4]", All ["[0,0,0,0,1]"])
            , ("[1,0,0,1]", All ["[1,1]"])
            , ("[4,3,2]", All ["[0,0,1,1,1]"])
            , ("[0]", All ["[1]"])
            ]
    describe "q217303: Linear integer function generator" $ do
        specEval
            "ᵉᵑ{ˣ∙ɔᵈç}T"
            [ ("10 [0,1] [1,1]", All ["[0,1,1,2,3,5,8,13,21,34]"])
            , ("20 [1,0,0] [1,1,0]", All ["[1,0,0,1,0,1,1,1,2,2,3,4,5,7,9,12,16,21,28,37]"])
            , ("10 [3,0,2] [1,1,0]", All ["[3,0,2,3,2,5,5,7,10,12]"])
            , ("5 [0,0] [1,1]", All ["[0,0,0,0,0]"])
            , ("20 [0,-1,0,1] [0,1,0,-1]", All ["[0,-1,0,1,-2,2,-1,-1,3,-4,3,0,-4,7,-7,3,4,-11,14,-10]"])
            ]
    describe "q224125: Replace all items with their counts" $ do
        specEval
            "ᵐĈ"
            [ ("[1]", All ["[1]"])
            , ("[1,2]", All ["[1,1]"])
            , ("[1,1]", All ["[2,2]"])
            , ("[1,4,4]", All ["[1,2,2]"])
            , ("[4,4,2]", All ["[2,2,1]"])
            , ("[4,4,4,4]", All ["[4,4,4,4]"])
            , ("[10,20,10,20]", All ["[2,2,2,2]"])
            , ("[1,2,2,4,4,4,4]", All ["[1,2,2,4,4,4,4]"])
            , ("[1,2,2,1,4,8,1]", All ["[3,2,2,3,1,1,3]"])
            ]
    describe "q225203: Delannoy numbers" $ do
        specEval
            "Ṁ→ᵒÇ∏ƃ"
            [ ("[5,8]", All ["13073"])
            , ("[5,7]", All ["7183"])
            , ("[3,9]", All ["1159"])
            , ("[8,6]", All ["40081"])
            , ("[1,7]", All ["15"])
            , ("[7,0]", All ["1"])
            , ("[11,6]", All ["227305"])
            , ("[0,4]", All ["1"])
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
            "O:ᵐ∑≡$Ðaṁl"
            [ ("[9,5,1,2,9,2]", All ["[[1,2,9,2],[9,5]]"])
            , ("[1,1,3,5,7,4]", All ["[[3,4],[7],[1,1,5]]"])
            , ("[2,9,6,1,5,8,2]", All ["[[1,8,2],[6,5],[2,9]]"])
            , ("[5,4,1]", All ["[[4,1],[5]]"])
            , ("[3,8,1,4,2,2]", All ["[[3,1,4,2],[8,2]]"])
            , ("[6,9,3,8,1]", All ["[[8,1],[6,3],[9]]"])
            , ("[4,1,6,9,1,4,5,2]", All ["[[1,9,4,2],[4,6,1,5]]"])
            , ("[8,7,8,6,1]", All ["[[8,6,1],[7,8]]"])
            , ("[2,7,4,5]", All ["[[4,5],[2,7]]"])
            , ("[5,2,1,4,4]", All ["[[4,4],[5,2,1]]"])
            , ("[5,7,4,6,2]", All ["[[4,6,2],[5,7]]"])
            , ("[4,1,6,6,9]", All ["[[4,9],[1,6,6]]"])
            , ("[2,6,4]", All ["[[2,4],[6]]"])
            , ("[6,3,1,6,8,4,5,7]", All ["[[1,8,4,7],[6,3,6,5]]"])
            , ("[2,2,2]", All ["[[2],[2],[2]]"])
            , ("[2,4,5]", All ["[[2,4,5]]"])
            ]
    describe "q229624: Generalised multi-dimensional chess knight's moves" $ do
        specEval
            "8ᵚ~ᵖ{≈←ň‼į="
            [ ("[0,7]", All ["[1,5]", "[2,6]"])
            , ("[3,4]", All ["[1,3]", "[1,5]", "[2,2]", "[2,6]", "[4,2]", "[4,6]", "[5,3]", "[5,5]"])
            , ("[7,7,7]", All ["[5,6,7]", "[5,7,6]", "[6,5,7]", "[6,7,5]", "[7,5,6]", "[7,6,5]"])
            ]
        specEval
            "0*2R+ŋ↕ũ+ň8<"
            [ ("[0,7]", All ["[1,5]", "[2,6]"])
            , ("[3,4]", All ["[4,6]", "[4,2]", "[2,6]", "[2,2]", "[5,5]", "[5,3]", "[1,5]", "[1,3]"])
            , ("[7,7,7]", All ["[6,5,7]", "[6,7,5]", "[5,6,7]", "[5,7,6]", "[7,6,5]", "[7,5,6]"])
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
            [ ("1", All ["1"])
            , ("2", All ["6"])
            , ("3", All ["25"])
            , ("4", All ["90"])
            , ("5", All ["301"])
            , ("6", All ["966"])
            , ("7", All ["3025"])
            , ("8", All ["9330"])
            , ("9", All ["28501"])
            , ("10", All ["86526"])
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
    describe "q233641: Hunt for discount" $ do
        specEval
            "o↔ĭ∑ä"
            [ ("[10]", All ["0"])
            , ("[10,20]", All ["5"])
            , ("[10,20,30]", All ["10"])
            , ("[2,2,2,2]", All ["2"])
            , ("[4,10,6,8,2,40]", All ["9"])
            ]
    describe "q235964: Implement the hyperfactorial" $ do
        specEval
            "R:E∏"
            [ ("0", All ["1"])
            , ("1", All ["1"])
            , ("2", All ["4"])
            , ("3", All ["108"])
            , ("4", All ["27648"])
            , ("5", All ["86400000"])
            , ("6", All ["4031078400000"])
            , ("7", All ["3319766398771200000"])
            , ("8", All ["55696437941726556979200000"])
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
    describe "q238607: Converge to a number" $ do
        specEval
            "ƊsC↔~cɗ-"
            [ ("4", All ["1", "2", "3", "4"])
            , ("16", All ["10", "11", "12", "13", "14", "15", "16"])
            , ("35", All ["10", "20", "30", "31", "32", "33", "34", "35"])
            , ("103", All ["100", "101", "102", "103"])
            , ("320", All ["100", "200", "300", "310", "320"])
            , ("354", All ["100", "200", "300", "310", "320", "330", "340", "350", "351", "352", "353", "354"])
            , ("1000", All ["1000"])
            , ("1001", All ["1000", "1001"])
            , ("4037 ", All ["1000", "2000", "3000", "4000", "4010", "4020", "4030", "4031", "4032", "4033", "4034", "4035", "4036", "4037"])
            ]
        specEval
            "¢Bx¢E$y↔∫"
            [ ("4", All ["[1,2,3,4]"])
            , ("16", All ["[10,11,12,13,14,15,16]"])
            , ("35", All ["[10,20,30,31,32,33,34,35]"])
            , ("103", All ["[100,101,102,103]"])
            , ("320", All ["[100,200,300,310,320]"])
            , ("354", All ["[100,200,300,310,320,330,340,350,351,352,353,354]"])
            , ("1000", All ["[1000]"])
            , ("1001", All ["[1000,1001]"])
            , ("4037 ", All ["[1000,2000,3000,4000,4010,4020,4030,4031,4032,4033,4034,4035,4036,4037]"])
            ]
    describe "q241267: Remove odd indices and double the even indices" $ do
        specEval
            "ĭ:Ĭ"
            [ ("\"abcdef\"", All ["bbddff"])
            , ("\"umbrella\"", All ["mmrrllaa"])
            , ("\"looooooooong text\"", All ["ooooooooooggttxx"])
            , ("\"abc\"", All ["bb"])
            , ("\"xkcd\"", All ["kkdd"])
            , ("\"Hello, World!\"", All ["eell,,WWrrdd"])
            , ("\"D\"", All ["[]"])
            , ("\"KK\"", All ["KK"])
            , ("\"Hi\"", All ["ii"])
            , ("\"odd_length!\"", All ["dd__eegghh"])
            , ("\"\"", All ["[]"])
            ]
    describe "q241474: Move to Right and left" $ do
        specEval
            "çç+"
            [ ("[1,2,3]", All ["[1,2,4,2,3]"])
            , ("[4,2]", All ["[4,2,4,2]"])
            , ("[1]", All ["[1,0,1]"])
            , ("[7,4,5,6]", All ["[7,4,12,10,5,6]"])
            , ("[1,2,4,2,1]", All ["[1,2,5,4,5,2,1]"])
            , ("[1,0,1]", All ["[1,0,2,0,1]"])
            , ("[1,0,2,0,1]", All ["[1,0,3,0,3,0,1]"])
            , ("[8,7,6,5,4]", All ["[8,7,14,12,10,5,4]"])
            , ("[1,4,9,16]", All ["[1,4,10,20,9,16]"])
            , ("[1,2]", All ["[1,2,1,2]"])
            ]
        specEval
            "5Ƃ×"
            [ ("[1,2,3]", All ["[1,2,4,2,3]"])
            , ("[4,2]", All ["[4,2,4,2]"])
            , ("[1]", All ["[1,0,1]"])
            , ("[7,4,5,6]", All ["[7,4,12,10,5,6]"])
            , ("[1,2,4,2,1]", All ["[1,2,5,4,5,2,1]"])
            , ("[1,0,1]", All ["[1,0,2,0,1]"])
            , ("[1,0,2,0,1]", All ["[1,0,3,0,3,0,1]"])
            , ("[8,7,6,5,4]", All ["[8,7,14,12,10,5,4]"])
            , ("[1,4,9,16]", All ["[1,4,10,20,9,16]"])
            , ("[1,2]", All ["[1,2,1,2]"])
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
            [ ("[[1]]", All ["1"])
            , ("[[1,0,1]]", All ["2"])
            , ("[[1,0],[0,1]]", All ["1"])
            , ("[[0,1,0],[0,0,1],[1,1,1]]", All ["1"])
            , ("[[0,1,1,0],[1,0,1,1],[1,1,0,1],[0,1,1,0]]", All ["-1"])
            , ("[[0,0,1,1,0],[0,1,1,1,1],[1,1,0,1,1],[0,1,1,0,0]]", All ["0"])
            , ("[[1,1,1,0,1,1,1],[1,0,1,0,1,0,1],[1,1,1,0,1,1,1]]", All ["0"])
            , ("[[1,1,1,1,1],[1,0,0,0,1],[1,0,1,0,1],[1,0,0,0,1],[1,1,1,1,1]] ", All ["1"])
            ]
    describe "q247326: There's more than one way to skin a set" $ do
        specEval
            "S∑a:u∕u"
            [ ("[1]", All ["[]"])
            , ("[4,5,2]", All ["[]"])
            , ("[9,10,11,12]", All ["[21]"])
            , ("[2,3,5,6]", All ["[5,8,11]"])
            , ("[15,16,7,1,4]", All ["[16,23,20,27]"])
            , ("[1,2,3,4,5]", All ["[3,4,5,6,7,8,9,10,11,12]"])
            ]
        specEval
            "Ë→Ƃʳ×2m2Ĩ"
            [ ("[1]", All [])
            , ("[4,5,2]", All [])
            , ("[9,10,11,12]", All ["21"])
            , ("[2,3,5,6]", All ["5", "8", "11"])
            , ("[15,16,7,1,4]", All ["16", "20", "23", "27"])
            , ("[1,2,3,4,5]", All ["3", "4", "5", "6", "7", "8", "9", "10", "11", "12"])
            ]
    describe "q247398: Alternating sums of multidimensional arrays" $ do
        specEval
            "ʷ{£d"
            [ ("[1]", All ["1"])
            , ("[-1]", All ["-1"])
            , ("[1,2]", All ["-1"])
            , ("[2,0,4]", All ["6"])
            , ("[1,-2]", All ["3"])
            , ("[[1]]", All ["1"])
            , ("[[1,2],[4,8]]", All ["3"])
            , ("[[-1,-1],[2,2]]", All ["0"])
            , ("[[[[1],[2]],[[4],[8]]]]", All ["3"])
            , ("[[[1,2],[2,4],[4,8]],[[-4,-4],[-1,1],[2,-2]]]", All ["-9"])
            ]
    describe "q247676: Generate All 8 Knight's Moves" $ do
        specEval
            "į→ŋ"
            [("", All ["[1,2]", "[1,-2]", "[-1,2]", "[-1,-2]", "[2,1]", "[2,-1]", "[-2,1]", "[-2,-1]"])]
    describe "q248445: Print the power set of the power set ... of an empty set" $ do
        specEval
            "Øᶦ{Sa"
            [("", Truncated ["[]", "[[]]", "[[],[[]]]", "[[],[[]],[[[]]],[[],[[]]]]"])]
    describe "q248991: The Unaverageables" $ do
        specEval
            "ᶠ{+ä∩z"
            [ ("[1,2,3]", All ["[2]"])
            , ("[1,2,3,4]", All ["[]"])
            , ("[1,3,4,5]", All ["[4]"])
            , ("[1,5,10,20,40]", All ["[1,5,10,20,40]"])
            , ("[1,5,6,10]", All ["[1,5,6,10]"])
            , ("[1,2,3,4,10,52,100,200]", All ["[10,52,200]"])
            , ("[1,2,3,5,8,13,21,34]", All ["[]"])
            ]
    describe "q249868: Every possible pairing" $ do
        specEval
            "O2ᵚL"
            [ ("2", All ["[[0,1]]"])
            , ("4", All ["[[2,3],[0,1]]", "[[1,3],[0,2]]", "[[0,3],[1,2]]"])
            ]
    describe "q250283: Rearrange to a palindrome" $ do
        specEval
            "↕:↔="
            [ ("\"nanas\"", First $ Just "nasan")
            , ("\"coconutnut\"", First $ Just "conuttunoc")
            , ("\"apotato\"", First $ Just "aotptoa")
            , ("\"canadadance\"", First $ Just "canadedanac")
            , ("\"nananana\"", First $ Just "nanaanan")
            , ("\"anaan\"", First $ Just "anana")
            ]
    describe "q250395: Have you heard of tralindromes?" $ do
        specEval
            "p::↔ᵃᶜt$,,="
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
            [ ("\"abcdabcd\"", All ["abcd", "abcdabcd"])
            , ("\"aaa\"", All ["a", "aaa"])
            , ("\"aaaaaaaa\"", All ["a", "aa", "aaaa", "aaaaaaaa"])
            , ("\"abcdef\"", All ["abcdef"])
            ]
    describe "q251594: Find the nth Mersenne Prime" $ do
        specEval
            "ŇË←Q"
            [("", Truncated ["3", "7", "31", "127", "8191"])]
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
            "ʷ{;\"tut-tut\"=ip,}ᵗN"
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
            [ ("2 22", All ["2"])
            , ("2 8", All ["1"])
            , ("2 15", All ["4"])
            , ("3 100", All ["6"])
            , ("10 12345", All ["46"])
            ]
    describe "q252082: Reconstruct Matrix from its diagonals" $ do
        specEval
            "#2÷:→:ᵒ{ᵋ{-+@}m@"
            [ ("[[5]]", All ["[[5]]"])
            , ("[[0],[1,69],[13]]", All ["[[1,0],[13,69]]"])
            , ("[[25],[0,1],[6,23,10],[420,9],[67]]", All ["[[6,0,25],[420,23,1],[67,9,10]]"])
            ]
        specEval
            "Øc;$ᶻ,ŤxᶻŘ"
            [ ("[[5]]", All ["[[5]]"])
            , ("[[0],[1,69],[13]]", All ["[[1,13],[0,69]]"])
            , ("[[25],[0,1],[6,23,10],[420,9],[67]]", All ["[[6,420,67],[9,0,23],[1,10,25]]"])
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
            [ ("2", All ["[1,2]"])
            , ("4", All ["[1,2,4]"])
            , ("5", All ["[1,3,5]"])
            , ("6", All ["[1,2,3,6]"])
            , ("25", All ["[1,25]"])
            , ("39", All ["[1,3,5,11,29,39]"])
            , ("42", All ["[1,2,7,14,21,42]"])
            , ("100", All ["[1,2,4,25,50,100]"])
            ]
    describe "q252303: Cut along the lines" $ do
        specEval
            "ĉJᵐj"
            [ ("[1,0]", All ["[[1],[0]]", "[[1,0]]"])
            , ("[1,1,1,1]", All ["[[1,1,1,1]]"])
            , ("[1,1,0,0,1]", All ["[[1,1],[0,0],[1]]", "[[1,1],[0,0,1]]", "[[1,1,0,0],[1]]", "[[1,1,0,0,1]]"])
            ]
    describe "q252927: Make a Court Transcriber" $ do
        specEval
            "ᶠ{JS=}ş"
            [ ("[\"dictionary\",\"transcriber\"] [\"dic\",\"ion\",\"ary\"]", All ["dictionary"])
            , ("[\"dictionary\",\"transcriber\"] [\"tra\",\"scr\",\"ber\"]", All ["transcriber"])
            ]
    describe "q254224: Maximum average ord" $ do
        specEval
            "ᵐµṀ"
            [ ("[\"hello\",\"world\",\"bye\"]", All ["552/5"])
            , ("[\"code\",\"golf\",\"stack\",\"exchange\"]", All ["534/5"])
            , ("[\"!@#\",\"$%^\",\"&*(\"]", All ["167/3"])
            , ("[\"qwertyuiop[\",\"asdfghjkl;\",\"zxcvbnm,\"]", All ["1220/11"])
            ]
    describe "q254947: Range of ASCII values" $ do
        specEval
            "o:h-l"
            [ ("\"Hello, World!\"", All ["82"])
            , ("\"aaaaa\"", All ["0"])
            , ("\"Code Golf\"", All ["79"])
            , ("\"Stack Exchange\"", All ["88"])
            , ("\"ASCII\"", All ["18"])
            , ("\"eo:h-l\"", All ["66"])
            ]
    describe "q255274: CGAC2022 Day 7: Fen The Wicked" $ do
        specEval
            "p↔:#:ËGT∑"
            [ ("[]", All [])
            , ("[999]", All ["999"])
            , ("[3,1,4]", All ["3", "4", "4"])
            , ("[3,1,4,1,5,9,2,6]", All ["3", "4", "4", "9", "5", "14", "2", "31"])
            ]
    describe "q255373: CGAC2022 Day 10: Help Santa sort presents!" $ do
        specEval
            "ˡ{ᵗ≡ĭ?}aṀ"
            [ ("[1]", All ["0"])
            , ("[1,0,1,0,1,0,1]", All ["1"])
            , ("[1,0,1,0,1,1]", All ["3"])
            , ("[1,0,1,1,0,0,1,1,0,1,1,1,0,0,0,1,1,0,1,0,1,1,1,0,0]", All ["5"])
            ]
    describe "q255650: Sum every second digit in a number" $ do
        specEval
            "Ɗĭ∑"
            [ ("10", All ["0"])
            , ("101011", All ["1"])
            , ("548915381", All ["26"])
            , ("999999", All ["27"])
            , ("2147483647", All ["29"])
            , ("999999999", All ["36"])
            ]
    describe "q256017: CGAC2022 Day 25: When The Planets Align" $ do
        specEval
            "ᵏ{*+$/1%≡"
            [ ("[0,0] [0,0] [1,1]", All ["0"])
            , ("[1,0] [1,0] [100,100]", All ["99"])
            , ("[1,5,3] [0,1,0] [4,8,12]", All ["5"])
            ]
    describe "q256034: Normal Subgroups of S4" $ do
        specEval
            "@ᵃ{x-¬∑}ä-"
            [ ("[0,1,2,3]", All ["2"])
            , ("[0,1,3,2]", All ["0"])
            , ("[0,2,1,3]", All ["0"])
            , ("[0,2,3,1]", All ["1/2"])
            , ("[0,3,1,2]", All ["1/2"])
            , ("[0,3,2,1]", All ["0"])
            , ("[1,0,2,3]", All ["0"])
            , ("[1,0,3,2]", All ["-2"])
            , ("[1,2,0,3]", All ["1/2"])
            , ("[1,2,3,0]", All ["0"])
            , ("[1,3,0,2]", All ["0"])
            , ("[1,3,2,0]", All ["1/2"])
            ]
    describe "q256147: Find the Prime Signature" $ do
        specEval
            "ƒo↔"
            [ ("1", All ["[]"])
            , ("2", All ["[1]"])
            , ("4", All ["[2]"])
            , ("6", All ["[1,1]"])
            , ("8", All ["[3]"])
            , ("12", All ["[2,1]"])
            , ("16", All ["[4]"])
            , ("24", All ["[3,1]"])
            , ("30", All ["[1,1,1]"])
            , ("32", All ["[5]"])
            , ("36", All ["[2,2]"])
            , ("1234567", All ["[1,1]"])
            , ("5174928", All ["[5,4,3]"])
            , ("8388608", All ["[23]"])
            , ("9999991", All ["[1]"])
            ]
    describe "q256502: Guess the song title" $ do
        specEval
            "ŢṂaş"
            [("[\"Hello, world\",\"Hello, world\",\"I just got to say it, hello world\",\"Goodbye, world\",\"Goodbye, world\",\"Goodbye\"]", All ["Hello, world"])]
    describe "q256814: Knight to fork!" $ do
        specEval
            "ᵐ{į→ŋ+}≡H"
            [ ("[\"d6\",\"f6\",\"g3\"]", All ["e4"])
            , ("[\"d4\",\"d6\",\"e7\"]", All ["f5"])
            , ("[\"c3\",\"f2\",\"b2\"]", All ["d1"])
            ]
    describe "q256920: Simplify a Cycle" $ do
        specEval
            "Cᶦ{ᵈsf¡C"
            [ ("[1,2,3,4,2,5]", All ["1", "2", "5"])
            , ("[4,3,6,2,3,8,5,2,8,7]", All ["4", "3", "8", "7"])
            , ("[1,1,2]", All ["1", "2"])
            , ("[1,2,7,2,7,2,3,7]", All ["1", "2", "3", "7"])
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
            "↕ᵃ{Jᵐɗ\"∩<\"<60b}-_Paṁ"
            [ ("[1,1,4,3]", All ["91"])
            , ("[0,1,0,1]", All ["9"])
            , ("[1,7,3,8]", All ["59"])
            , ("[1,4,2,1]", All ["413"])
            , ("[1,3,2,0]", All ["413"])
            , ("[2,3,4,1]", All [])
            , ("[0,0,0,0]", All [])
            ]
    describe "q257649: Arbitrary Apple Dilemma" $ do
        specEval
            ":←/∏*"
            [ ("[3,4,6] 10", All ["24"])
            , ("[2] 14", All ["28"])
            , ("[6] 30", All ["36"])
            , ("[4,3], 20", All ["40"])
            , ("[5,8,7], 9", All ["15"])
            , ("[2,9,4,8], 7", All ["24"])
            ]
    describe "q257372: Smallest Bit Rotation" $ do
        specEval
            "ƂxŘƃṁ"
            [ ("177", All ["27"])
            , ("1", All ["1"])
            , ("12", All ["3"])
            , ("23", All ["15"])
            , ("34", All ["5"])
            , ("45", All ["27"])
            , ("56", All ["7"])
            , ("67", All ["7"])
            , ("78", All ["29"])
            , ("89", All ["27"])
            , ("100", All ["19"])
            ]
    describe "q257458: Sum of Consecutive Squares" $ do
        specEval
            "qᵗz:∙="
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
            [ ("2", Truncated ["1", "2", "3", "4", "5", "6"])
            , ("3", Truncated ["5", "7", "11", "14", "15", "16", "17", "19"])
            , ("4", Truncated ["27", "30", "39", "45"])
            ]
        specEval
            "Ňᵖ{$Bçu#="
            [ ("2", Truncated ["1", "2", "3", "4", "5", "6"])
            , ("3", Truncated ["5", "7", "11", "14", "15", "16", "17", "19"])
            , ("4", Truncated ["27", "30", "39", "45"])
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
            [ ("0", All ["1"])
            , ("1", All ["0"])
            , ("2", All ["1"])
            , ("3", All ["2"])
            , ("4", All ["6"])
            , ("5", All ["18"])
            , ("6", All ["57"])
            , ("7", All ["186"])
            , ("8", All ["622"])
            , ("9", All ["2120"])
            ]
    describe "q258299: Primes with Distinct Prime Digits" $ do
        specEval
            "ƤƊQůɗ"
            [("", Truncated ["2", "3", "5", "7", "23", "37", "53", "73", "257", "523", "2357", "2753", "3257", "3527", "5237", "5273", "7253", "7523"])]
        specEval
            "¢SQ↕ɗQao"
            [("", All ["[2,3,5,7,23,37,53,73,257,523,2357,2753,3257,3527,5237,5273,7253,7523]"])]
    describe "q258335: Shortest Code to Find the Smallest Missing Positive Integer" $ do
        specEval
            "ŇPᵖf"
            [ ("[1,2,3]", First $ Just "4")
            , ("[3,4,-1,1]", First $ Just "2")
            , ("[7,8,9,11,12]", First $ Just "1")
            , ("[-5,-4,-3,-2,-1,0,1,2,3,5,7,10]", First $ Just "4")
            , ("[]", First $ Just "1")
            , ("[-1,-4,-7]", First $ Just "1")
            ]
    describe "q258432: Shortest code to generate all Pythagorean triples up to a given limit" $ do
        specEval
            "RSᵖ{:*Ɔ$đ+="
            [ ("15", All ["[3,4,5]", "[6,8,10]", "[5,12,13]", "[9,12,15]"])
            , ("5", All ["[3,4,5]"])
            ]
        specEval
            "RS2L::∙√ɔ$≤"
            [ ("15", All ["[3,4,5]", "[6,8,10]", "[5,12,13]", "[9,12,15]"])
            , ("5", All ["[3,4,5]"])
            ]
    describe "q258511: Longest Valid Parentheses" $ do
        specEval
            "q£E→∫x>çƆᵖLaṀ"
            [ ("\"(()())\"", All ["6"])
            , ("\")()())\"", All ["4"])
            , ("\"()(())\"", All ["6"])
            , ("\"()(()\"", All ["2"])
            , ("\"))\"", All ["0"])
            , ("\"\"", All ["0"])
            ]
    describe "q258951: \"Sort\" by element duplication" $ do
        specEval
            "ᶦ{:Ɔ≥$tI}ṁ"
            [ ("[4,3,1,2]", All ["1", "1", "1", "2"])
            , ("[1,2,3,4]", All ["1", "2", "3", "4"])
            , ("[3,2,1,0]", All ["0", "1", "2", "3"])
            , ("[1,2,3,1]", All ["1", "1", "2", "3"])
            , ("[101,103,101,105]", All ["101", "101", "101", "105"])
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
            "R↔$∆çJᵐ{CᵈAc}-ň"
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
            "į3ƂÐ3~ᵑᵐç3~ᵑçaᵐᶜ{0*}∑2<"
            [ ("", Count 215)
            , ("", Truncated ["[[0,1,0,1],[1,1,1,1],[0,1,0,1],[1,1,1,1]]", "[[0,1,0,1],[1,1,1,1],[0,1,1,0],[1,1,1,1]]"])
            ]
    describe "q259633: Make a Custom Bayer Matrix" $ do
        specEval
            "ᵒ{ᵃƂᵈ:≈Ä+ç4ŗd"
            [ ("1", All ["[[0]]"])
            , ("2", All ["[[0,1/2],[3/4,1/4]]"])
            , ("4", All ["[[0,1/2,1/8,5/8],[3/4,1/4,7/8,3/8],[3/16,11/16,1/16,9/16],[15/16,7/16,13/16,5/16]]"])
            ]
        specEval
            "0UU$ᵑ{4*4ᵐ+↔3Ř;ᶻᶻ,j"
            [ ("0", All ["[[0]]"])
            , ("1", All ["[[0,2],[3,1]]"])
            , ("2", All ["[[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]"])
            ]
    describe "q259707: Shortest distinguishable slice" $ do
        specEval
            "∑ᵚ+xqNᵖ{ᵚ@ů}aşᵉhl→Ð"
            [ ("[\"happy\",\"angry\",\"hungry\"]", First $ Just "[1,2]")
            , ("[\"sheer\",\"shrew\",\"shine\",\"shire\",\"spike\",\"shy\"]", First $ Just "[2,4]")
            , ("[\"snap\",\"crackle\",\"pop\",\"smack\",\"sizzle\",\"whiff\",\"sheen\"]", First $ Just "[0,2]")
            ]
    describe "q259881: The Jaccard Index" $ do
        specEval
            ",Ţ←µ"
            [ ("[1,2] []", All ["0"])
            , ("[-7,3,-9] [9,2,3,4]", All ["1/6"])
            , ("[1,2,3] [2,4,6]", All ["1/5"])
            , ("[0,64] [0,64,89,93]", All ["1/2"])
            , ("[6,42,7,1] [42,7,6]", All ["3/4"])
            , ("[3,6,9] [3,6,9]", All ["1"])
            ]
        specEval
            "ᵋ∩Ŭᵃ#/"
            [ ("[1,2] []", All ["0"])
            , ("[-7,3,-9] [9,2,3,4]", All ["1/6"])
            , ("[1,2,3] [2,4,6]", All ["1/5"])
            , ("[0,64] [0,64,89,93]", All ["1/2"])
            , ("[6,42,7,1] [42,7,6]", All ["3/4"])
            , ("[3,6,9] [3,6,9]", All ["1"])
            ]
    describe "q259875: How Super is this Prime?" $ do
        specEval
            "ˡ{Qƥ}Ƃ#←"
            [ ("2", All ["0"])
            , ("3", All ["1"])
            , ("11", All ["2"])
            , ("211", All ["1"])
            , ("277", All ["2"])
            , ("823", All ["0"])
            , ("4397", All ["2"])
            , ("5381", All ["3"])
            , ("171697", All ["2"])
            , ("499403", All ["2"])
            , ("648391", All ["3"])
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
            [ ("0", All ["0"])
            , ("1", All ["1"])
            , ("2", All ["1"])
            , ("3", All ["5"])
            , ("4", All ["19"])
            , ("5", All ["101"])
            , ("6", All ["619"])
            , ("7", All ["4421"])
            , ("8", All ["35899"])
            , ("9", All ["326981"])
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
            "↕∆A1>"
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
            [ ("[]", All ["0"])
            , ("[[1,1]]", All ["1"])
            , ("[[1,2],[2,1],[2,2],[2,3],[3,2]]", All ["2"])
            ]
    describe "q260472: Find Index of Rational Number in Calkin-Wilf Sequence" $ do
        specEval
            "ˡ{ŗ:KÄ$-←"
            [ ("1", All ["1"])
            , ("1/3", All ["4"])
            , ("4/3", All ["9"])
            , ("3/4", All ["14"])
            , ("53/37", All ["1081"])
            , ("37/53", All ["1990"])
            ]
    describe "q260804: Minkowski's ?(x) for rational x" $ do
        specEval
            "ᶦ{1%ŗ}kaC$∫←:_Ë£d§+ṇ$çlÐ"
            [ ("0/1", All ["[0,0]"])
            , ("1/1", All ["[1,0]"])
            , ("1/2", All ["[1,1]"])
            , ("-1/2", All ["[-1,1]"])
            , ("2/1", All ["[2,0]"])
            , ("1/3", All ["[1,2]"])
            , ("1/8", All ["[1,7]"])
            , ("2/5", All ["[3,3]"])
            , ("8/5", All ["[13,3]"])
            , ("58/27", All ["[1033,9]"])
            , ("30/73", All ["[399,10]"])
            , ("144/89", All ["[853,9]"])
            , ("-17/77", All ["[-767,13]"])
            , ("-17/99", All ["[-133,12]"])
            , ("355/113", All ["[12648447,22]"])
            , ("16000/1", All ["[16000,0]"])
            ]
    describe "q260811: Given 4 fence lengths, what's the largest rectangular yard you can make?" $ do
        specEval
            "o;*ṁ"
            [ ("[1,1,1,1]", All ["1"])
            , ("[1,2,3,4]", All ["3"])
            , ("[4,3,2,1]", All ["3"])
            , ("[90,1,2,1]", All ["2"])
            , ("[1,90,1,1]", All ["1"])
            , ("[44,51,50,36]", All ["1800"])
            , ("[3,3,3,3]", All ["9"])
            , ("[3,3,3,4]", All ["9"])
            , ("[3,4,3,4]", All ["12"])
            , ("[4,4,3,4]", All ["12"])
            , ("[4,4,4,4]", All ["16"])
            ]
        specEval
            "oĭ$∏"
            [ ("[1,1,1,1]", All ["1"])
            , ("[1,2,3,4]", All ["3"])
            , ("[4,3,2,1]", All ["3"])
            , ("[90,1,2,1]", All ["2"])
            , ("[1,90,1,1]", All ["1"])
            , ("[44,51,50,36]", All ["1800"])
            , ("[3,3,3,3]", All ["9"])
            , ("[3,3,3,4]", All ["9"])
            , ("[3,4,3,4]", All ["12"])
            , ("[4,4,3,4]", All ["12"])
            , ("[4,4,4,4]", All ["16"])
            ]
    describe "q260966: sum of a range of a sum of a range of n" $ do
        specEval
            "R∑R∑"
            [ ("1", All ["1"])
            , ("2", All ["6"])
            , ("3", All ["21"])
            , ("4", All ["55"])
            , ("5", All ["120"])
            , ("6", All ["231"])
            , ("7", All ["406"])
            , ("8", All ["666"])
            , ("9", All ["1035"])
            , ("10", All ["1540"])
            ]
    describe "q261325: Output endless powers of 2" $ do
        specEval
            "ŇË"
            [("", Truncated ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"])]
    describe "q261861: Lowest digit addition generator" $ do
        specEval
            "ᵏ{:Ɗ∑+="
            [ ("0", All ["0"])
            , ("29", All ["19"])
            , ("216", All ["198"])
            ]
    describe "q261908: Last odd digit of power of 2" $ do
        specEval
            "Ë¢BÖ1Ĩ"
            [ ("1", First Nothing)
            , ("2", First Nothing)
            , ("3", First Nothing)
            , ("4", First $ Just "1")
            , ("5", First $ Just "1")
            , ("6", First Nothing)
            , ("7", First $ Just "2")
            , ("8", First $ Just "1")
            , ("9", First $ Just "1")
            , ("10", First $ Just "3")
            ]
    describe "q262032: Vertices of a regular dodecahedron" $ do
        specEval
            "7Ƃ89\\55:ŗÐçxŘ~?ŋ"
            [ ("", Truncated ["[1,1,1]", "[1,1,-1]", "[1,-1,1]", "[1,-1,-1]"])
            , ("", Count 20)
            ]
    describe "q262140: XOR of independent Bernoulli variables" $ do
        specEval
            "Ä←_∏←_ä"
            [ ("[123/1000]", All ["123/1000"])
            , ("[123/1000, 1/2]", All ["1/2"])
            , ("[0,0,1,1,0,1]", All ["1"])
            , ("[0,0,1,1,0,1,1/2]", All ["1/2"])
            , ("[3/4,3/4]", All ["3/8"])
            , ("[3/4,3/4,3/4]", All ["9/16"])
            ]
    describe "q262159: Minimum partition with non-empty intersections" $ do
        specEval
            "Oᵖᵐ{Ťđṁ<}aş"
            [ ("[]", All ["[]"])
            , ("[[0,1]]", All ["[[[0,1]]]"])
            , ("[[0,1],[2,3]]", All ["[[[2,3]],[[0,1]]]"])
            , ("[[1,2],[0,3]]", All ["[[[1,2],[0,3]]]"])
            , ("[[0,2],[1,3]]", All ["[[[0,2],[1,3]]]"])
            , ("[[1,2],[3,4],[0,5]]", All ["[[[3,4],[0,5]],[[1,2]]]", "[[[1,2],[0,5]],[[3,4]]]"])
            , ("[[0,1],[4,5],[2,3]]", All ["[[[2,3]],[[4,5]],[[0,1]]]"])
            , ("[[4,5],[1,2],[0,3]]", All ["[[[1,2],[0,3]],[[4,5]]]"])
            , ("[[0,1],[2,5],[3,4]]", All ["[[[2,5],[3,4]],[[0,1]]]"])
            , ("[[0,2],[3,5],[1,4]]", All ["[[[3,5],[1,4]],[[0,2]]]", "[[[0,2],[1,4]],[[3,5]]]"])
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
            "\"-_\"Ňŧĉᵗz'=ᵚcjt"
            [("", Truncated ["-=_", "_=-", "--=_", "-=_=-", "-=__", "_=--", "_=-=_", "__=-"])]
    describe "q262518: Landmine Number I" $ do
        specEval
            "ᵉpttᵋ+*:,,$Ĉ"
            [ ("[1,8,3,7,1] 4", All ["2"])
            , ("[2,9,2,8,0,6,4] 4", All ["4"])
            , ("[3,3,3,3,3] 9", All ["6"])
            , ("[9,9,9,9,9,9,9,9,9,9] 100", All ["0"])
            , ("[1,2,3,4,5,6,7,8,9,0] 8", All ["4"])
            , ("[2,2,2,2,2] 4", All ["9"])
            , ("[0,0,0,0,0,0,0,0,0,0,0,0,0,0] 0", All ["36"])
            , ("[1,2,9,5,1] 10", All ["4"])
            , ("[1,2,3,1,2,3,1,2,3,1,2,3] 3", All ["11"])
            , ("[8,2,8,8,2,8,8,2,8] 16", All ["11"])
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
            [ ("0", All ["0"])
            , ("1", All ["1"])
            , ("2", All ["6"])
            , ("3", All ["231"])
            , ("4", All ["1186570"])
            ]
    describe "q262809: Diagonalize a vector" $ do
        specEval
            "x:ᵒ-¬*"
            [ ("[]", All ["[]"])
            , ("[0]", All ["[[0]]"])
            , ("[1]", All ["[[1]]"])
            , ("[1,2,3]", All ["[[1,0,0],[0,2,0],[0,0,3]]"])
            , ("[1,0,2,3]", All ["[[1,0,0,0],[0,0,0,0],[0,0,2,0],[0,0,0,3]]"])
            ]
    describe "q262868: Chamber of Reflection" $ do
        specEval
            "Ðᵒ÷u#←"
            [ ("5 4 11", All ["4"])
            , ("1 1 10", All ["9"])
            , ("100 100 1", All ["0"])
            , ("3 2 9", All ["5"])
            , ("6 3 18", All ["5"])
            , ("1 1 100", All ["99"])
            , ("2398 2308 4", All ["0"])
            , ("500 10000 502", All ["1"])
            ]
    describe "q262985: Numbers that can be negated by reading backwards" $ do
        specEval
            "3Ňŧ←1c:↔_=3b"
            [("", Truncated ["2", "8", "20", "26", "32", "56", "80", "104", "146", "164"])]
    describe "q263200: Print all Polynomials" $ do
        specEval
            "Ňƒ$ƥ←ËƂ∙ŋ"
            [("", Truncated ["0", "[1]", "[-1]", "[0,1]", "[0,-1]", "[2]", "[-2]", "[0,0,1]", "[0,0,-1]"])]
    describe "q263308: Make a k-skip-j range" $ do
        specEval
            "+ᵚ%-±ç1Ĩ"
            [ ("1 1 11", All ["1", "3", "5", "7", "9", "11"])
            , ("2 13 19", All ["1", "2", "16", "17"])
            , ("2 13 16", All ["1", "2", "16"])
            , ("1 4 49", All ["1", "6", "11", "16", "21", "26", "31", "36", "41", "46"])
            , ("2 4 22", All ["1", "2", "7", "8", "13", "14", "19", "20"])
            , ("2 10 13", All ["1", "2", "13"])
            , ("5 15 10", All ["1", "2", "3", "4", "5"])
            , ("1 13 27", All ["1", "15"])
            , ("7 4 31", All ["1", "2", "3", "4", "5", "6", "7", "12", "13", "14", "15", "16", "17", "18", "23", "24", "25", "26", "27", "28", "29"])
            , ("99 99 1", All ["1"])
            ]
    describe "q263364: Compute this fractal matrix" $ do
        specEval
            "Ë:ᵒ{ᵃƂ+2Ĉ←A±"
            [ ("1", All ["[[1,1],[1,0]]"])
            , ("2", All ["[[1,1,1,1],[1,0,1,0],[1,1,0,0],[1,0,0,1]]"])
            , ("3", All ["[[1,1,1,1,1,1,1,1],[1,0,1,0,1,0,1,0],[1,1,0,0,1,1,0,0],[1,0,0,1,1,0,0,1],[1,1,1,1,0,0,0,0],[1,0,1,0,0,1,0,1],[1,1,0,0,0,0,1,1],[1,0,0,1,0,1,1,1]]"])
            ]
    describe "q263438: Is this a powerful number?" $ do
        specEval
            "ƒ1>"
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
            "~ᵉ{$#ᵑ{ˣ@j,u}}∕~"
            [ ("[[],[],[]]", Count 0)
            , ("[[1],[0]]", Count 2)
            , ("[[1],[0],[]]", Count 2)
            , ("[[1],[0,2],[]]", Count 3)
            , ("[[2],[],[1,0],[2]]", Count 5)
            , ("[[1,2,3],[2,3],[3],[]]", Count 0)
            , ("[[1],[2],[3],[4],[5],[0]]", Count 30)
            ]
        specEval
            "xᵒĈ:§#ᵑ{ˣᵐ∙+}±≈j∑"
            [ ("[[],[],[]]", All ["0"])
            , ("[[1],[0]]", All ["2"])
            , ("[[1],[0],[]]", All ["2"])
            , ("[[1],[0,2],[]]", All ["3"])
            , ("[[2],[],[1,0],[2]]", All ["5"])
            , ("[[1,2,3],[2,3],[3],[]]", All ["0"])
            , ("[[1],[2],[3],[4],[5],[0]]", All ["30"])
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
            [("", Truncated ["1", "4", "8", "16", "32", "64", "128", "144", "216", "288", "432", "864"])]
    describe "q263774: Round up to a smoother number" $ do
        specEval
            "Ň+ᵖ{ʷ½≥"
            [ ("101 100", First $ Just "102")
            , ("201 100", First $ Just "204")
            , ("256 100", First $ Just "256")
            ]
    describe "q263910: Evenly spread values" $ do
        specEval
            "kŢᵉR→/+j"
            [("[1/10,2/10,14/10,23/10,24/10,25/10,32/10]", All ["[1/3,2/3,3/2,9/4,5/2,11/4,7/2]"])]
    describe "q264102: Iterate over all non-equivalent strings" $ do
        specEval
            "r:,↕ũ∆Zç∫:ux=¿'a+H"
            [ ("2", All ["abab"])
            , ("3", All ["abcabc", "abcacb", "abcbac", "abcbca", "abacbc"])
            ]
    describe "q264166: Count N-Rich Permutations of an Integer Sequence" $ do
        specEval
            "↕∆±ĉᵐ∑çṀ→="
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
            "ᶦ{ĭ,ᵖ≠"
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
            [ ("\"A\"", All ["1"])
            , ("\"ABA\"", All ["1", "4", "9"])
            , ("\"DDDD\"", All ["1", "2", "3", "4"])
            , ("\"AABAA\"", All ["1", "2", "6", "12", "15"])
            , ("\"CABAC\"", All ["1", "4", "9", "16", "25"])
            , ("\"EBBBE\"", All ["1", "4", "6", "8", "15"])
            , ("\"DEBBBE\"", All ["1", "4", "9", "12", "15", "24"])
            , ("\"CCAABBA\"", All ["1", "2", "6", "8", "15", "18", "28"])
            ]
    describe "q264505: Repeat your program to print Fibonacci numbers" $ do
        specEval
            "ˣ+1I"
            [("", All ["1"])]
    describe "q264589: Multiply multivariate polynomials" $ do
        specEval
            "×"
            [ ("[1,1,1] [1,1,1]", All ["[1,2,3,2,1]"])
            , ("[[0,1],[1]] [[0,1],[1]]", All ["[[0,0,1],[0,2],[1]]"])
            , ("[[[0,1],[1]],[[1]]] [[[0,1],[-1]],[[1]]]", All ["[[[0,0,1],[0,0],[-1]],[[0,2],[0]],[[1]]]"])
            , ("[[],[[[[[[[[[1]]]]]]]]]] [[],[[[[[[[[[1]]]]]]]]]]", All ["[[],[],[[[[[[[[[1]]]]]]]]]]"])
            ]
    describe "q264624: Is it a canonical elementary CA rule number?" $ do
        specEval
            "¥+Ƃi:↔¬?:Jĭ,Ťđ,?ƃa≤"
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
            [ ("[0,0]", All ["[0,0]"])
            , ("[5,0]", All ["[5,0]"])
            , ("[0,2]", All ["[2,0]"])
            , ("[1,1]", All ["[1,1]"])
            , ("[1,-1]", All ["[1,1]"])
            , ("[3,-5]", All ["[5,3]"])
            , ("[-3,5]", All ["[5,3]"])
            , ("[3,5]", All ["[3,5]"])
            , ("[-3,-5]", All ["[3,5]"])
            ]
    describe "q264918: Sum of consecutive nth powers" $ do
        specEval
            "←ᶠ{ᵈqEů∑="
            [ ("1", All ["[]"])
            , ("2", All ["[]"])
            , ("3", All ["[]"])
            , ("4", All ["[2]"])
            , ("5", All ["[]"])
            , ("6", All ["[2]"])
            , ("7", All ["[2]"])
            , ("8", All ["[2]"])
            , ("9", All ["[3]"])
            , ("10", All ["[]"])
            ]
        specEval
            "←ᶠ{1>B:o±="
            [ ("1", All ["[]"])
            , ("2", All ["[]"])
            , ("3", All ["[]"])
            , ("4", All ["[2]"])
            , ("5", All ["[]"])
            , ("6", All ["[2]"])
            , ("7", All ["[2]"])
            , ("8", All ["[2]"])
            , ("9", All ["[3]"])
            , ("10", All ["[]"])
            ]
