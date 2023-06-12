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
            "[\":h,\"]:h,"
            [("", All ["[\":h,\"]:h,"])]
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
    describe "q12177: Collatz Conjecture (OEIS A006577)" $ do
        specEval
            "ˡ{1>ᵉ½3*→I"
            [ ("2", All ["1"])
            , ("16", All ["4"])
            , ("5", All ["5"])
            , ("7", All ["16"])
            ]
    describe "q42529: Mode (most common element) of a list" $ do
        specEval
            "ŢṂ"
            [("[4,3,1,0,6,1,6,4,4,0,3,1,7,7,3,4,1,1,2,8]", All ["1"])]
    describe "q50020: List Sophie Germain primes" $ do
        specEval
            "Ƥ←½Q"
            [("", Truncated ["2", "3", "5", "11", "23", "29", "41", "53", "83", "89"])]
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
            , ("\"glove\" \"dove\"", All [""])
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
    describe "q66127: Catalan Numbers" $ do
        specEval
            "+$Ç$→/"
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
            "+1Dŋ∫çƆž≥"
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
            "ᵐ{3r~←}∫Ɔž≥"
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
    describe "q70365: Construct the Identity Matrix" $ do
        specEval
            "ᵒ-¬"
            [ ("1", All ["[[1]]"])
            , ("2", All ["[[1,0],[0,1]]"])
            , ("3", All ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", All ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
            ]
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
            "Ň$ᵚ~"
            [("[\"a\",\"b\"]", Truncated ["[]", "[\"a\"]", "[\"b\"]", "[\"a\",\"a\"]", "[\"a\",\"b\"]", "[\"b\",\"a\"]", "[\"b\",\"b\"]"])]
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
    describe "q83377: Write a program to elasticize strings" $ do
        specEval
            "#Rᶻřjj"
            [ ("\"Why\"", All ["Whhyyy"])
            , ("\"SKype\"", All ["SKKyyyppppeeeee"])
            , ("\"LobbY\"", All ["LoobbbbbbbYYYYY"])
            , ("\"A and B\"", All ["A  aaannnnddddd      BBBBBBB"])
            ]
    describe "q83533: Calculate Euler's totient function" $ do
        specEval
            "RGŢh"
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
            "2%∫¬∑T"
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
            "Ne;<"
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
    describe "q94291: Is it a balanced number?" $ do
        specEval
            "¢D;ᶜtᶻ-∑ž"
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
    describe "q119854: Raise integer x to power x, without exponentiation built-ins" $ do
        specEval
            "ř∏"
            [ ("2", All ["4"])
            , ("3", All ["27"])
            , ("5", All ["3125"])
            , ("6", All ["46656"])
            , ("10", All ["10000000000"])
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
    describe "q125104: Cartesian product of two lists" $ do
        specEval
            "ᵐ~"
            [ ("[\"abc\",\"123\"]", All ["[\"a\",\"1\"]", "[\"a\",\"2\"]", "[\"a\",\"3\"]", "[\"b\",\"1\"]", "[\"b\",\"2\"]", "[\"b\",\"3\"]", "[\"c\",\"1\"]", "[\"c\",\"2\"]", "[\"c\",\"3\"]"])
            , ("[\"aa\",\"aba\"]", All ["[\"a\",\"a\"]", "[\"a\",\"b\"]", "[\"a\",\"a\"]", "[\"a\",\"a\"]", "[\"a\",\"b\"]", "[\"a\",\"a\"]"])
            ]
    describe "q126373: Am I a Fibonacci Number?" $ do
        specEval
            "*5*4ŋ-A√"
            [ ("0", Check True)
            , ("1", Check True)
            , ("2", Check True)
            , ("12", Check False)
            ]
    describe "q126699: Create a checkerboard matrix" $ do
        specEval
            "ᵒ+→2%"
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
            "eµkH"
            [ ("[\"A\",\"C\"]", All ["B"])
            , ("[\"a\",\"z\"]", All ["m"])
            , ("[\"d\",\"j\"]", All ["g"])
            , ("[\"B\",\"e\"]", All ["S"])
            , ("[\"Z\",\"a\"]", All ["]"])
            ]
    describe "q199290: Reversed Iota's" $ do
        specEval
            "RRᵐ↔"
            [("4", All ["[[1],[2,1],[3,2,1],[4,3,2,1]]"])]
        specEval
            "RpN↔"
            [("4", All ["[1]", "[2,1]", "[3,2,1]", "[4,3,2,1]"])]
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
            "oĉ~z"
            [ ("[1,1,1,2,1,1]", All ["2"])
            , ("[3,5,5,5,5]", All ["3"])
            , ("[9,2,9,9,9,9,9]", All ["2"])
            , ("[4,4,4,6]", All ["6"])
            , ("[5,8,8]", All ["5"])
            , ("[8,5,8]", All ["5"])
            , ("[8,8,5]", All ["5"])
            ]
        specEval
            "ĕ$≡¿"
            [ ("[1,1,1,2,1,1]", All ["2"])
            , ("[3,5,5,5,5]", All ["3"])
            , ("[9,2,9,9,9,9,9]", All ["2"])
            , ("[4,4,4,6]", All ["6"])
            , ("[5,8,8]", All ["5"])
            , ("[8,5,8]", All ["5"])
            , ("[8,8,5]", All ["5"])
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
    describe "q225203: Delannoy numbers" $ do
        specEval
            "Ṁ→ᵒÇ∏2d"
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
            "ʷ{į1?-0≥"
            [ ("[3,3]", Count 63)
            , ("[3,9]", Count 1159)
            , ("[1,7]", Count 15)
            , ("[7,0]", Count 1)
            , ("[0,4]", Count 1)
            ]
    describe "q252927: Make a Court Transcriber" $ do
        specEval
            "ᶠ{JS=}ş"
            [ ("[\"dictionary\",\"transcriber\"] [\"dic\",\"ion\",\"ary\"]", All ["dictionary"])
            , ("[\"dictionary\",\"transcriber\"] [\"tra\",\"scr\",\"ber\"]", All ["transcriber"])
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
            "RO3L"
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
            "o↔ĭ∑½"
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
            "¢DsCr↔~c¢b-"
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
            "¢Bx¢E$ᶻřj↔∫"
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
    describe "q248991: The Unaverageables" $ do
        specEval
            "ᶠ{+2/∩z"
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
            "rO2ᵚL"
            [ ("2", All ["[[0,1]]"])
            , ("4", All ["[[2,3],[0,1]]", "[[1,3],[0,2]]", "[[0,3],[1,2]]"])
            ]
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
            "Rᶠ{$R~ᵃƂ×ᵈƂ-½"
            [ ("2", All ["[1,2]"])
            , ("4", All ["[1,2,4]"])
            , ("5", All ["[1,3,5]"])
            , ("6", All ["[1,2,3,6]"])
            , ("25", All ["[1,25]"])
            , ("39", All ["[1,3,5,11,29,39]"])
            , ("42", All ["[1,2,7,14,21,42]"])
            , ("100", All ["[1,2,4,25,50,100]"])
            ]
    describe "q255650: Sum every second digit in a number" $ do
        specEval
            "¢Dĭ∑"
            [ ("10", All ["0"])
            , ("101011", All ["1"])
            , ("548915381", All ["26"])
            , ("999999", All ["27"])
            , ("2147483647", All ["29"])
            , ("999999999", All ["36"])
            ]
    describe "q256017: CGAC2022 Day 25: When The Planets Align" $ do
        specEval
            "Ňᵖ{*+$/1%≡"
            [ ("[0,0] [0,0] [1,1]", First $ Just "0")
            , ("[1,0] [1,0] [100,100]", First $ Just "99")
            , ("[1,5,3] [0,1,0] [4,8,12]", First $ Just "5")
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
            "eᵐ{į→ŋ+}≡H"
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
            "↕ᵃ{J¢ᵚb\"∩<\"e<60b}-_Paṁ"
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
            "ƂxŘ2dṁ"
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
            "qCᵉLR↔<a*$h→L"
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
    describe "q258299: Primes with Distinct Prime Digits" $ do
        specEval
            "Ƥ¢BQ:u=¢d"
            [("", Truncated ["2", "3", "5", "7", "23", "37", "53", "73", "257", "523", "2357", "2753", "3257", "3527", "5237", "5273", "7253", "7523"])]
        specEval
            "¢rSQ↕¢bQao"
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
            "qe£E→∫x>çƆᵖLaṀ"
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
            "R↔$∆çJᵐ{CᵈAc}-0≥"
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
            "RO↕ᶠz$L"
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
            "į1:ÐÐ3r~ᵑᵐç3r~ᵑçaᵐᶜ{0*}∑2<"
            [ ("", Count 215)
            , ("", Truncated ["[[0,1,0,1],[1,1,1,1],[0,1,0,1],[1,1,1,1]]", "[[0,1,0,1],[1,1,1,1],[0,1,1,0],[1,1,1,1]]"])
            ]
    describe "q259633: Make a Custom Bayer Matrix" $ do
        specEval
            "ᵒ{ᵃƂᵈ:-A2*+ç4ŗd"
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
            "e¥b¥Dx:ᵃ~→ᵖ{r+@Ť:u=}ç$+"
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
            "↔ᵃj<"
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
            "r↕∆A1>"
            [ ("1", Count 1)
            , ("2", Count 0)
            , ("3", Count 0)
            , ("4", Count 2)
            , ("5", Count 14)
            , ("6", Count 90)
            ]
    describe "q260302: Is it a plausible chess move?" $ do
        specEval
            "-AZ‼2M≡"
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
            "Ňᵖᵚ{ᵑ{įŋ+}f"
            [ ("[]", First $ Just "0")
            , ("[[1,1]]", First $ Just "1")
            , ("[[1,2],[2,1],[2,2],[2,3],[3,2]]", First $ Just "2")
            ]
    describe "q260472: Find Index of Rational Number in Calkin-Wilf Sequence" $ do
        specEval
            "ˡ{ŗ:K2*$-←"
            [ ("1", All ["1"])
            , ("1/3", All ["4"])
            , ("4/3", All ["9"])
            , ("3/4", All ["14"])
            , ("53/37", All ["1081"])
            , ("37/53", All ["1990"])
            ]
    describe "q260804: Minkowski's ?(x) for rational x" $ do
        specEval
            "ᶦ{1%ŗ}kaC$∫←:_2E£d§+ṇ$çlÐ"
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
            "Ň2E"
            [ ("", Truncated ["1", "2", "4", "8", "16", "32", "64", "128", "256", "512", "1024"])
            ]
