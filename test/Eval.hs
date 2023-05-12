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
    describe "q50020: List Sophie Germain primes" $ do
        specEval
            "Ƥ←2¦Q"
            [("", Truncated ["2", "3", "5", "11", "23", "29", "41", "53", "83", "89"])]
    describe "q55422: \"Hello, World!\"" $ do
        specEval
            "\"Hello, World!\""
            [("", All ["Hello, World!"])]
    describe "q62732: Implement a Truth-Machine" $ do
        specEval
            "ᶦP"
            [ ("0", All ["0"])
            , ("1", Truncated ["1", "1", "1", "1", "1", "1", "1", "1", "1", "1"])
            ]
    describe "q63999: Parenthifiable Binary Numbers" $ do
        specEval
            "2D£E∫Ɔ0=≤"
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
            "+Ø$ᵑ{ᵉçt?}Ø="
            [ ("0", Count 1)
            , ("1", Count 1)
            , ("2", Count 2)
            , ("3", Count 5)
            , ("4", Count 14)
            , ("5", Count 42)
            , ("6", Count 132)
            , ("7", Count 429)
            , ("8", Count 1430)
            , ("9", Count 4862)
            ]
    describe "q70365: Construct the Identity Matrix" $ do
        specEval
            "ᵒ-¬"
            [ ("1", All ["[[1]]"])
            , ("2", All ["[[1,0],[0,1]]"])
            , ("3", All ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", All ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
            ]
    describe "q79483: Continued Fraction of a Rational Number" $ do
        specEval
            "ᶦ{1%ŗ}1÷"
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
            "RG←¬∑"
            [ ("1", All ["1"])
            , ("2", All ["1"])
            , ("3", All ["2"])
            , ("8", All ["4"])
            , ("9", All ["6"])
            , ("26", All ["12"])
            , ("44", All ["20"])
            , ("105", All ["48"])
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
    describe "q94291: Is it a balanced number?" $ do
        specEval
            "¢D;ᶜtᶻ-∑0="
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
    describe "q103756: Big numbers: Ultrafactorials" $ do
        specEval
            "→rF:E∑"
            [ ("0", All ["1"])
            , ("1", All ["2"])
            , ("2", All ["6"])
            , ("3", All ["46662"])
            ]
    describe "q105861: Can this number be written in (3^x) - 1 format?" $ do
        specEval
            "3D←P∑"
            [ ("2", All ["1"])
            , ("26", All ["3"])
            , ("1024", All [])
            ]
    describe "q120350: Determine if an Array contains something other than 2" $ do
        specEval
            "2-¬/"
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
            "ᶦ{C4+>‼N"
            [ ("[0]", Count 1)
            , ("[0,3,5,7]", Count 2)
            , ("[0,3,4,7,9,10,11,12]", Count 3)
            , ("[0,30,120]", Count 3)
            , ("[0,4,8,12,16]", Count 3)
            , ("[0,4,8,12,16,20]", Count 3)
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]", Count 4)
            , ("[0,5,10,15,20]", Count 5)
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]", Count 5)
            , ("[0,1,4,5,9,11,12,14,16,18,23,24,26,28,29,30]", Count 6)
            ]
    describe "q142534: Is it a completely even number?" $ do
        specEval
            "←2D≡"
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
            "ᶦ{Z:←2¦$3*→I"
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
    describe "q199290: Reversed Iota's" $ do
        specEval
            "RRᵐ↔"
            [("4", All ["[[1],[2,1],[3,2,1],[4,3,2,1]]"])]
        specEval
            "RpN↔"
            [("4", All ["[1]", "[2,1]", "[3,2,1]", "[4,3,2,1]"])]
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
            "o↔x2%∙2/"
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
            "2R↕ᵐᶜ_"
            [("", All ["[1,2]", "[1,-2]", "[-1,2]", "[-1,-2]", "[2,1]", "[2,-1]", "[-2,1]", "[-2,-1]"])]
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
            "#2÷:→:ᵒ{ˣmᵈ{-+@}@"
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
    describe "q252927: Make a Court Transcriber" $ do
        specEval
            "~ᵖ{JS="
            [ ("[\"dictionary\",\"transcriber\"] [\"dic\",\"ion\",\"ary\"]", First $ Just "dictionary")
            , ("[\"dictionary\",\"transcriber\"] [\"tra\",\"scr\",\"ber\"]", First $ Just "transcriber")
            ]
    describe "q256814: Knight to fork!" $ do
        specEval
            "eᵐ{2R↕ᵐᶜ_+}≡H"
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
            [ ("[1,2,3]", First (Just "4"))
            , ("[3,4,-1,1]", First (Just "2"))
            , ("[7,8,9,11,12]", First (Just "1"))
            , ("[-5,-4,-3,-2,-1,0,1,2,3,5,7,10]", First (Just "4"))
            , ("[]", First (Just "1"))
            , ("[-1,-4,-7]", First (Just "1"))
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
            "SjoJŤđ=az"
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
            "∑2¦$ṁ±*$:#←c≥"
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
            "ʷ{P↕1:Ð-:C0=+?}0U="
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
            "RO↕ᵐz‼$L"
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
            "2r↕1:ÐÐ3r~ᵑᵐç3r~ᵑçaᵐᶜ{0*}∑2<"
            [ ("", Count 215)
            , ("", Truncated ["[[0,1,0,1],[1,1,1,1],[0,1,0,1],[1,1,1,1]]", "[[0,1,0,1],[1,1,1,1],[0,1,1,0],[1,1,1,1]]"])
            ]
    describe "q259633: Make a Custom Bayer Matrix" $ do
        specEval
            "r2B:ᵒ{ᵈ:-A2*+ç4ŗd"
            [ ("1", All ["[[0]]"])
            , ("2", All ["[[0,1/2],[3/4,1/4]]"])
            , ("4", All ["[[0,1/2,1/8,5/8],[3/4,1/4,7/8,3/8],[3/16,11/16,1/16,9/16],[15/16,7/16,13/16,5/16]]"])
            ]
        specEval
            "0UU$ᵑ{4*::3+,$→:→$,ᶻ,"
            [ ("0", All ["[[0]]"])
            , ("1", All ["[[0,2],[3,1]]"])
            , ("2", All ["[[0,8,2,10],[12,4,14,6],[3,11,1,9],[15,7,13,5]]"])
            ]
    describe "q259707: Shortest distinguishable slice" $ do
        specEval
            "e¥b¥Dx:ᵃ~→ᵖ{r+@Ť:u=}ç$+"
            [ ("[\"happy\",\"angry\",\"hungry\"]", First (Just "[1,2]"))
            , ("[\"sheer\",\"shrew\",\"shine\",\"shire\",\"spike\",\"shy\"]", First (Just "[2,4]"))
            , ("[\"snap\",\"crackle\",\"pop\",\"smack\",\"sizzle\",\"whiff\",\"sheen\"]", First (Just "[0,2]"))
            ]
    describe "q259875: How Super is this Prime?" $ do
        specEval
            "ᶦ{ƥQ}n2B#←"
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
            "$,ᵈ,>"
            [ ("\"a\" \"b\"", Check True)
            , ("\"ac\" \"a\"", Check False)
            , ("\"bekcka\" \"kwnfoe\"", Check True)
            , ("\"beztbest\" \"bestbe\"", Check False)
            , ("\"mcjaf\" \"mc\"", Check True)
            , ("\"akboe\" \"uenvi\"", Check True)
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
            "$ᵚpj$Lũ"
            [ ("8 \"aaaaa\"", Count 1)
            , ("4 \"abcde\"", Count 8)
            , ("5 \"abcdef\"", Count 16)
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
    describe "q260472: Find Index of Rational Number in Calkin-Wilf Sequence" $ do
        specEval
            "ᶦ{ŗ:£%2*-←Z"
            [ ("1", Count 1)
            , ("1/3", Count 4)
            , ("4/3", Count 9)
            , ("3/4", Count 14)
            , ("53/37", Count 1081)
            , ("37/53", Count 1990)
            ]
    describe "q260804: Minkowski's ?(x) for rational x" $ do
        specEval
            "ᶦ{1%ŗ}1÷aC$∫←ᵉ{_2E£d+ṇ}çlÐ"
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
