{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eval (testEval) where

import Control.Monad
import Data.Either (isRight)
import Data.Maybe (fromMaybe)
import Nekomata.Eval
import Test.Hspec

data Result
    = All [String]
    | Truncated [String]
    | First (Maybe String)
    | Count Integer
    | Check Bool
    deriving (Eq)

instance Show Result where
    show (All xs) = unwords xs
    show (Truncated xs) = unwords xs ++ " ..."
    show (First x) = fromMaybe "None" x
    show (Count n) = show n
    show (Check b) = show b

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
    describe "q55422: \"Hello, World!\"" $ do
        specEval
            "\"Hello, World!\""
            [("", All ["Hello, World!"])]
    describe "q66127: Catalan Numbers" $ do
        specEval
            "2*$Ç$→/"
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
    describe "q70365: Construct the Identity Matrix" $ do
        specEval
            "ᵒ-¬"
            [ ("1", All ["[[1]]"])
            , ("2", All ["[[1,0],[0,1]]"])
            , ("3", All ["[[1,0,0],[0,1,0],[0,0,1]]"])
            , ("4", All ["[[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]"])
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
            "0c-P‼∑"
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
            "0cM-_∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", All ["9"])
            , ("[5,8]", All ["8"])
            , ("[1,1,1,1]", All ["1"])
            , ("[]", All ["0"])
            , ("[0,0]", All ["0"])
            , ("[2]", All ["2"])
            , ("[2,0,2]", All ["4"])
            , ("[10,9,8,9]", All ["11"])
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
            "ᵉᵑ{ˣ{s∙}ɔ}T"
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
            , ("[[[1,2],[2,4],[4,8]],[[-4,-4],[-1,1],[2,-2]]] ", All ["-9"])
            ]
    describe "q247676: Generate All 8 Knight's Moves" $ do
        specEval
            "2R↕ᵐᶜ_"
            [("", All ["[1,2]", "[1,-2]", "[-1,2]", "[-1,-2]", "[2,1]", "[2,-1]", "[-2,1]", "[-2,-1]"])]
    describe "q252082: Reconstruct Matrix from its diagonals" $ do
        specEval
            "#2÷:→:ᵒ{ˣmᵈ{-+@}@"
            [ ("[[5]]", All ["[[5]]"])
            , ("[[0],[1,69],[13]]", All ["[[1,0],[13,69]]"])
            , ("[[25],[0,1],[6,23,10],[420,9],[67]]", All ["[[6,0,25],[420,23,1],[67,9,10]]"])
            ]
    describe "q252927: Make a Court Transcriber" $ do
        specEval
            "~ᵖ{JS="
            [ ("[\"dictionary\",\"transcriber\"] [\"dic\",\"ion\",\"ary\"]", First $ Just "dictionary")
            , ("[\"dictionary\",\"transcriber\"] [\"tra\",\"scr\",\"ber\"]", First $ Just "transcriber")
            ]
    describe "q257631: Time to shortest permutation" $ do
        specEval
            "↕ᵃ{¢b100B\"<∩\"e<60d}-_Paṁ"
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
            [ ("[3, 4, 6] 10", All ["24"])
            , ("[2] 14", All ["28"])
            , ("[6] 30", All ["36"])
            , ("[4, 3], 20", All ["40"])
            , ("[5, 8, 7], 9", All ["15"])
            , ("[2, 9, 4, 8], 7", All ["24"])
            ]
    describe "q257752: Print all pandigital numbers" $ do
        specEval
            "Ňᵖ{*$Bu$L"
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
            "ŇPᵖ{-Z"
            [ ("[1,2,3]", First (Just "4"))
            , ("[3,4,-1,1]", First (Just "2"))
            , ("[7,8,9,11,12]", First (Just "1"))
            , ("[-5,-4,-3,-2,-1,0,1,2,3,5,7,10]", First (Just "4"))
            , ("[]", First (Just "1"))
            , ("[-1,-4,-7]", First (Just "1"))
            ]
    describe "q258432: Shortest code to generate all Pythagorean triples up to a given limit" $ do
        specEval
            "RS3Lᵖ{:*Ɔ$∑="
            [ ("20", All ["[3,4,5]", "[6,8,10]", "[5,12,13]", "[9,12,15]", "[8,15,17]", "[12,16,20]"])
            , ("5", All ["[3,4,5]"])
            ]
    describe "q258511: Longest Valid Parentheses" $ do
        specEval
            "qe7%3%∫:l0?R≥¤#aṀ"
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
            , ("[101,103,101,105] ", All ["101", "101", "101", "105"])
            ]
