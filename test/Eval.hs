{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Eval (testEval) where

import Control.Monad
import Data.Either (isRight)
import Nekomata.Eval
import Test.Hspec

specEval :: Maybe Int -> String -> [(String, [String])] -> Spec
specEval limit code testCases = context code $ do
    it "should compile" $ do
        code `shouldSatisfy` isRight . compile
    it ("should be " ++ show (length code) ++ " bytes") $ do
        length code `shouldBe` length code
    let Right f = compile code
    forM_ testCases $ \(input, output) -> do
        it (show input ++ " -> " ++ unwords (map show output)) $ do
            let Right input' = readInput input
            let runtime = initRuntime input'
            let results = allResults . snd $ runFunction f runtime
            case limit of
                Nothing -> results `shouldBe` output
                Just n -> take n results `shouldBe` take n output

testEval :: Spec
testEval = describe "Evaluation" $ do
    describe "q85: Fibonacci function or sequence" $ do
        specEval
            (Just 10)
            "1:ⁿ{$ᵉ+"
            [("", ["1", "1", "2", "3", "5", "8", "13", "21", "34", "55"])]
        specEval
            Nothing
            "ʷ{←P:←?}n"
            [ ("0", ["1"])
            , ("1", ["1"])
            , ("2", ["2"])
            , ("3", ["3"])
            , ("4", ["5"])
            , ("5", ["8"])
            , ("6", ["13"])
            , ("7", ["21"])
            , ("8", ["34"])
            , ("9", ["55"])
            ]
    describe "q55422: \"Hello, World!\"" $ do
        specEval
            Nothing
            "\"Hello, World!\""
            [("", ["Hello, World!"])]
    describe "q141949: Count edits accounting for grace period" $ do
        specEval
            Nothing
            "ⁿ{C4+>N}n"
            [ ("[0]", ["1"])
            , ("[0,3,5,7]", ["2"])
            , ("[0,3,4,7,9,10,11,12]", ["3"])
            , ("[0,30,120]", ["3"])
            , ("[0,4,8,12,16]", ["3"])
            , ("[0,4,8,12,16,20]", ["3"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]", ["4"])
            , ("[0,5,10,15,20]", ["5"])
            , ("[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]", ["5"])
            , ("[0,1,4,5,9,11,12,14,16,18,23,24,26,28,29,30]", ["6"])
            ]
    describe "q175248: The inverse Collatz Conjecture" $ do
        specEval
            Nothing
            "ⁿ{Z:←2∣$3*→?∃"
            [ ("0", ["0"])
            , ("1", ["1", "0"])
            , ("2", ["2", "7", "3", "1", "0"])
            , ("3", ["3", "1", "0"])
            , ("10", ["10", "31", "15", "7", "3", "1", "0"])
            , ("14", ["14", "43", "21", "10", "31", "15", "7", "3", "1", "0"])
            ]
    describe "q179464: Covering a Skyline with brush strokes" $ do
        specEval
            Nothing
            "0c-P∑"
            [ ("[1,3,2,1,2,1,5,3,3,4,2]", ["9"])
            , ("[5,8]", ["8"])
            , ("[1,1,1,1]", ["1"])
            , ("[]", ["0"])
            , ("[0,0]", ["0"])
            , ("[2]", ["2"])
            , ("[2,0,2]", ["4"])
            , ("[10,9,8,9]", ["11"])
            ]
    describe "q199290: Reversed Iota's" $ do
        specEval
            Nothing
            "RRᵐ↔"
            [("4", ["[[1],[2,1],[3,2,1],[4,3,2,1]]"])]
        specEval
            Nothing
            "RpN↔"
            [("4", ["[1]", "[2,1]", "[3,2,1]", "[4,3,2,1]"])]
    describe "q257649: Arbitrary Apple Dilemma" $ do
        specEval
            Nothing
            "∏*$←∏÷"
            [ ("[3, 4, 6] 10", ["24"])
            , ("[2] 14", ["28"])
            , ("[6] 30", ["36"])
            , ("[4, 3], 20", ["40"])
            , ("[5, 8, 7], 9", ["15"])
            , ("[2, 9, 4, 8], 7", ["24"])
            ]
