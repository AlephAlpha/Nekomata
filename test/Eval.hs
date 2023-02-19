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
    describe "q55422: \"Hello, World!\"" $ do
        specEval
            Nothing
            "\"Hello, World!\""
            [("", ["\"Hello, World!\""])]
    describe "q175248: The inverse Collatz Conjecture" $ do
        specEval
            Nothing
            "ʳ{Z:←2∣$3*→?∃"
            [ ("0", ["0"])
            , ("1", ["1", "0"])
            , ("2", ["2", "7", "3", "1", "0"])
            , ("3", ["3", "1", "0"])
            , ("10", ["10", "31", "15", "7", "3", "1", "0"])
            , ("14", ["14", "43", "21", "10", "31", "15", "7", "3", "1", "0"])
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
