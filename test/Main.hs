module Main (main) where

import Data.List
import Nekomata.Builtin
import Nekomata.CodePage
import Nekomata.Particle hiding (name, short)
import qualified Nekomata.Particle as Particle
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "CodePage" $ do
        it "should be of length 256" $ do
            length codePage `shouldBe` 256

        it "should contain only unique characters except '�'" $ do
            let codePage' = filter (/= '�') codePage
            length codePage' `shouldBe` length (nub codePage')

        it "should contain all printable ASCII characters" $ do
            let ascii = [' ' .. '~']
            take 95 (drop 32 codePage) `shouldBe` ascii

    describe "Builtin" $ do
        it "should have unique names" $ do
            let names = map name builtins
            length names `shouldBe` length (nub names)

        it "should have unique short names" $ do
            let shortNames = map short builtins
            length shortNames `shouldBe` length (nub shortNames)

        it "should have short names that are in the code page" $ do
            let shortNames = map short builtins
            let codePage' = filter (`notElem` " \\\"\n[]0123456789�") codePage
            all (`elem` codePage') shortNames `shouldBe` True

    describe "Particle" $ do
        it "should have unique names" $ do
            let names = map Particle.name builtinParticles
            length names `shouldBe` length (nub names)

        it "should have unique short names" $ do
            let shortNames = map Particle.short builtinParticles
            length shortNames `shouldBe` length (nub shortNames)

        it "should have short names that are in the code page" $ do
            let shortNames = map Particle.short builtinParticles
            let codePage' = filter (`notElem` " \\\"\n[]0123456789�") codePage
            all (`elem` codePage') shortNames `shouldBe` True

        it "should have different names than builtin functions" $ do
            let names = map Particle.name builtinParticles
            let builtins' = map name builtins
            all (`notElem` builtins') names `shouldBe` True

        it "should have different short names than builtin functions" $ do
            let shortNames = map Particle.short builtinParticles
            let builtins' = map short builtins
            all (`notElem` builtins') shortNames `shouldBe` True
