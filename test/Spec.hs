module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Ingredients.Basic
import Test.Tasty.Runners.AntXML

main :: IO ()
main = do
  theSpecs <- testSpec "Specs" specs
  defaultMainWithIngredients (antXMLRunner:defaultIngredients) theSpecs

specs :: Spec
specs = do
  describe "A test" $ do
    it "passes" $ (1::Int) `shouldBe` 1
    it "fails - or does it?" $ (1::Int) `shouldBe` 0
