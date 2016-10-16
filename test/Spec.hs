module Main (main) where

import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.Runners.AntXML

main :: IO ()
main = do
  theSpecs <- testSpec "Specs" specs
  defaultMainWithIngredients [antXMLRunner] theSpecs

specs :: Spec
specs = do
  describe "A test" $ do
    it "passes" $ (1::Int) `shouldBe` 1
