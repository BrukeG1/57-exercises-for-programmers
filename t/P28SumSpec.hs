module P28SumSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P28Sum hiding (main)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "sum'" $ do
    it "returns zero when called with an empty list" $ do
      sum' [] `shouldBe` 0
    it "correctly sums a single element list" $ do
      sum' [7] `shouldBe` 7
    it "correctly sums a 2 elt list" $ do
      sum' [1,4] `shouldBe` 5
    prop "gives the same result as the sum function from the prelude" $
      \xs -> sum' (xs::[Int]) == sum xs
  describe "promptForN" $ do
    it "returns empty list when called with 0" $ do
      n <- promptForN 0
      n `shouldBe` []
    -- todo: how to test IO?
