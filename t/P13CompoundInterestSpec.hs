module P13CompoundInterestSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P13CompoundInterest hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "computeCompoundInterest" $ do
    it "computes interest the same as http://www.moneychimp.com/calculator/compound_interest_calculator.htm for some arbitrary numbers" $ do
      computeCompoundInterest 1200 4 5 4 `shouldBe` 264.23
    it "computes interest the same as  for some arbitrary numbers" $ do
      computeCompoundInterest 10000 1.5 5 12 `shouldBe` 778.34
  describe "showD" $ do
    it "rounds correctly for whole number floats" $ do
      showD 9.0 `shouldBe` "9.00"
    it "rounds correctly for non-whole number floats" $ do
      showD 9.00012 `shouldBe` "9.00"
    it "rounds correctly for .5" $ do
      showD 9.5 `shouldBe` "9.50"
    it "rounds correctly for .05" $ do
      showD 9.05 `shouldBe` "9.05"
    it "rounds correctly for .005" $ do
      showD 9.005 `shouldBe` "9.00"
    it "rounds correctly for .006" $ do
      showD 9.006 `shouldBe` "9.01"
