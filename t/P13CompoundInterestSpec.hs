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
