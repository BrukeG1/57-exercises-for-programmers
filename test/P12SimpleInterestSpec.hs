module P12SimpleInterestSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P12SimpleInterest hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "computeInterest" $ do
    it "computes interest the same as http://www.webmath.com/_answer.php for some arbitrary numbers" $ do
      computeInterest 1000 10.3 14 `shouldBe` 1442.00
    it "computes interest the same as http://www.calculatorsoup.com/calculators/financial/simple-interest-plus-principal-calculator.php for some arbitrary numbers" $ do
      computeInterest 10000 3.875 5 `shouldBe` 1937.50

