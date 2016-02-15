module P14TaxCalcSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P14TaxCalc hiding (main)

main :: IO ()
main = hspec spec

spec = do
  describe "wisconsinTax" $ do
    it "calculates and formats wisoconsin tax on $100 correctly" $ do
      wisconsinTax 100 `shouldBe` "Tax:                 5.50\nTotal:             105.50"
    it "calculates and formats wisoconsin tax on $200 correctly" $ do
      wisconsinTax 200 `shouldBe` "Tax:                11.00\nTotal:             211.00"

