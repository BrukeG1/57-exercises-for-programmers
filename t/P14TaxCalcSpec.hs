module P14TaxCalcSpec (main,spec) where

import Test.Hspec
import P14TaxCalc hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "wisconsinTax" $ do
    it "calculates and formats wisoconsin tax on $100 correctly" $
      wisconsinTax 100 `shouldBe` "Tax:                 5.50\nTotal:             105.50"
    it "calculates and formats wisoconsin tax on $200 correctly" $
      wisconsinTax 200 `shouldBe` "Tax:                11.00\nTotal:             211.00"

