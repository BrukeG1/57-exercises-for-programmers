module P17BloodAlcoholSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P17BloodAlcohol hiding (main)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isLegal" $ do
    prop "has the property of always returning the same as < 0.08" $ 
      \x -> isLegal x `shouldBe` (x<0.08)
    it "returns true for < legal limit" $
      isLegal 0.07 `shouldBe` True
    it "returns false for > legal limit" $
      isLegal 0.08001 `shouldBe` False
  describe "bac" $ do
    it "gives expected result for some arbitrary figures" $
      bac 12 150 1 Female `shouldBe` 0.12139199999999997
