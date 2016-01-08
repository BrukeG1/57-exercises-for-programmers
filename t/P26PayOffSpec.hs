module P26PayOffSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P26PayOff hiding (main)
import Data.List (sort)
import Control.Exception (evaluate)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "monthsToPayOff" $ do
    it "works for book example" $ do
      monthsToPayOff 12 5000 100 `shouldBe` 70
    it "works for another example" $ do
      monthsToPayOff 7.8 5000 100 `shouldBe` 61
    it "fails if payment would take forever" $ do
      evaluate(monthsToPayOff 22 7500 125) `shouldThrow` anyException
