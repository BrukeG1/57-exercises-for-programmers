module P31KarvonenHRSpec (main,spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import P31KarvonenHR hiding (main)
 
 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "targetHR" $ do
    it "calculates the book example at 55% correctly" $ do
      targetHR 22 65 0.55 `shouldBe` 138
    it "calculates the book example at 60% correctly" $ do
      targetHR 22 65 0.6 `shouldBe` 145
    it "calculates the book example at 65% correctly" $ do
      targetHR 22 65 0.65 `shouldBe` 151
    it "calculates the book example at 85% correctly" $ do
      targetHR 22 65 0.85 `shouldBe` 178
    it "calculates the book example at 90% correctly" $ do
      targetHR 22 65 0.9 `shouldBe` 185
    it "calculates the book example at 95% correctly" $ do
      targetHR 22 65 0.95 `shouldBe` 191
