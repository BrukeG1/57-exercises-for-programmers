module P31KarvonenHRSpec (main,spec) where

import Test.Hspec
import P31KarvonenHR hiding (main)
 
 
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "targetHR" $ do
    it "calculates the book example at 55% correctly" $ do
      targetHR 22 65 55 `shouldBe` 138
    it "calculates the book example at 60% correctly" $ do
      targetHR 22 65 60 `shouldBe` 145
    it "calculates the book example at 65% correctly" $ do
      targetHR 22 65 65 `shouldBe` 151
    it "calculates the book example at 85% correctly" $ do
      targetHR 22 65 85 `shouldBe` 178
    it "calculates the book example at 90% correctly" $ do
      targetHR 22 65 90 `shouldBe` 185
    it "calculates the book example at 95% correctly" $ do
      targetHR 22 65 95 `shouldBe` 191
