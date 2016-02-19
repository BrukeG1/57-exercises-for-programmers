module LibrarySpec (main,spec) where

import Test.Hspec
import Library

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "showD" $ do
    it "rounds correctly for whole number floats" $
      showD 9.0 `shouldBe` "9.00"
    it "rounds correctly for non-whole number floats" $
      showD 9.00012 `shouldBe` "9.00"
    it "rounds correctly for .5" $
      showD 9.5 `shouldBe` "9.50"
    it "rounds correctly for .05" $
      showD 9.05 `shouldBe` "9.05"
    it "rounds correctly for .005" $
      showD 9.005 `shouldBe` "9.00"
    it "rounds correctly for .006" $
      showD 9.006 `shouldBe` "9.01"
  describe "showD8" $ do
    it "rounds and formats correctly for whole number floats" $
      showD8 9.0 `shouldBe` "    9.00"
    it "rounds and formats correctly for non-whole number floats" $
      showD8 9.00012 `shouldBe` "    9.00"
    it "rounds and formats correctly for .5" $
      showD8 9.5 `shouldBe` "    9.50"
    it "rounds and formats correctly for .05" $
      showD8 9.05 `shouldBe` "    9.05"
    it "rounds and formats correctly for .005" $
      showD8 9.005 `shouldBe` "    9.00"
    it "rounds correctly for .006" $
      showD8 9.006 `shouldBe` "    9.01"
  describe "uc" $ do
    it "gives back an empty string if given an empty string" $
      uc "" `shouldBe` ""
    it "gives back a capitalized string if given a lowercase string" $
      uc "eek" `shouldBe` "EEK"
    it "gives back a capitalized string if given a mixed case string" $
      uc "eEk" `shouldBe` "EEK"
    it "gives back a capitalized string if given an uppercase string" $
      uc "EEK" `shouldBe` "EEK"
  describe "lc" $ do
    it "gives back an empty string if given an empty string" $
      lc "" `shouldBe` ""
    it "gives back a lowercase string if given a lowercase string" $
      lc "eek" `shouldBe` "eek"
    it "gives back a lowercase string if given a mixed case string" $
      lc "eEk" `shouldBe` "eek"
    it "gives back a lowercase string if given an uppercase string" $
      lc "EEK" `shouldBe` "eek"
